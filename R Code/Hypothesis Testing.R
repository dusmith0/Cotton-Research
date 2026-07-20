## Initial Data Sourcing
# Working with Lubbock Data to being
# 
# Comment Conventions
# Section breaks
#     #### --------------------------------------------------- ####
# Overarching sections #####
# Smaller sections/comments ##
# 
#
# Naming Conventions
# Year
# County
# irrigated_abandon
# non-irrigated_abandon
# total_abandon
# TMAX = Maximum Temperature
# TMIN = Minimum Temperature
# PRCP = Precipitation
# HAIL = Days in which hail occurred. (Count data)
# EXTREME = Number of days beyond 102 Degrees (39C)
# GDD = Growing Degree Days
# Prefix Acronyms 
# S1 = Season 1 First Flower (May and June)
# S2 = Season 2 Last Flower (July and August)
# S3 = Season 3 Last Boll (September and October)
# Total = the combined 3 Seasons


#### --------------------------------------------------- ####
##### Libraries:
library("readr")
library("ggplot2")
library("dplyr")
library("tidyr")
library("patchwork")
library("rlang")
library("tibble")
library("purrr")
library("glue")
library("ggcorrplot")
library("clipr")
library("Kendall")
library("glmnet")
library("car")
library("caret")


##### Defining visual colors:
colors = c(
  "#E69F00",  #1 Orange #Trendline 1
  "#56B4E9",  #2 Sky Blue #Fill color
  "#009E73",  #3 Bluish Green #Trendline 3
  "#F5C710",  #4 Darker Amber #Trendline 2
  "#0072B2",  #5 Blue  -- Base color
  "#D55E00",  #6 Vermillion
  "#CC79A7",  #7 Reddish Purple # Background colors?
  "#000080",  #8 Deep Navy
  "#000000"   #9 Black
) #  okabe_ito


##### Useful functions:
## residual/deviance
resid <- function(model){
  deviance <- rstandard(model, type = "deviance")
  residuals <- data.frame(
    Fitted <- fitted(model),
    deviance <- deviance
  )
  names(residuals) <- c("Fitted", "deviance")
  return(list(residuals = residuals))
}

## RMSE
loss <- function(observed, predicted, weights){
  resid <- (observed - predicted)
  RMSE <- sqrt(sum(weights * (resid)^2)/sum(weights))

  return(RMSE)
}


#### --------------------------------------------------- ####
### Reading in the rds files
#### --------------------------------------------------- ####
Cotton <- read_rds("Data/Cotton_Climate.rds") 
GDD <- read_rds("Data/Accumulated_Climate.rds")

dim(Cotton)
dim(GDD)
unique(Cotton$County)


### Checking for missing data
dim(Cotton)
colSums(is.na(Cotton))
Cotton[which(is.na(Cotton$S1PRCP)),]
Cotton[which(is.na(Cotton$TotalGDD)),]

### Removing Gains County 2024, 2025, 2024, 2012 for missing Climate Data
Cotton <- Cotton[- c(which(is.na(Cotton$S1PRCP)), which(is.na(Cotton$TotalTMIN))),]

### Potter County is removed due to a lack of observations (only two)
Cotton <- Cotton[- which(Cotton$County == "POTTER"),]
dim(Cotton)
colSums(is.na(Cotton))

average_GDD <- GDD %>%
  ## it does not appear that the nested lists created are all of equal length. 
  ## This should create a mapped "day" value that works for each list individually. 
  mutate(GDDhistory = map(GDDhistory, ~ .x %>% ## did not know I could do this, it is super cools :D
                            mutate(day = row_number()))) %>% 
  select(GDDhistory) %>%
  ## unpacking the history data
  unnest(cols = c(GDDhistory)) %>%
  ## assigning values to average over
  group_by(day) %>%
  summarize(Average_GDD = mean(DailyGDD, na.rm = TRUE)) 


## This finds an average overall departure
departures <- GDD %>%
  # Add the day counter to the original nested lists so they match the baseline
  mutate(GDDhistory = map(GDDhistory, ~ .x %>% mutate(day = row_number()))) %>%
  unnest(cols = c(GDDhistory)) %>%
  left_join(average_GDD, by = "day") %>%
  mutate(daily_deviation = DailyGDD - Average_GDD) %>%
  group_by(County, Year) %>%
  summarize(
    TotalDeparture = mean(daily_deviation, na.rm = TRUE),
    S1Departure = mean(daily_deviation[1:61], na.rm = TRUE),
    S2Departure = mean(daily_deviation[62:123], na.rm = TRUE),
    S3Departure = mean(daily_deviation[124:length(daily_deviation)], na.rm = TRUE),
    .groups = "drop"
  )

Cotton <- left_join(Cotton, departures)

#### --------------------------------------------------- ####
#### Hold Out Data  ~ .081                               ####
#### --------------------------------------------------- ####
## beginning 1968 ending in 2025 
HOLD <- Cotton[Cotton$Year %in% c("2020", "2021", "2022", "2023", "2024", "2025"),]
train <- Cotton[Cotton$Year < 2020,]
## DeBugging length(train$Year) + length(HOLD$Year) == length(Cotton$Year)
## length(HOLD$Year)/ (length(Cotton$Year))

#### --------------------------------------------------- ####
#### Hypothesis 1                                        ####
#### --------------------------------------------------- ####

### Mann-Kendall Testing for trends and FDR alpha corrections
ordered_cotton <- Cotton[order(Cotton$Year),]

kendall <- sapply(ordered_cotton[,c(10,13:ncol(ordered_cotton))], function(x) MannKendall(x)$sl)
tau <- sapply(ordered_cotton[,c(10,13:ncol(ordered_cotton))], function(x) MannKendall(x)$tau)
FDR <- p.adjust(kendall, method = "BH") ## FDR alpha correction

true_trend <- data.frame("Variable" = names(FDR), "P-Value" = as.numeric(FDR), "Tau" = round(tau, 3)) 
true_trend$Significant <- ifelse(true_trend[,2] < .05, "Yes", "No")


### Interaction Testing:
fit <- glm(total_abandon ~ TotalTMAX * TotalPRCP, 
           family = quasibinomial("logit"), 
    weight = total_planted, data = Cotton)

summary(fit)

## It seems that the interaction term is indeed significant. 
###


#### --------------------------------------------------- ####
#### General Hypothesis 2, 3  
#### Residual Testing
#### --------------------------------------------------- ####
names(Cotton)

## Baseline
base <- glm(total_abandon ~ TotalTMIN + TotalHAIL +
              TotalEXTREME * TotalPRCP + TotalTMAX * TotalPRCP + County,
            family = quasibinomial(link = "logit"),
            weight = total_planted,
            data = train)
summary(base)

ggplot(resid(base)$residuals, aes(x = Fitted, y = deviance)) + 
  geom_point(color = colors[5]) + 
  labs(
    title = "Standardized Deviance Plot",
    subtitle = "Total Climate Stress",
    x = "Deviance",
    y = "Fitted values",
  ) + theme_minimal()

HOLD$pred_base <- predict(
  base, 
  newdata = HOLD,    
  type = "response"   
)

(HOLD[, c("Year", "County", "total_abandon", "pred_base")])

spatial_rmse <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_base,     
  weights   = HOLD$total_planted
)

print(glue("Spatial GLM Holdout RMSE: {round(spatial_rmse, 4)}"))


## Seasonal
seasonal <- glm(total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                  S1HAIL + S2HAIL + S3HAIL + 
                  S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                  S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP,
                family = quasibinomial(link = "logit"),
                weight = total_planted,
                data = train)
summary(seasonal)

ggplot(resid(seasonal)$residuals, aes(x = Fitted, y = deviance)) + 
  geom_point(color = colors[5]) + 
  labs(
    title = "Standardized Deviance Plot",
    subtitle = "Seasonal Climate Stress",
    x = "Deviance",
    y = "Fitted values",
  ) + theme_minimal()


HOLD$pred_seasonal <- predict(
  seasonal, 
  newdata = HOLD,    
  type = "response"   
)

(HOLD[, c("Year", "County", "total_abandon", "pred_base")])

spatial_rmse <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_seasonal,     
  weights   = HOLD$total_planted
)

print(glue("Spatial GLM Holdout RMSE: {round(spatial_rmse, 4)}"))


# GDD
gdd <- glm(total_abandon ~ County + S1GDD*S1PRCP + S2GDD*S2PRCP + S3GDD*S3PRCP + 
             S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP,
           family = quasibinomial(link = "logit"),
           weight = total_planted,
           data = train)
summary(gdd)

ggplot(resid(gdd)$residuals, aes(x = Fitted, y = deviance)) + 
  geom_point(color = colors[5]) +
  labs(
    title = "Standardized Deviance Plot",
    subtitle = "GDD trends",
    x = "Deviance",
    y = "Fitted values",
  ) + theme_minimal()

HOLD$pred_gdd <- predict(
  gdd, 
  newdata = HOLD,    
  type = "response"   
)

(HOLD[, c("Year", "County", "total_abandon", "pred_base")])

spatial_rmse <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_gdd,     
  weights   = HOLD$total_planted
)

print(glue("Spatial GLM Holdout RMSE: {round(spatial_rmse, 4)}"))

## Departure
departure <- glm(total_abandon ~ County + S1Departure*S1PRCP + S2Departure*S2PRCP + S3Departure*S3PRCP + 
             S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP,
           family = quasibinomial(link = "logit"),
           weight = total_planted,
           data = train)
summary(departure)

ggplot(resid(departure)$residuals, aes(x = Fitted, y = deviance)) + 
  geom_point(color = colors[5]) +  
  labs(
    title = "Standardized Deviance Plot",
    subtitle = "GDD departure trends",
    x = "Deviance",
    y = "Fitted values",
  ) + theme_minimal()

HOLD$pred_departure <- predict(
  departure, 
  newdata = HOLD,    
  type = "response"   
)

(HOLD[, c("Year", "County", "total_abandon", "pred_base")])

spatial_rmse <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_departure,     
  weights   = HOLD$total_planted
)

print(glue("Spatial GLM Holdout RMSE: {round(spatial_rmse, 4)}"))

## Complete
complete <- glm(total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                  S1HAIL + S2HAIL + S3HAIL + 
                  S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP + 
                  S1Departure*S1PRCP + S2Departure*S2PRCP + S3Departure*S3PRCP + 
                  S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                  S1GDD*S1PRCP + S2GDD*S2PRCP + S3GDD*S3PRCP,
                family = quasibinomial(link = "logit"),
                weight = total_planted,
                data = train)
summary(complete)

ggplot(resid(complete)$residuals, aes(x = Fitted, y = deviance)) + 
  geom_point(color = colors[5]) + 
  labs(
    title = "Standardized Deviance Plot",
    subtitle = "Complete Data Set",
    x = "Deviance",
    y = "Fitted values",
  ) + theme_minimal()


HOLD$pred_complete <- predict(
  complete, 
  newdata = HOLD,    
  type = "response"   
)

(HOLD[, c("Year", "County", "total_abandon", "pred_base")])

spatial_rmse <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_complete,     
  weights   = HOLD$total_planted
)

print(glue("Spatial GLM Holdout RMSE: {round(spatial_rmse, 4)}"))

#### --------------------------------------------------- ####
#### General Hypothesis 2, 3  
#### CV Pipline
#### --------------------------------------------------- ####
## Choosing 4 year intervals for training allows 13 complete splits
## By moving one year up, it creates 48 effective splits. 
length(unique(train$Year))/4

## Creating a Custom Cross-Validation Fixed Forward Window Split
years <- sort(unique(train$Year), decreasing = FALSE)
window <- 4

rm(train_index, test_index)
train_index <- list()
test_index <- list()
fold_count <- 1

for(i in 1:(length(years) - window)){
  train_cv <- years[i:(i + window - 1)]
  test_cv <- years[(i + window):(i + window + 1)]
  
  train_index[[fold_count]] <- which(train$Year %in% train_cv)
  test_index[[fold_count]] <- which(train$Year %in% test_cv)
  
  fold_count <- fold_count + 1
}

#### --------------------------------------------------- ####
#### Scaling the data
scaled <- Cotton %>%
  mutate(across(c(TotalTMIN, TotalHAIL, TotalEXTREME, TotalPRCP, TotalTMAX,
                   S1TMIN, S2TMIN, S3TMIN, S1HAIL, S2HAIL, S3HAIL,
                   S1EXTREME, S2EXTREME, S3EXTREME, S1TMAX, S2TMAX, S3TMAX,
                   S1PRCP, S2PRCP, S3PRCP, S1Departure, S2Departure, S3Departure,
                   S1GDD, S2GDD, S3GDD),
                 ~ as.vector(scale(.x))))

HOLD_scaled <- scaled[scaled$Year %in% c("2020", "2021", "2022", "2023", "2024", "2025"),]
train_scaled <- scaled[scaled$Year < 2020,]



#### --------------------------------------------------- ####
#### Generating the input matricies
target <- train_scaled$total_abandon

y_matrix <- cbind(
  Successes = train_scaled$total_planted * train_scaled$total_abandon,
  Failures  = train_scaled$total_planted * (1 - train_scaled$total_abandon)
)

x_total <- model.matrix(total_abandon ~ County + TotalTMIN + TotalHAIL +
                          TotalEXTREME * TotalPRCP + TotalTMAX * TotalPRCP,
                        data = train_scaled)[,-1]

fit <- cv.glmnet(
  x = x_total,
  y = y_matrix,
  family = 'binomial'
)

x_hold <- model.matrix(total_abandon ~ County + TotalTMIN + TotalHAIL +
                         TotalEXTREME * TotalPRCP + TotalTMAX * TotalPRCP,
                       data = HOLD_scaled)[,-1]

HOLD_scaled$pred_abandon <- predict(
  fit, 
  newx = x_hold, 
  s = "lambda.1se",   # Or "lambda.min" depending on your preference
  type = "response"   # CRITICAL: Converts log-odds back to 0-1 probabilities
)

head(HOLD[, c("Year", "County", "total_abandon", "pred_abandon")])


