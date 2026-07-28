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
library("betareg")
library("ranger")

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
    ## residual calculation
    resid <- (observed - predicted)
    
    ##RMSE Loss
    RMSE <- sqrt(mean(resid^2))
    RMSE_weighted <- sqrt(sum(weights * (resid)^2)/sum(weights))
    
    ## Log Loss
    predicted <- pmax(pmin(predicted, 1 - 1e-15), 1e-15)
    
    Log_loss          <- -mean(observed * log(predicted) + (1 - observed) * log(1 - predicted))
    Log_loss_weighted <- -sum(weights * (observed * log(predicted) + (1 - observed) * log(1 - predicted))) / sum(weights)
    
    return(list(RMSE = RMSE,
                RMSE_weighted = RMSE_weighted, 
                Log_loss = Log_loss, 
                Log_loss_weighted = Log_loss_weighted))
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


## Debugging length(train$Year) + length(HOLD$Year) == length(Cotton$Year)
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
nrow(true_trend)

### Interaction Testing:
fit <- glm(total_abandon ~ TotalTMAX * TotalPRCP, 
           family = quasibinomial("logit"), 
    weight = total_planted, data = Cotton)

summary(fit)

## It seems that the interaction term is indeed significant. 


#### --------------------------------------------------- ####
#### General Hypothesis 2, 3  
#### Residual Testing
#### --------------------------------------------------- ####
names(Cotton)

#### --------------------------------------------------- ####
#### Quasibinomial

## Baseline
base <- glm(total_abandon ~ TotalTMIN + TotalHAIL +
            TotalEXTREME * TotalPRCP + TotalTMAX * TotalPRCP + County,
            family = quasibinomial(link = "logit"),
            weights = total_planted,
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

error <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_base,     
  weights   = HOLD$total_planted
)

error


## Seasonal
seasonal <- glm(total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                  S1HAIL + S2HAIL + S3HAIL + 
                  S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                  S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP,
                family = quasibinomial(link = "logit"),
                weights = total_planted,
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

(HOLD[, c("Year", "County", "total_abandon", "pred_seasonal")])

error <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_seasonal,     
  weights   = HOLD$total_planted
)

error


# GDD
gdd <- glm(total_abandon ~ County + S1GDD*S1PRCP + S2GDD*S2PRCP + S3GDD*S3PRCP + 
             S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP,
           family = quasibinomial(link = "logit"),
           weights = total_planted,
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

(HOLD[, c("Year", "County", "total_abandon", "pred_gdd")])

error <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_gdd,     
  weights   = HOLD$total_planted
)

error

## Departure
departure <- glm(total_abandon ~ County + S1Departure*S1PRCP + S2Departure*S2PRCP + S3Departure*S3PRCP + 
             S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP,
           family = quasibinomial(link = "logit"),
           weights = total_planted,
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

(HOLD[, c("Year", "County", "total_abandon", "pred_departure")])

error <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_departure,     
  weights   = HOLD$total_planted
)

error

## Complete
complete <- glm(total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                  S1HAIL + S2HAIL + S3HAIL + 
                  S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP + 
                  S1Departure*S1PRCP + S2Departure*S2PRCP + S3Departure*S3PRCP + 
                  S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                  S1GDD*S1PRCP + S2GDD*S2PRCP + S3GDD*S3PRCP,
                family = quasibinomial(link = "logit"),
                weights = total_planted,
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

(HOLD[, c("Year", "County", "total_abandon", "pred_complete")])

error <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_complete,     
  weights   = HOLD$total_planted
)

error


#### --------------------------------------------------- ####
#### Beta Regression   LASSO can be done with "Bamlss"

## Preparing for Beta Regression
## As only two values are 0 we will force them to .0001

train$total_abandon[which(train$total_abandon == 0)] <- .0001

## 
## here the | is being used as a weighted precision parameter not a true weights
## it is the phi weighting in the beta distribution. 
beta_total <- betareg(total_abandon ~ County + S1Departure*S1PRCP + 
                      S2Departure*S2PRCP + S3Departure*S3PRCP + 
                      S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + 
                      S3EXTREME*S3PRCP | total_planted,
                      link = "logit",
                      data = train)

summary(beta_total)

HOLD$pred_beta_total <- predict(
  beta_total, 
  newdata = HOLD,    
  type = "response"   
)

(HOLD[, c("Year", "County", "total_abandon", "pred_beta_total")])

error <- loss(
  observed  = HOLD$total_abandon, 
  predicted = HOLD$pred_beta_total,     
  weights   = HOLD$total_planted
)

error


#### --------------------------------------------------- ####
#### General Hypothesis 2, 3  
#### CV Pipeline
#### --------------------------------------------------- ####
## Choosing 4 year intervals for training allows 13 complete splits
## By moving one year up, it creates 48 effective splits. 
length(unique(train$Year))/4

## Creating a Custom Cross-Validation Fixed Forward Window Split
years <- sort(unique(train$Year), decreasing = FALSE)
window <- 10
test_window <- 4

rm(train_index, test_index)
train_index <- list()
test_index <- list()
fold_count <- 0

for(i in 1:(length(years) - window - (test_window - 1))){
  fold_count <- fold_count + 1
  
  train_cv <- years[i:(i + window - 1)]
  test_cv <- years[(i + window):(i + window + (test_window - 1))]
  
  train_index[[fold_count]] <- which(train$Year %in% train_cv)
  test_index[[fold_count]] <- which(train$Year %in% test_cv)

}

#### --------------------------------------------------- ####
#### Scaling the data This needs to change I ended up not scaling the data
scaled <- scale(train[,13:40])
scale_mean <- attr(scaled, "scaled:center")
scale_sd <- attr(scaled, "scaled:scale")

train_scaled <- train
train_scaled[,13:40] <- as.data.frame(scaled)

HOLD_scaled <- HOLD
HOLD_scaled[,13:40] <- as.data.frame(scale(HOLD[,13:40], center = scale_mean, scale = scale_sd))


#### --------------------------------------------------- ####
#### Creating the CV loop
train_index
test_index
fold_count

cv_glm <- function(model, data, train_index, test_index){
  Errors <- matrix(nrow = fold_count, ncol = 4)

  for(i in 1:fold_count){
    train_fold <- data[train_index[[i]],]
    test_fold  <- data[test_index[[i]],]

    # This will force both train and test to contain all counties as a factor 
    # Even if one is missing a county 
    train_fold$Set <- "Train"
    test_fold$Set  <- "Test"
    combined_fold  <- bind_rows(train_fold, test_fold)
    combined_fold$County <- factor(combined_fold$County)
    
    # Resetting and splitting the model
    master_matrix <- model.matrix(as.formula(model), data = combined_fold)[, -1]
    
    x_train <- master_matrix[combined_fold$Set == "Train", ]
    x_test  <- master_matrix[combined_fold$Set == "Test", ]
    
    # Setting costume fail/success for the quasi binomial
    y_train_prop <- cbind(
      Failures  = 1 - train_fold$total_abandon,
      Successes = train_fold$total_abandon
    )
    
    # Setting the phi-value
    norm_weights <- train_fold$total_planted / mean(train_fold$total_planted)
    
    # Forcing County to avoid regularization
    County_factors <- ifelse(grepl("County", colnames(master_matrix)), 0, 1)
    
    # Fitting Elsatic Net
    fit_fold <- cv.glmnet(
      x = x_train, 
      y = y_train_prop, 
      weights = norm_weights, 
      family = "binomial", 
      intercept = FALSE,
      alpha = .3,
      penalty.factor = County_factors
    )
    
    # Generating the prediction. 
    pred <- predict(fit_fold, 
                     newx = x_test, 
                     s = "lambda.min", 
                     type = "response")

  
    Errors[i,] <- unlist(loss(
      observed = test_fold$total_abandon,
      predicted = pred,
      weights = test_fold$total_planted
    ))
  
  }
  colnames(Errors) <- c("RMSE", "W_RMSE", "Log", "W_Log")
  return(Errors)
}


#### --------------------------------------------------- ####
#### Preforming Cross Validation
#### --------------------------------------------------- ####
models <- c("total_abandon ~  County + TotalTMIN + TotalHAIL +
                  TotalEXTREME * TotalPRCP + 
                  TotalTMAX * TotalPRCP", # Total
            
                 "total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                  S1HAIL + S2HAIL + S3HAIL + 
                  S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                  S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP", # Seasonal
            
                 "total_abandon ~ County + S1GDD*S1PRCP + S2GDD*S2PRCP + 
                  S3GDD*S3PRCP + S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + 
                  S3EXTREME*S3PRCP", # GDD 
            
                 "total_abandon ~ County + S1Departure*S1PRCP + S2Departure*S2PRCP +
                  S3Departure*S3PRCP + S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + 
                  S3EXTREME*S3PRCP", # Departure
            
                 "total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                  S1HAIL + S2HAIL + S3HAIL + 
                  S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP + 
                  S1Departure*S1PRCP + S2Departure*S2PRCP + S3Departure*S3PRCP + 
                  S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                  S1GDD*S1PRCP + S2GDD*S2PRCP + S3GDD*S3PRCP")

CV_Loss_Table <- matrix(nrow = 4, ncol = 5)
RMSE_Loss <- matrix(nrow = fold_count, ncol = 5)
WRMSE_Loss <- matrix(nrow = fold_count, ncol = 5)
for(i in 1:length(models)){
  err <- cv_glm(model = models[i], data = train_scaled, train_index = train_index, test_index = test_index)
  
  RMSE_Loss[,i] <- err[,1]
  WRMSE_Loss[,i] <- err[,2]
  CV_Loss_Table[,i] <- colMeans(err)
  colnames(RMSE_Loss) <- c("Total","Seasonal","GDD","Departure","Complete")
  colnames(CV_Loss_Table) <- c("Total","Seasonal","GDD","Departure","Complete")
}

CV_Loss_Table
RMSE_Loss
WRMSE_Loss

#### --------------------------------------------------- ####
#### H2 and H3 Statistical Tests and Confidence Intervals
#### --------------------------------------------------- ####
.05/4
seasonal_p <- wilcox.test(WRMSE_Loss[,2], WRMSE_Loss[,1], paired = TRUE, exact = FALSE)$p.value
GDD_p <- (wilcox.test(WRMSE_Loss[,1], WRMSE_Loss[,3], paired = TRUE, exact = FALSE))$p.value
departure_p <- (wilcox.test(WRMSE_Loss[,1], WRMSE_Loss[,4], paired = TRUE, exact = FALSE))$p.value
wilcox.test(WRMSE_Loss[,1], WRMSE_Loss[,5], paired = TRUE, exact = FALSE)


print(glue("Wilcoxon P-value Total vs Seasonal is: {round(seasonal_p, 4)}"))
print(glue("Wilcoxon P-value Total vs GDD is: {round(GDD_p, 4)}"))
print(glue("Wilcoxon P-value Total vs Departue is: {round(departure_p, 4)}"))


Interval <- matrix(nrow = 2, ncol = 5)
for(i in 1:5){
  Interval[,i] <- (mean(WRMSE_Loss[,i]) + c(-1,1)*sd(WRMSE_Loss)/sqrt(fold_count))
  colnames(Interval) <- c("Total","Seasonal","GDD","Departure","Complete")
}

Interval



#### --------------------------------------------------- ####
#### Single LASSO to see what is removed
#### --------------------------------------------------- ####
train_scaled$Set <- "Train"
HOLD_scaled$Set <- "Holdout"

combined_data <- bind_rows(train_scaled, HOLD_scaled)

combined_data$County <- as.factor(combined_data$County)

master_matrix <- model.matrix(total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
                                S1HAIL + S2HAIL + S3HAIL + 
                                S1TMAX*S1PRCP + S2TMAX*S2PRCP + S3TMAX*S3PRCP + 
                                S1Departure*S1PRCP + S2Departure*S2PRCP + S3Departure*S3PRCP + 
                                S1EXTREME*S1PRCP + S2EXTREME*S2PRCP + S3EXTREME*S3PRCP + 
                                S1GDD*S1PRCP + S2GDD*S2PRCP + S3GDD*S3PRCP + 
                                TotalTMAX + TotalTMIN + TotalPRCP,
                              data = combined_data)[, -1] 

x_total <- master_matrix[combined_data$Set == "Train", ]
x_hold  <- master_matrix[combined_data$Set == "Holdout", ]

y_matrix_prop <- cbind(
  Successes = train_scaled$total_abandon,
  Failures  = 1 - train_scaled$total_abandon
)

norm_weights <- train_scaled$total_planted / mean(train_scaled$total_planted)

fit <- cv.glmnet(
  x = x_total,
  y = y_matrix_prop,
  weights = norm_weights,
  family = 'binomial',
  intercept = FALSE,
  alpha = 1 
)

HOLD_scaled$pred_abandon <- (1 - predict(
  fit, 
  newx = x_hold,       
  s = "lambda.1se",    
  type = "response"    
))

print(head(HOLD_scaled[, c("Year", "County", "total_abandon", "pred_abandon")]))

lasso_coefs <- coef(fit, s = "lambda.1se")

print(lasso_coefs)






#### --------------------------------------------------- ####
#### Hypotheses 4 Random Forrest  
#### 
#### --------------------------------------------------- ####
set.seed(806)

train
train_index
test_index
fold_count

### An initial Random Forrest
forest <- ranger(
  total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
  S1HAIL + S2HAIL + S3HAIL + S1TMAX + S2TMAX + S3TMAX +
  S1PRCP + S2PRCP + S3PRCP + S1Departure + S2Departure +
  S3Departure + S1EXTREME + S2EXTREME + S3EXTREME +
  S1GDD + S2GDD + S3GDD,
  data = train_scaled,
  num.trees  = 500,
  case.weights = train_scaled$total_planted,
)

print(forest)

HOLD$Forest <- predict(forest, data = HOLD_scaled)$predictions

head(HOLD[, c("Year", "County", "total_abandon", "Forest")])



#### --------------------------------------------------- ####
#### CV Loop
#### --------------------------------------------------- ####
cv_forest <- function(model, data, train_index, test_index){
  Errors <- matrix(nrow = fold_count, ncol = 4)
  
  for(i in 1:fold_count){
    train_fold <- data[train_index[[i]],]
    test_fold  <- data[test_index[[i]],]
    
    # This will force both train and test to contain all counties as a factor 
    # Even if one is missing a county 
    all_counties <- unique(c(as.character(train_fold$County), as.character(test_fold$County)))
    train_fold$County <- factor(train_fold$County, levels = all_counties)
    test_fold$County  <- factor(test_fold$County,  levels = all_counties)
    
    # Random Forest model
    forest <- ranger(
      formula = as.formula(model),
      data = train_fold,
      num.trees  = 500,
      case.weights = train_fold$total_planted,
      seed = 806
    )
    
    # Generating the prediction. 
    pred <- predict(forest, data = test_fold)$predictions
    
    
    Errors[i,] <- unlist(loss(
      observed = test_fold$total_abandon,
      predicted = pred,
      weights = test_fold$total_planted
    ))
    
  }
  colnames(Errors) <- c("RMSE", "W_RMSE", "Log", "W_Log")
  return(Errors)
}


#### --------------------------------------------------- ####
#### Running the Forest CV and Test
#### --------------------------------------------------- ####
model_forest <- "total_abandon ~ County + S1TMIN + S2TMIN + S3TMIN + 
  S1HAIL + S2HAIL + S3HAIL + S1TMAX + S2TMAX + S3TMAX +
  S1PRCP + S2PRCP + S3PRCP + S1Departure + S2Departure +
  S3Departure + S1EXTREME + S2EXTREME + S3EXTREME +
  S1GDD + S2GDD + S3GDD"


err_forest <- cv_forest(model = model_forest, data = train_scaled, train_index = train_index, test_index = test_index)
colMeans(err_forest)

forest_p <- wilcox.test(RMSE_Loss[,1], err_forest[,1], paired = TRUE, exact = FALSE)$p.value
forest_wp <- wilcox.test(RMSE_Loss[,2], err_forest[,2], paired = TRUE, exact = FALSE)$p.value

print(glue("GLM vs RandomForest: {round(forest_p, 4)}"))
print(glue("GLM vs RandomForest Weighted: {round(forest_p, 4)}"))

(mean(err_forest[,1]) + c(-1,1)*sd(err_forest[,1])/sqrt(fold_count))
(mean(err_forest[,2]) + c(-1,1)*sd(err_forest[,2])/sqrt(fold_count))


#### --------------------------------------------------- ####
#### Forest on Out of sample data for predictions
#### --------------------------------------------------- ####
Use this : importance = "permutation"



