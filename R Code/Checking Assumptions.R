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
library("car")


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


#### --------------------------------------------------- ####
##### Reading in the rds files
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


### Generating the Departure measures
## Calculate GDD departure as a metric, assess it usefulness in modeling
### Creating the Averaged GDD plot:
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
### Ensuring Boundlessness in response
# to remove unwanted variables and key values. 

cotton <- Cotton %>%
  select(10,13:ncol(Cotton))

names(cotton)

sum(cotton$total_abandon > 1 | cotton$total_abandon < 0)
sum(cotton$total_abandon == 1 | cotton$total_abandon == 0)
# The abandonment is total bound by 0 and 1, 
# There are two edge cases both at Zero... 
# This will need to be controlled


### Variance structure

## Checking for constant variance (0 or 1)
variance <- apply(cotton, 2, function(x) round(var(x), 3))
variance

# Other then the response all variances appear different from 0. 

## Multi Collinearity
cotton_vif <- cotton %>%
  select(1:19, 27:last_col())

## All Variables
fit_vif <- lm(total_abandon ~ ., 
              data = cotton_vif)

vif(fit_vif)
mean(vif(fit_vif))## holy avocados this is bad....

## Totals
fit_vif <- lm(total_abandon ~ TotalTMAX + TotalTMIN + TotalPRCP + TotalEXTREME + 
                TotalHAIL + TotalDeparture,
              data = cotton)

vif(fit_vif)
mean(vif(fit_vif))


## Departures
fit_vif <- lm(total_abandon ~ S1Departure + S2Departure + S3Departure + TotalPRCP,
              data = cotton)

vif(fit_vif)
mean(vif(fit_vif))

## Splits
fit_vif <- lm(total_abandon ~ S1TMAX + S2TMAX + S3TMAX + S1TMIN + S2TMIN + S3TMIN +
                S1PRCP + S2PRCP + S3PRCP + S1GDD + S2GDD + S3GDD, 
              data = cotton)

vif(fit_vif)
mean(vif(fit_vif))

## GDD values
fit_vif <- lm(total_abandon ~ S1GDD + S2GDD + S3GDD + TotalPRCP,
              data = cotton)

vif(fit_vif)
mean(vif(fit_vif))

 







