## Initial Data Sourcing
# Working with Lubbock Data to being
# 
# Comment Conventions
# Section breaks
#     #### --------------------------------------------------- ####
# Overarcking sections #####
# Smaller setions/comments ##
# 
#
# Naming Conventions
# WT-- 05 = Hail, 10 = Tornado, 11 = Damaging Winds
# TMAX = Maximum Temperature
# TMIN = Minimum Temperature
# PRCP = Precipitation
# HAIL = Days in which hail occured. (Count data)
# EXTREME = Number of days beyond 102 Degrees (39C)
# GDD = Growing Degree Days
# Prefix Acronyms 
# S1 = Season 1 First Flower (May and June)
# S2 = Season 2 Last Flower (July and August)
# S3 = Season 3 Last Boll (September and October)
# Total = the combined 3 Seasons


#### --------------------------------------------------- ####
##### Libraries:
library("dplyr")
library("tidyr")
library("lubridate")
library("rlang")
library("writexl")


##### Useful functions
abandonment_rate <- function(Harvest,Total){
  (Total- Harvest)/Total
}

calculate_GDD <- function(TMAX, TMIN){
  sum(pmax((TMAX + TMIN)/2 - 60, 0), na.rm = TRUE)
}

calcualte_GDDA <- function(TMAX, TMIN){
  cumsum(pmax((TMAX + TMIN)/2 - 60, 0), na.rm = TRUE)
}

##### Loading/Merging the data

### Cotton Data
#***** Add a section to control for uncertainty of file location
cotton.data <- read.csv("Original Cotton Abandonment data.csv")
## Removing unwanted ',' is the values data
cotton.data$Value <- gsub(",","", cotton.data$Value)

Cotton <- cotton.data %>%
  filter(County %in% c("POTTER","RANDALL","HALE","LUBBOCK","CROSBY","GAINES")) %>%
  select(Year, Ag.District.Code, County, Data.Item, Value) %>%
  mutate(Value = as.integer(Value)) %>%
  pivot_wider(names_from = Data.Item, values_from = Value) %>%
  rename(
    total_harvested         = "COTTON, UPLAND - ACRES HARVESTED",
    total_planted           = "COTTON, UPLAND - ACRES PLANTED",
    irrigated_harvested     = "COTTON, UPLAND, IRRIGATED - ACRES HARVESTED",
    irrigated_planted       = "COTTON, UPLAND, IRRIGATED - ACRES PLANTED",
    non_irrigated_harvested = "COTTON, UPLAND, NON-IRRIGATED - ACRES HARVESTED",
    non_irrigated_planted   = "COTTON, UPLAND, NON-IRRIGATED - ACRES PLANTED"
  ) %>%
  mutate(total_abandon         = abandonment_rate(total_harvested,total_planted), 
         irrigated_abandon     = abandonment_rate(irrigated_harvested,irrigated_planted),
         non_irrigated_abandon = abandonment_rate(non_irrigated_harvested,non_irrigated_planted))
  
View(Cotton)
unique(Cotton$County)

## Checking for missing values
#aggregate(Value ~ County, data = Cotton, FUN = function(x) sum(is.na(x)))



### Climate Data
lubbock.data <- read.csv("Data\\Lubbock Climate.csv")
climate.data <- read.csv("Data\\West Texas CLimate Data.csv")

names(climate.data)
unique(climate.data$NAME)
str(climate.data)


## renaming to create a relational database with the Cotton 
Climate <- climate.data %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(month(DATE) >= 5 & month(DATE) <= 10) %>%
  select(DATE, NAME, PRCP, TMAX, TMIN, SNOW, WT05, WT11, WT10) %>%
  mutate(WT10 = as.integer(WT10)) %>%
  mutate(across(c(PRCP, SNOW, WT05, WT11, WT10), ~ replace_na(.,0))) %>%
  mutate(County = case_match(
    NAME,
    "PLAINVIEW, TX US"                     ~ "HALE",
    "CROSBYTON, TX US"                     ~ "CROSBY",
    "SEMINOLE, TX US"                      ~ "GAINES",
    "LUBBOCK INTERNATIONAL AIRPORT, TX US" ~ "LUBBOCK",
    "CANYON, TX US"                        ~ "RANDALL",
    "AMARILLO AIRPORT, TX US"              ~ "POTTER"
  )) %>%
  select(-NAME)

View(Climate)


str(Climate)
### Pre-processing Calculations

## setting seasonal splits
first_flower <- expr(month(DATE) %in% c(5,6))
last_flower <- expr(month(DATE) %in% c(7,8))
last_boll <- expr(month(DATE) %in% c(9,10))
   
   
Collapsed_Climate <- Climate %>%
  filter(year(DATE) >= "1968") %>%
  select(DATE, County, PRCP, TMAX, TMIN, SNOW, WT05, WT11, WT10) %>%
  group_by(year(DATE), County) %>%
  summarize(
    ## Calulating average TMAX, TMIN, PRCP, WT05, (WT10 + WT11) for time to first flower
    S1TMAX = mean(TMAX[!!first_flower], na.rm = TRUE),
    S1TMIN = mean(TMIN[!!first_flower], na.rm = TRUE),
    S1PRCP = sum(PRCP[!!first_flower], na.rm = TRUE),
    S1HAIL = sum(WT05[!!first_flower], na.rm = TRUE),
    S1EXTREME = sum(TMAX[!!first_flower] >= 102, na.rm = TRUE),
    S1GDD = calculate_GDD(TMAX[!!first_flower], TMIN[!!first_flower]),
    
    S2TMAX = mean(TMAX[!!last_flower], na.rm = TRUE),
    S2TMIN = mean(TMIN[!!last_flower], na.rm = TRUE),
    S2PRCP = sum(PRCP[!!last_flower], na.rm = TRUE),
    S2HAIL = sum(WT05[!!last_flower], na.rm = TRUE),
    S2EXTREME = sum(TMAX[!!last_flower] >= 102, na.rm = TRUE),
    S2GDD = calculate_GDD(TMAX[!!last_flower], TMIN[!!last_flower]),
    
    S3TMAX = mean(TMAX[!!last_boll], na.rm = TRUE),
    S3TMIN = mean(TMIN[!!last_boll], na.rm = TRUE),
    S3PRCP = sum(PRCP[!!last_boll], na.rm = TRUE),
    S3HAIL = sum(WT05[!!last_boll], na.rm = TRUE),
    S3EXTREME = sum(TMAX[!!last_boll] >= 102, na.rm = TRUE),
    S3GDD = calculate_GDD(TMAX[!!last_boll], TMIN[!!last_boll]),
    
    TotalTMAX = mean(c(S1TMAX,S2TMAX,S3TMAX)),
    TotalTMIN = mean(c(S1TMIN,S2TMIN,S3TMIN)),
    TotalPRCP = sum(c(S1PRCP,S2PRCP,S3PRCP)),
    TotalHAIL = sum(c(S1HAIL,S2HAIL,S3HAIL)),
    TotalEXTREME = sum(c(S1EXTREME,S2EXTREME,S3EXTREME)),
    TotalGDD = sum(c(S1GDD,S2GDD,S3GDD))
    ) %>%
  rename(Year = "year(DATE)")
  
View(Collapsed_Climate)
names(Collapsed_Climate)
names(Cotton)
View(Cotton)

### Merging the Climate and Cotton Data
Cotton_Climate <- Cotton %>%
  left_join(Collapsed_Climate, by = c("Year", "County"))

View(Cotton_Climate)
### Creating a data frame for Accumulated GDD for each region and year. 
#Accumulated_Climate <- Climate %<%
#  filter(year(DATE) >= "1968") %<%



##### For exporting the file as a ready XLS file
#write_xlsx(Cotton_Climate, path = "Data\\Cotton_Climate.xlsx")
