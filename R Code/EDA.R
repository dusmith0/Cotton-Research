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

##### Reading in the rds files
Cotton <- read_rds("Data/Cotton_Climate.rds") 
GDD <- read_rds("Data/Accumulated_Climate.rds")

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

##### EDA

### Abandonment 
## General Values
ave_rate <- aggregate(Cotton$total_abandon ~ Cotton$County, FUN = mean)
names(ave_rate) <- c("County", "Average")
sd_rate <- aggregate(Cotton$total_abandon ~ Cotton$County, FUN = sd)
names(sd_rate) <- c("County", "Standard Deviation")
mean(ave_rate[,2])
mean(sd_rate[,2])

abandon_table <- left_join(ave_rate, sd_rate)
abandon_table

## Some simple testing
norm_test <- shapiro.test(Cotton$total_abandon)$p.value
zero_inflation <- mean(Cotton$total_abandon == 0)

glue("The Total Abandonment has the following meta data:
     Overall Mean:              {mean(ave_rate[,2])}
     Overall Standard Devation: {mean(sd_rate[,2])}
     Shapiro-Wilks Test:        {norm_test}
     Zero inflation:            {zero_inflation}")

### plotting histogram of abandonment


ggplot(data = Cotton, aes(x = (total_abandon))) + 
  geom_histogram(fill = colors[5]) + 
  labs(
    title = "Distribution of Total Abandonment",
    x = "Abandonment Rate",
    y = "Frequency"
  ) + theme_minimal()


#--- Average across year per county and plot for general trend
ggplot(data = Cotton, aes(x = Year, y = total_abandon)) + 
  geom_point() + geom_smooth(method = 'gam', se = FALSE, color = colors[5]) + 
  theme_minimal()

ave_abandon <- Cotton %>%
  select(County, Year, total_abandon, irrigated_abandon, non_irrigated_abandon) %>%
  group_by(Year) %>%
  summarize(
    ave = mean(total_abandon),
    dry_ave = mean(non_irrigated_abandon),
    irr_ave = mean(irrigated_abandon)
  )

ggplot(data = ave_abandon, aes(x = Year, y = ave)) + 
  geom_line(color = colors[5]) + 
  geom_smooth(method = "lm", color = colors[1], se = FALSE) + 
  geom_smooth(method = "loess", color = colors[4], se = FALSE) + 
  labs(
    title = "Averaged Abandonment by year",
    subtitle = "Displayed tend models are the Linear and GAM"
  ) + 
  theme_minimal()

county_abandon <- Cotton %>%
  select(County, Year, total_abandon) %>%
  group_by(County) %>%
  mutate(
    ave = mean(total_abandon)
  )

glimpse(county_abandon)
ggplot(data = county_abandon, aes(x = County, y = total_abandon)) + 
  geom_boxplot(fill = colors[5]) + 
  geom_point(aes(x = County, y = ave), shape = 18, size = 3, color = colors[1]) + 
  labs(
    title = "Abandonment varation across County"
  ) +
  theme_minimal()

## Comparing Irrigated
names(Cotton)
ggplot(data = Cotton) + 
  geom_boxplot(aes(x = total_abandon, y = "Total"), fill = colors[5]) + 
  geom_boxplot(aes(x = non_irrigated_abandon, y = "Dry Land"), fill = colors[5]) + 
  geom_boxplot(aes(x = irrigated_abandon, y = "Irrigated"), fill = colors[5]) + labs(
    title = "Abandonment varation across Irrigation Type",
    x = "Abandonment Rate",
    y = "Irrigation Type"
  ) +
  theme_minimal()

### Climate

names(Cotton)
climate_ave <- Cotton %>%
  group_by(Year) %>%
  summarize(
   s1tmax = mean(S1TMAX),
   s2tmax = mean(S2TMAX),
   s3tmax = mean(S3TMAX),
   totaltmax = mean(TotalTMAX),
   s1tmin = mean(S1TMIN),
   s2tmin = mean(S2TMIN),
   s3tmin = mean(S3TMIN),
   totaltmin = mean(TotalTMIN),
   s1prcp = mean(S1PRCP),
   s2prcp = mean(S2PRCP),
   s3prcp = mean(S3PRCP),
   totalprcp = mean(TotalPRCP),
   s1hail = mean(S1HAIL),
   s2hail = mean(S2HAIL),
   s3hail = mean(S3HAIL),
   totalhail = mean(TotalHAIL),
   s1extreme = mean(S1EXTREME),
   s2extreme = mean(S2EXTREME),
   s3extreme = mean(S3EXTREME),
   totalextreme = mean(TotalEXTREME),
   s1gdd = mean(S1GDD),
   s2gdd = mean(S2GDD),
   s3gdd = mean(S3GDD),
   totalgdd = mean(TotalGDD),
  )


### Mann-Kendall Testing for trends and FDR alpha corrections
install.packages("Kendall")
library(Kendall)
names(Cotton)

ordered_cotton <- Cotton[order(Cotton$Year),]

kendall <- sapply(ordered_cotton[,c(10,13:ncol(ordered_cotton))], function(x) MannKendall(x)$sl)
tau <- sapply(ordered_cotton[,c(10,13:ncol(ordered_cotton))], function(x) MannKendall(x)$tau)
FDR <- p.adjust(kendall, method = "BH") ## FDR alpha correction

true_trend <- data.frame("Variable" = names(FDR), "P-Value" = as.numeric(FDR), "Tau" = round(tau, 3)) 
true_trend$Significant <- ifelse(true_trend[,2] < .05, "Yes", "No")
true_trend

### This function takes climate names and plots 4 time based graphs for each seasonal period. 
plot_average <- function(climate_value){
  seasons <- c(
    "total" = "Total",
    "s1"    = "Season 1",
    "s2"    = "Season 2",
    "s3"    = "Season 3"
  )
  
  climate_axis <- c(
    "prcp"    = "Precipitation", 
    "tmax"    = "Max Temperature", 
    "tmin"    = "Min Temperature", 
    "extreme" = "Extreme Heat > 102", 
    "hail"    = "Hail", 
    "gdd"     = "Growing Degree Days"
  )

  plots <- lapply(names(seasons), function(prefix) { 
    ## prefix is an input value not a predefined object
    
    col_name <- paste0(prefix, climate_value)
    plot_title <- seasons[prefix]
    
    ggplot(data = climate_ave, aes(x = Year, y = !!sym(col_name))) + 
    geom_line(color = colors[5]) + geom_smooth(method = 'loess', se = FALSE, color = colors[1]) +
    labs(title = seasons[prefix], y = climate_axis[climate_value]) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  grid <- wrap_plots(plots, nrow = 1) + 
    plot_annotation(
      title = paste("Yearly Climate Averages", climate_axis[climate_value]),
      subtitle = "Climate data averaged by combining all reagions for each year",
      #tag_levels = list(c("Total", "Season 1", "Season 2", "Season 3")), tag_suffix = ':'
    )
  
  return(grid)
}

climate_values <- c("prcp", "tmax", "tmin", "extreme", "hail", "gdd")
all_plot_averages <- lapply(climate_values, plot_average)
names(all_plot_averages) <- climate_values

all_plot_averages



### GDD -- This is a Tibble....
# glimpse(GDD) ## Debugging
# colSums(is.na(GDD)) ## Debugging
# View(average_GDD) ## Debugging
# sum(is.na(average_GDD$Average_GDD)) ## Debugging cheching for missing


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

### Plotting a single average GDD curve
ggplot(data = average_GDD, aes(x = day, y = Average_GDD)) + 
  geom_line(color = colors[5], linewidth = 1) + 
  geom_vline(xintercept = c(60,120), alpha = .8, linetype = "dashed", color = colors[7]) + 
  labs(
    title = "Growing Degree Days",
    subtitle = "Averaged across all years and counties",
    x = "Days along the Growing Season",
    y = "GDD accumplated values") + 
theme_minimal()



### Creating the 'Shadowed' GDD plot

## Creating an indexing day for the x-axis (plotting)
plot_GDD <- GDD %>%
  mutate(GDDhistory = map(GDDhistory, ~.x %>%
                            mutate(day = row_number()))) %>%
  unnest(cols = c(GDDhistory))

## Shadowed GDD curve
ggplot() + 
  geom_line(data = plot_GDD, 
            aes(x = day, y = DailyGDD, group = interaction(Year, County)),
            color = colors[2], alpha = .4) + 
  geom_line(data = average_GDD, aes(x = day, y = Average_GDD), 
            color = colors[5], linewidth = 1) + 
  geom_vline(xintercept = c(60,120), alpha = .8, linetype = "dashed", color = colors[7]) + 
  labs(
    title = "Growing Degree Day deviation",
    subtitle = "Shows all GDD by year and county vs Averaged GDD",
    y = "Growing Degree Day",
    x = "GDD accumplated values") + 
    theme_minimal()

     

### Correlation Heat map
names(Cotton)
Seasons <- c( ## capitalized to avoid issues with the previous seasons
  "Total" = "Total",
  "S1"    = "Season 1",
  "S2"    = "Season 2",
  "S3"    = "Season 3"
)

corrplots <- lapply(names(Seasons), function(prefix) {
  correlation <- cor(Cotton[,c("total_abandon", paste0(prefix, toupper(climate_values)))])
  colnames(correlation)[colnames(correlation) == "total_abandon"] <- "Abandonment"
  rownames(correlation)[rownames(correlation) == "total_abandon"] <- "Abandonment"
  

  ggcorrplot(correlation, method = "square", type = "lower",
           colors = c("white",colors[2],colors[8]),
           lab = TRUE) + 
    labs(title = "Heatmap",
        subtitle = paste0(Seasons[prefix],  " Climate indicators"))
})

corrplots

### Checking total plot connections between Abandonment and 

