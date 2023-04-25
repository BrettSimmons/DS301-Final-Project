## import raw data csv as data frame 
library(readr)
Life_Expectancy_Data_Updated <- read_csv("Life-Expectancy-Data-Updated.csv")

# look at summary of dataset
summary(Life_Expectancy_Data_Updated)
View(Life_Expectancy_Data_Updated)

## create new Dataset with averages for each countryn
library(tidyverse)

# there are 179 unique countries in our data set
length(unique(Life_Expectancy_Data_Updated$Country))

life_expectancy_avg<-Life_Expectancy_Data_Updated %>%
  group_by(Country, Region) %>%
  summarize(across(2:19, mean))

#check to make sure dataframe is formatted correctly
View(life_expectancy_avg)
summary(life_expectancy_avg)
str(life_expectancy_avg)

#All checks out so we can continue on using the 'life_expectancy_avg' data frame