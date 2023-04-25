## import raw data csv as data frame 
library(readr)
Life_Expectancy_Data_Updated <- read_csv("Life-Expectancy-Data-Updated.csv")

# look at summary of dataset
summary(Life_Expectancy_Data_Updated)
View(Life_Expectancy_Data_Updated)

library(tidyverse)

# there are 179 unique countries in our data set
length(unique(Life_Expectancy_Data_Updated$Country))

#create training set using averages of all years except 2015
life_expectancy_train<-Life_Expectancy_Data_Updated %>%
  filter(Year!=2015) %>%
  group_by(Country, Region) %>%
  summarize(across(2:19, mean))

#check to make sure dataframe is formatted correctly
View(life_expectancy_avg)
summary(life_expectancy_avg)
str(life_expectancy_avg)

#All checks out so we can use 'life_expectancy_train' as our training set

life_expectancy_test<-Life_Expectancy_Data_Updated %>%
  filter(Year==2015)