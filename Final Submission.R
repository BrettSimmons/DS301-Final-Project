## import raw data csv as data frame 
#install.packages("readr")
library(readr)
Life_Expectancy_Data_Updated <- read_csv("Life-Expectancy-Data-Updated.csv")

# look at summary of data set
summary(Life_Expectancy_Data_Updated)
View(Life_Expectancy_Data_Updated)

#install.packages("tidyverse")
library(tidyverse)

# there are 179 unique countries in our data set
length(unique(Life_Expectancy_Data_Updated$Country))

#create training set using averages of all years except 2015
life_expectancy_train<-Life_Expectancy_Data_Updated %>%
  filter(Year!=2015) %>%
  group_by(Country, Region) %>%
  summarize(across(2:19, mean))

#check to make sure data frame is formatted correctly
#View(life_expectancy_train)
dim(life_expectancy_train)
summary(life_expectancy_train)
str(life_expectancy_train)

#All checks out so we can use 'life_expectancy_train' as our training set

#create test set using data from year 2015
life_expectancy_test<-Life_Expectancy_Data_Updated %>%
  filter(Year==2015) %>%
  select(-Year)

#check to make sure data fram is formatted correctly
#View(life_expectancy_test)
summary(life_expectancy_test)
dim(life_expectancy_test)
str(life_expectancy_test)

#All is good so we can use 'life_expectancy_test' as our test set

