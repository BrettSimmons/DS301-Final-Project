setwd("/Users/madelinewang/Desktop/DS301/DS301-Final-Project")

## import raw data csv as data frame 
library(readr)
Life_Expectancy_Data_Updated <- read.csv("Life-Expectancy-Data-Updated.csv")
set.seed(23)

# look at summary of dataset
summary(Life_Expectancy_Data_Updated)
View(Life_Expectancy_Data_Updated)

library(tidyverse)

# there are 179 unique countries in our data set
length(unique(Life_Expectancy_Data_Updated$Country))

#create training set using averages of all years except 2015
train<-Life_Expectancy_Data_Updated %>%
  filter(Year!=2015) %>%
  group_by(Country, Region) %>%
  summarize(across(2:19, mean))

train <- train[, c(-1, -2, -18, -19)]

le <- train$Life_expectancy

#check to make sure dataframe is formatted correctly
#View(life_expectancy_train)
dim(train)
summary(train)
str(train)


#All checks out so we can use 'life_expectancy_train' as our training set

test<-Life_Expectancy_Data_Updated %>%
  filter(Year==2015) %>%
  select(-Year)

test <- test[, c(-1, -2, -18, -19)]


#View(life_expectancy_test)
summary(test)
dim(test)
str(test)

library(leaps)

##########################
# Best Subset Selection #
#########################

## On the training set
regfit = regsubsets(Life_expectancy~.,data=train,nbest=1,nvmax=15)
regfit.sum = summary(regfit)

n = dim(train)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

cbind(p,rss,adjr2,cp,AIC,BIC)

which.min(BIC) 
which.min(AIC) 
which.min(cp)
which.max(adjr2) 

coef(regfit, which.min(BIC))

model_train = lm(Life_expectancy~Under_five_deaths + Adult_mortality + 
                   Alcohol_consumption + GDP_per_capita, data = train)

predicted_values = predict(model_train,test)
MSE_test = mean((test$Life_expectancy - predicted_values)^2)
# 2.122
MSE_test




#############################################
# Combining CV with 'best' subset selection #
#############################################
best.train = regsubsets(Life_expectancy~.,data=train,nbest=1,nvmax=15)

val.error = rep(NA, 15)

for (i in 1:15){
  test.mat = model.matrix(Life_expectancy~., data = test)
  coef.m = coef(best.train, id = i)
  pred = test.mat[,names(coef.m)]%*%coef.m
  val.error[i] = mean((test$Life_expectancy - pred)^2)
}

which.min(val.error)
#selected M12
coef(best.train, which.min(val.error))

#USing those 12 variables
model_train = lm(Life_expectancy~Infant_deaths + Under_five_deaths + Adult_mortality + 
                   Alcohol_consumption + Hepatitis_B + BMI + Diphtheria + Incidents_HIV + 
                   GDP_per_capita + Population_mln + Thinness_ten_nineteen_years + 
                   Schooling, data = train)

predicted_values = predict(model_train,test)
MSE_test = mean((test$Life_expectancy - predicted_values)^2)
# 2.103
MSE_test

# Forward Selection
regfit.fwd = regsubsets(Life_expectancy~.,data=train,nvmax=15, method="forward")
summary(regfit.fwd)

## Smallest to biggest
test[order(test$Life_expectancy),]
order(predicted_values)

## Biggest to smallest
test[order(test$Life_expectancy, decreasing = TRUE),]
order(predicted_values, decreasing = TRUE)


## For all variables
model_train = lm(Life_expectancy~., data = train)

predicted_values = predict(model_train,test)
MSE_test = mean((test$Life_expectancy - predicted_values)^2)
MSE_test


library(ggplot2)
library(reshape2)

test<-Life_Expectancy_Data_Updated %>%
  filter(Year==2015) %>%
  select(-Year)

data <- data.frame(test[c(44, 57, 142, 22, 65),], predicted = predicted_values[c(44, 57, 142, 22, 65)])
df_melt <- melt(data[, c("Country", "Life_expectancy", "predicted")], id.vars = "Country")

# Create a side-by-side bar graph
ggplot(df_melt, aes(x = Country, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Life Expectancy", fill = NULL)


predict <- data.frame(test[c(148, 107, 28, 9, 86),], predicted = predicted_values[c(148, 107, 28, 9, 86)])
predict_melt <- melt(predict[, c("Country", "Life_expectancy", "predicted")], id.vars = "Country")

# Create a side-by-side bar graph
ggplot(predict_melt, aes(x = Country, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Life Expectancy", fill = NULL)


test <- data.frame(test[c(86, 107, 2, 133, 136),], predicted = predicted_values[c(86, 107, 2, 133, 136)])
test_melt <- melt(test[, c("Country", "Life_expectancy", "predicted")], id.vars = "Country")

# Create a side-by-side bar graph
ggplot(test_melt, aes(x = Country, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Life Expectancy", fill = NULL)
