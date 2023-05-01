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
##The whole dataset 
df = Life_Expectancy_Data_Updated[, c(-1, -2, -3, -19, -20)]
regfit = regsubsets(Life_expectancy~.,data=df,nbest=1,nvmax=15)
regfit.sum = summary(regfit)

n = dim(df)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

cbind(p,rss,adjr2,cp,AIC,BIC)

which.min(BIC) #M10
which.min(AIC) #M10
which.min(cp) #M10
which.max(adjr2) #M11
#Three out of the four selected M10
coef(regfit, which.min(AIC))


## On the training set
regfit = regsubsets(Life_expectancy~.,data=train,nbest=1,nvmax=15)
regfit.sum = summary(regfit)

n = dim(df)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

cbind(p,rss,adjr2,cp,AIC,BIC)

which.min(BIC) #M10
which.min(AIC) #M12
which.min(cp) #M8
which.max(adjr2) #M10
#Two out of the four selected M10
coef(regfit, which.min(BIC))

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
MSE_test

#The whole dataset
fit = lm(Life_expectancy~Infant_deaths + Under_five_deaths + Adult_mortality + 
           Alcohol_consumption + Hepatitis_B + BMI + Diphtheria + Incidents_HIV + 
           GDP_per_capita + Population_mln + Thinness_ten_nineteen_years + 
           Schooling, data = df)
summary(fit)



## For all variables
model_train = lm(Life_expectancy~., data = train)

predicted_values = predict(model_train,test)
MSE_test = mean((test$Life_expectancy - predicted_values)^2)
MSE_test

## Using the variable from BIC
model_train = lm(Life_expectancy~Infant_deaths + Under_five_deaths + Adult_mortality + 
                   Alcohol_consumption + Hepatitis_B + BMI + Diphtheria + Incidents_HIV + 
                   GDP_per_capita + Thinness_ten_nineteen_years, data = train)

predicted_values = predict(model_train,test)
MSE_test = mean((test$Life_expectancy - predicted_values)^2)
MSE_test




