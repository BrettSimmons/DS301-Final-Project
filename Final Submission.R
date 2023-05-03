###############
## libraries ##
###############

library(readr)
library(tidyverse)
library(leaps)
library(glmnet)
library(car)

###################################################
## Data cleaning and preparation - Abbie Pigatto ##
###################################################

# import raw data csv as data frame 
Life_Expectancy_Data_Updated <- read_csv("Life-Expectancy-Data-Updated.csv")

#look at summary of data set
summary(Life_Expectancy_Data_Updated)
View(Life_Expectancy_Data_Updated)

#check for NA values; no NA values present
sum(is.na(Life_Expectancy_Data_Updated))

# there are 179 unique countries in our data set
length(unique(Life_Expectancy_Data_Updated$Country))

#create training set using averages of all years except 2015
life_expectancy_train<-Life_Expectancy_Data_Updated %>%
  filter(Year!=2015) %>%
  group_by(Country, Region) %>%
  summarize(across(2:19, mean)) %>%
  select(-Economy_status_Developing)


#check to make sure data frame is formatted correctly
#View(life_expectancy_train)
dim(life_expectancy_train)
summary(life_expectancy_train)
str(life_expectancy_train)

#All checks out so we can use 'life_expectancy_train' as our training set

#create test set using data from year 2015
life_expectancy_test<-Life_Expectancy_Data_Updated %>%
  filter(Year==2015) %>%
  select(-c(Year,Economy_status_Developing))



#check to make sure data frame is formatted correctly
#View(life_expectancy_test)
summary(life_expectancy_test)
dim(life_expectancy_test)
str(life_expectancy_test)

#All is good so we can use 'life_expectancy_test' as our test set


#Create new variable above_below_GDP, which indicates if a countries GDP is above or below the average GDP in the data set

mean_GDP=mean(life_expectancy_train$GDP_per_capita)

above_or_below_GDP=ifelse(life_expectancy_train$GDP_per_capita>=mean_GDP,1,0)

life_expectancy_train['above_or_below_GDP']<-above_or_below_GDP

mean_GDP=mean(life_expectancy_test$GDP_per_capita)

above_or_below_GDP=ifelse(life_expectancy_test$GDP_per_capita>=mean_GDP,1,0)

life_expectancy_test['above_or_below_GDP']<-above_or_below_GDP

###########################################
## MLR, Ridge, and Lasso - Brett Simmons ##
###########################################

### Creating scatterplots for hypothesis & to examine relationships ###
ggplot(Life_Expectancy_Data_Updated, aes(x = Infant_deaths, y = Life_expectancy)) + 
  geom_point() + 
  labs(x = "Infant deaths (per 1000 live births)", y = "Life expectancy (years)") 

ggplot(Life_Expectancy_Data_Updated, aes(x = Adult_mortality, y = Life_expectancy)) + 
  geom_point() + 
  labs(x = "Adult mortality (per 1000 population)", y = "Life expectancy (years)")

ggplot(Life_Expectancy_Data_Updated, aes(x = Schooling, y = BMI, color = Life_expectancy >= 68.68223)) + 
  geom_point() +
  scale_color_manual(values = c("orange", "darkgreen"), labels = c("Life Expectancy < 68.68", "Life Expectancy >= 68.68")) +
  labs(x = "Schooling", y = "BMI") + theme(legend.title= element_blank())

ggplot(Life_Expectancy_Data_Updated, aes(x = Hepatitis_B, y = Polio, color = Life_expectancy >= 68.68223)) + 
  geom_point() +
  scale_color_manual(values = c("orange", "darkgreen"), labels = c("Life Expectancy < 68.68", "Life Expectancy >= 68.68")) +
  labs(x = "Hepatitis B immunization (%)", y = "Polio immunization (%)") + theme(legend.position="none")
### Linear Regression ### 

mlr.model = lm(Life_expectancy~.,data=life_expectancy_train)
predicted_values = predict(mlr.model,life_expectancy_test)

MSE_test = mean((life_expectancy_test$Life_expectancy - predicted_values)^2)
MSE_test # Test MSE of 2.1146

### Ridge Regression ### 

# Lambda grid
grid = 10^seq(10,-2,length=100)

# Full data set
x = model.matrix(Life_expectancy~.,data=Life_Expectancy_Data_Updated)[,-1] 
Y = Life_Expectancy_Data_Updated$Life_expectancy

# Create training set 
x.train = model.matrix(Life_expectancy~.,data=life_expectancy_train)[,-1] 
Y.train = life_expectancy_train$Life_expectancy

# Create testing set
x.test = model.matrix(Life_expectancy~.,data=life_expectancy_test)[,-1] 
Y.test = life_expectancy_test$Life_expectancy

# Fit ridge regression model on training set
ridge.train = glmnet(x.train,Y.train,alpha=0,lambda=grid)

# Run cross-validation on training set to find optimal lambda
set.seed(23)
cv.out.ridge = cv.glmnet(x.train,Y.train,alpha = 0, lambda = grid) 
plot(cv.out.ridge)
bestlambda = cv.out.ridge$lambda.min
bestlambda

# Use optimal lambda to run prediction and find our test MSE
ridge.pred = predict(ridge.train,s=bestlambda,newx=x.test)
test.MSE = mean((ridge.pred-Y.test)^2)
test.MSE

# Run model on full dataset
final.model = glmnet(x,Y,alpha=0,lambda = bestlambda)
coef(final.model)


### Lasso Regression ###


# Fit lasso regression model on training set
lasso.train = cv.glmnet(x.train,Y.train,alpha=1,lambda=grid) 

# Run cross-validation on training set to find optimal lambda
cv.out.lasso = cv.glmnet(x.train,Y.train,alpha = 1, lambda = grid) 
plot(cv.out.lasso)
bestlambda2 = cv.out.lasso$lambda.min
bestlambda2

# Use optimal lambda for prediction and test MSE
lasso.pred = predict(lasso.train,s=bestlambda2,newx=x.test)
mean((lasso.pred-Y.test)^2)

# Run model on full dataset
final.lasso = glmnet(x,Y,alpha=1,lambda=bestlambda2)
coef(final.lasso)

### Lasso a slightly smaller test MSE than Ridge, 2.1378 vs 2.1429 ###

###########################################
## Best Subset Selection - Madeline Wang ##
###########################################

set.seed(23)

train <- life_expectancy_train[, c(-1, -2, -18)]

le <- train$Life_expectancy

test <- life_expectancy_test[, c(-1, -2, -18)]

## Best Subset Selection on training set

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






