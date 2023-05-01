library(readr)
library(leaps)
library(tidyverse)
library(glmnet)

Life_Expectancy_Data_Updated <- read_csv("Life-Expectancy-Data-Updated.csv")

# Create training set using averages of all years except 2015
life_expectancy_train<-Life_Expectancy_Data_Updated %>%
  filter(Year!=2015) %>%
  group_by(Country, Region) %>%
  summarize(across(2:19, mean)) %>%
  ungroup()

# Create testing set using data from 2015
life_expectancy_test<-Life_Expectancy_Data_Updated %>%
  filter(Year==2015) %>%
  select(-Year)

# Remove unnecessary variables
Life_Expectancy_Data_Updated = select(life_expectancy_train,-Country,-Region,
                                      -Economy_status_Developed,-Economy_status_Developing)

life_expectancy_train = select(life_expectancy_train,-Country,-Region,
                               -Economy_status_Developed,-Economy_status_Developing)

life_expectancy_test = select(life_expectancy_test,-Country,-Region,
                              -Economy_status_Developed,-Economy_status_Developing)


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
coef(final)


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
