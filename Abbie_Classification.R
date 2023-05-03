# Question: Given a set of health related predictors, is a countries GDP per capita above or below the world average?

#check distribution of 'GDP_per_capita'

hist(life_expectancy_train$GDP_per_capita)

# GDP_per_capita is not normally distributed

# k-fold cross validation

#install.packages("caret")
library(caret)

#install.packages('class')
library(class)


standardized_X = scale(life_expectancy_train[,-c(1,2,13,20)])

set.seed(23)
flds <- createFolds(life_expectancy_train$GDP_per_capita, k = 10, list = TRUE, returnTrain = FALSE)

#is there any type of method to determine what K's to select?
K= c(1,3,5,7,9,11,13,15,17,19,21)

cv_error = matrix(NA, 10, 11)

for(j in 1:11){
  k = K[j]
  for(i in 1:10){
    test_index = flds[[i]]
    testX = standardized_X[test_index,]
    trainX = standardized_X[-test_index,]
    
    trainY = life_expectancy_train$above_or_below_GDP[-test_index]
    testY = life_expectancy_train$above_or_below_GDP[test_index]
    
    knn.pred = knn(trainX,testX,trainY,k=k)
    cv_error[i,j] = mean(testY!=knn.pred)
  }
}

optimal_k=(which.min(apply(cv_error,2,mean)))
plot(K,apply(cv_error,2,mean),ylab = "average CV error")

#K=7 is optimal K

standardized_X_test = scale(life_expectancy_test[,-c(1,2,13,20)])

train_X = standardized_X
test_X = standardized_X_test
train_Y = life_expectancy_train$above_or_below_GDP
test_Y = life_expectancy_test$above_or_below_GDP

knn_pred = knn(train_X,test_X,train_Y,k=K[optimal_k])

#confusion matrix
table(knn_pred,test_Y)

#missclassification error
mean(test_Y!=knn_pred)

#false positive =  0.1086957
false_positive = 15/(123+15)


## attempt using predictors identified by Amanda's single tree


standardized_X = scale(life_expectancy_train[,c("Under_five_deaths","Population_mln","Infant_deaths","Hepatitis_B","BMI")])

for(j in 1:11){
  k = K[j]
  for(i in 1:10){
    test_index = flds[[i]]
    testX = standardized_X[test_index,]
    trainX = standardized_X[-test_index,]
    
    trainY = life_expectancy_train$above_or_below_GDP[-test_index]
    testY = life_expectancy_train$above_or_below_GDP[test_index]
    
    knn.pred = knn(trainX,testX,trainY,k=k)
    cv_error[i,j] = mean(testY!=knn.pred)
  }
}

optimal_k=(which.min(apply(cv_error,2,mean)))
plot(K,apply(cv_error,2,mean),ylab = "average CV error")

standardized_X_test = scale(life_expectancy_test[,c("Under_five_deaths","Population_mln","Infant_deaths","Hepatitis_B","BMI")])

train_X = standardized_X
test_X = standardized_X_test
train_Y = life_expectancy_train$above_or_below_GDP
test_Y = life_expectancy_test$above_or_below_GDP

knn_pred = knn(train_X,test_X,train_Y,k=K[optimal_k])


#confusion matrix
table(knn_pred,test_Y)

#missclassification error =  0.1564246
mean(test_Y!=knn_pred)

#false positive rate = 0.1212121
false_positive = 16/(116+16)

#attempt using ensemble decision tree

standardized_X = scale(life_expectancy_train[,c("Under_five_deaths","Infant_deaths","Hepatitis_B","BMI","Alcohol_consumption","Schooling","Incidents_HIV")])
standardized_X_test = scale(life_expectancy_test[,c("Under_five_deaths","Infant_deaths","Hepatitis_B","BMI","Alcohol_consumption","Schooling","Incidents_HIV")])

for(j in 1:11){
  k = K[j]
  for(i in 1:10){
    test_index = flds[[i]]
    testX = standardized_X[test_index,]
    trainX = standardized_X[-test_index,]
    
    trainY = life_expectancy_train$above_or_below_GDP[-test_index]
    testY = life_expectancy_train$above_or_below_GDP[test_index]
    
    knn.pred = knn(trainX,testX,trainY,k=k)
    cv_error[i,j] = mean(testY!=knn.pred)
  }
}

optimal_k=(which.min(apply(cv_error,2,mean)))
plot(K,apply(cv_error,2,mean),ylab = "average CV error")

train_X = standardized_X
test_X = standardized_X_test
train_Y = life_expectancy_train$above_or_below_GDP
test_Y = life_expectancy_test$above_or_below_GDP

knn_pred = knn(train_X,test_X,train_Y,k=K[optimal_k])

#confusion matrix
confusion_matrix=table(knn_pred,test_Y)
confusion_matrix
#missclassification error = 0.1564246
mean(test_Y!=knn_pred)

#false positive rate = 0.1323529
false_positive=18/(118+18)
