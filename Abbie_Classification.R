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
K= c(1,3,5,7)

cv_error = matrix(NA, 10, 4)

for(j in 1:4){
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


