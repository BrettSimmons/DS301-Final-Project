#Load in training set
life_expectancy_train <- read_csv("train.csv")
life_expectancy_test <- read_csv("test.csv")

quantile(life_expectancy_train$GDP_per_capita, c(0,0.25,0.5,0.75,1))


#finding mean GDP for testing and training ------------------------------------#

#training - GDP
mean_GDP=mean(life_expectancy_train$GDP_per_capita)

above_or_below_GDP=ifelse(life_expectancy_train$GDP_per_capita>=mean_GDP,1,0)

life_expectancy_train['above_or_below_GDP']<-above_or_below_GDP

#testing -GDP
mean_GDP_test=mean(life_expectancy_test$GDP_per_capita)

above_or_below_GDP=ifelse(life_expectancy_test$GDP_per_capita>=mean_GDP_test,1,0)

life_expectancy_test['above_or_below_GDP']<-above_or_below_GDP


#finding mean life expectancy for testing and training ------------------------------------#

#training - life expectancy
mean_LifeExp=mean(life_expectancy_train$Life_expectancy)

#testing - life expectancy
mean_LifeExp_test=mean(life_expectancy_train$Life_expectancy)



#------------------------------------------------------------------------------#
#Analyzing Predictor Distributions - histogram and plot

#training
plot(life_expectancy_train$GDP_per_capita)
abline(h = mean_GDP, col = "blue")

hist(life_expectancy_train$GDP_per_capita)
abline(v = mean_GDP, col = "blue")

#testing
plot(life_expectancy_test$GDP_per_capita)
abline(h = mean_GDP_test, col = "blue")

hist(life_expectancy_test$GDP_per_capita)
abline(v = mean_GDP_test, col = "blue")


#------------------------------------------------------------------------------#
#Checking multicolinearity
library(car)

removeClassifier = life_expectancy_test[,-c(1,2)]
View(removeClassifier)

treeMo = lm(GDP_per_capita~.,data=removeClassifier)


x = model.matrix(GDP_per_capita~.,data=removeClassifier)
y = treeMo$GDP_per_capita

cor(x,y)


VIFTree = vif(treeMo)


#CREATING DECISION TREE CLASSIFIER --------------------------------------------#
library(MASS)
#install.packages('tree')
library(tree)


set.seed(23)

#removed GDP_per_capita because it has too much correlation with above_or_below_GDP
life = life_expectancy_train[-13]

life$above_or_below_GDP = as.factor(life$above_or_below_GDP)

tree.GDP = tree(above_or_below_GDP~.,data=life)

summary(tree.GDP)

tree.GDP

plot(tree.GDP)
text(tree.GDP,pretty=0)


test =  life_expectancy_train[-train,]
tree.pred = predict(tree.GDP, newdata=test)

Y.test = GDP[-train,"GDP_per_capita"]

mean((tree.pred - Y.test)^2)

#classifiers

tree.gini = tree(above_or_below_GDP~., split=c("gini"), data=life)

tree.deviance = tree(above_or_below_GDP~., split=c("deviance"), data=life)

summary(tree.GDP)
summary(tree.gini)
summary(tree.deviance)