#XGBoost
#Importing Dataset
setwd("~/Downloads/Machine Learning Course/Deep Learning/Artificial Neural Networks/Section 36")
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

#Encoding the categorical  variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

#Spliting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split=sample.split(Y = dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

#feature scaling is totally unnecessary for xgboost

#Fitting XGBoostto the Training set
#install.packages("xgboost")
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-11]), 
                     label = training_set$Exited, 
                     nrounds = 10)

#Aplying k-Fold Cross Validation
library(caret)
folds = createFolds(training_set$Exited, k = 10)
cv = lapply(X = folds, FUN = function(x){
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-11]), 
                       label = training_set$Exited, 
                       nrounds = 10)
  y_pred = predict(object = classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1] + cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))