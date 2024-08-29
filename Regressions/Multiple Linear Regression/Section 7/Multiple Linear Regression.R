#Multiple Linear Regression

dataset = X50_Startups
dataset

#Encoding Categorical Data

dataset$State = factor(dataset$State, 
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

#Spliting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split=sample.split(Y = dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

#Fitting Multiple Linear Regression to the Training set
#"." represent the linear combination of all linear variables
#in this case R.D.Spend + Administration + Marketing.Spend + State
regressor = lm(formula = Profit ~ ., data = training_set)

summary(regressor)

#R.D.Spend has more statistical significance for the dependent variable (Profit)
#R.D.Spend has a strong effect on profit
#you can see that by the ***, the other independent variable doesn't has
#that kind of significance

#We could transform this multiple linear regression into a simple linear

#Predicting the Test Results
y_pred = predict(regressor, newdata = test_set)

#For an automated Backward elimination
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)
