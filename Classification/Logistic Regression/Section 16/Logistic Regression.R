#Logistic Regression

setwd("~/Downloads/Machine Learning Course/Classification/Logistic Regression/Section 16")
dataset = read.csv('Social_Network_Ads.csv')

#Spliting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split=sample.split(Y = dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

#Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

#Fittig Logistic Regression to the Training Set
classifier =glm(formula = Purchased ~., family = binomial, data = training_set)

#Predicting the Test set results
prob_pred = predict(object = classifier, 
                    type = 'response', newdata = test_set[-3])
#Nergative indexing on a vector is to drop an entire column
prob_pred
y_pred = ifelse(prob_pred> 0.5, 1, 0)
y_pred
test_set[,3]

#Making the Confusion Matrix
install.packages("caret")
library(caret)

confusionMatrix(factor(y_pred), factor(test_set$Purchased), positive = '1')
cm = confusionMatrix(factor(y_pred), factor(test_set$Purchased), positive = '1')

#visualizing the Training Set Results
install.packages('ElemStatLearn')

par(mfrow=c(1,2))

library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))