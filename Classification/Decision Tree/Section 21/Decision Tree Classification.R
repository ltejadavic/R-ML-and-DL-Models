#Decision Tree Classification
dataset = Social_Network_Ads
dataset

#Encoding the target feature as factor
dataset$Purchased = factor(x = dataset$Purchased, levels = c(0,1))

#Spliting the dataset into the Training set and Test set
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(Y = dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

#Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

#Fitting Naive Decision Tree to the Training set and Predicting Results
library(rpart)
classifier = rpart(formula = Purchased ~ ., data = training_set)


#Predicting the Test set result
y_pred = predict(object = classifier, newdata = test_set[-3], type = 'class')

#Making the Confusion Matrix
#install.packages("caret")
library(caret)
confusionMatrix(factor(y_pred), factor(test_set$Purchased), positive = '1')
cm = confusionMatrix(factor(y_pred), factor(test_set$Purchased), positive = '1')

# Visualising the Training set results
#installed by archive
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(object = classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(object = classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))
