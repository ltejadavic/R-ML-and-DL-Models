#LDA
setwd("~/Downloads/Machine Learning Course/Dimensionality Reduction/Linear Discriminant Analysis/Section 40")
dataset = read.csv('Wine.csv')

#Spliting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split=sample.split(Y = dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

#Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

#Applying LDA
library(MASS)
lda = lda(formula = Customer_Segment ~ ., data = training_set)
training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(5, 6, 1)]
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(5, 6, 1)]
#k-1 linear discriminant, where k is the number of clases
#number of classes is 3 so linear discriminants is 2
#posterior are information but no importan for the prediction

#Fitting SVM to the Training set and Predicting Results
library(e1071)
classifier = svm(formula = class ~ ., data = training_set, 
                 type = 'C-classification', kernel = 'linear')

#Predicting the Test set result
y_pred = predict(object = classifier, newdata = test_set[-3])

#Making the Confusion Matrix
library(caret)
confusionMatrix(factor(y_pred), factor(test_set$class), positive = '1')
cm = confusionMatrix(factor(y_pred), factor(test_set$class), positive = '1')

library(ElemStatLearn)
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'x.LD1', ylab = 'x.LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'purple', ifelse(y_grid == 1, 'dodgerblue', 'salmon')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'purple', ifelse(set[, 3] == 1, 'dodgerblue', 'salmon')))

# Visualising the Test set results
set = test_set 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(object = classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'x.LD1', ylab = 'x.LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'purple', ifelse(y_grid == 1, 'dodgerblue', 'salmon')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'purple', ifelse(set[, 3] == 1, 'dodgerblue', 'salmon')))