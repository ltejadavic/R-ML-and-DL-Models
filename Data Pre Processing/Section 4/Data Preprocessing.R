dataset<- Data
dataset

#Dealing with missing data

dataset$Age = ifelse(is.na(dataset$Age), ave(dataset$Age, 
FUN = function(x) mean(x, na.rm = TRUE)), dataset$Age)

dataset$Salary = ifelse(is.na(dataset$Salary), ave(dataset$Salary, 
FUN = function(x) mean(x, na.rm = TRUE)), dataset$Salary)

dataset

#Encoding Categorical Data

dataset$Country = factor(dataset$Country, levels = c('France','Spain','Germany'),
labels = c(1,2,3))

dataset

dataset$Purchased = factor(dataset$Purchased, levels = c('No', 'Yes'),
labels = c(0,1))

dataset

#Splitting Datatset into training and test
install.packages("caTools")
library(caTools)

set.seed(123)
split=sample.split(Y = dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

training_set
test_set

#Feature Scaling
training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])