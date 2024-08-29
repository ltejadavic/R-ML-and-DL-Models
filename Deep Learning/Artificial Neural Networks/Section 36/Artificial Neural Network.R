#Artificial Neural Network

setwd("~/Downloads/Machine Learning Course/Deep Learning/Artificial Neural Networks/Section 36")
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

#The package that we are using already recognise the output as a categorical
#so we don't need to encode target feature as a factor
# Encoding the target feature as factor

#In the case of gender and country we need to transform as numeric factors
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
#It is so important to apply feature scaling to a ann

#Feature Scaling
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

#Fittig ANN to the Training Set
install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)
# Transform the results into a factor 0,1 to train a binary model 
# Encoding the target feature as factor
training_set$Exited = factor(training_set$Exited, levels = c(0, 1))
test_set$Exited = factor(test_set$Exited, levels = c(0, 1))

classifier = h2o.deeplearning(y = 'Exited',
                              training_frame = as.h2o(training_set),
                              activation = 'Rectifier',
                              hidden = c(6, 6),
                              epochs = 100,
                              train_samples_per_iteration = -2)
#A good way to determine the number of hidden layers / neurons is calculating
#the average of the number of Independent Variables and number of output Neu
#10 and 1
#hidden = c(number of neurons in 1st layer, number of neurons in 2nd layer))
#the number of epochs means how many times the dataset should be iterated
#train_samples_per_iteration -2 Special values are 0: one epoch, 
#-1: all available data (e.g., replicated training data), 
#-2: automatic. Defaults to -2.
#thanks to this we don/t need to choose the size of batch

#Predicting the Test set results
prob_pred = h2o.predict(object = classifier, newdata = as.h2o(test_set[-11]))
prob_pred
y_pred = factor(as.vector(prob_pred[1]), levels = c(0, 1))
y_pred
test_set[,11]
#Making the Confusion Matrix
install.packages("caret")
library(caret)
confusionMatrix(factor(y_pred), factor(test_set$Exited), positive = '1')

h2o.shutdown()