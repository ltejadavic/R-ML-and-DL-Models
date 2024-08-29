#Simple Linear Regression

dataset = Salary_Data
dataset

library(caTools)

set.seed(123)
split=sample.split(Y = dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)

training_set
test_set

#Fitting Simple Linear Regression
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)
summary(regressor)

#Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

#Visualization
install.packages("ggplot2")
library(ggplot2)

ggplot() + 
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), 
             colour = 'red') + 
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)), 
            colour = 'blue') +
ggtitle('Salary vs Experience (Training Set)') + xlab('Years of experience') + ylab('Salary') 