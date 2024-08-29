#SVR
dataset = Position_Salaries

dataset = dataset[2:3]
dataset

install.packages("e1071")
library(e1071)
library(ggplot2)
#Fitting SVR Model to the dataset
regressor = svm(formula = Salary ~., data = dataset, type = 'eps-regression')

#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

#Visualising the SVR Results
ggplot() +
  geom_point(aes(x = dataset$Level,y = dataset$Salary), 
             colour = 'red') + 
  geom_line(aes(x = dataset$Level,y = predict(regressor, newdata = dataset)), 
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR Model)') + 
  xlab('Level') +
  ylab('Salary')