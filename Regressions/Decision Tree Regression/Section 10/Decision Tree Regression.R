#Decision Tree Regression

dataset = Position_Salaries

dataset = dataset[2:3]
dataset

install.packages('rpart')
library(rpart)

#Fitting SVR Model to the dataset
regressor = rpart(formula = Salary ~ ., data = dataset, 
                  control = rpart.control(minsplit = 1))
#In this case it is necessary to specify the minsplit cause there are to
#few data and the algotithm is tring to not do any split

#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

#Visualising the SVR Results
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')