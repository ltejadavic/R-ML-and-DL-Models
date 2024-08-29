#Random Forest Regression

dataset = Position_Salaries

dataset = dataset[2:3]
dataset

install.packages("randomForest")
library(randomForest)
set.seed(1234)

#Fitting Random Forest Regression Model to the dataset
regressor = randomForest(x = dataset[1], y = dataset$Salary, 
                         ntree = 500)

#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

#Visualising the Random Forest Regression Results
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')