#Polynomial Regression

dataset = Position_Salaries

dataset = dataset[2:3]
dataset


#Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ ., data = dataset)
summary(lin_reg)

#Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ ., data = dataset)
summary(poly_reg)

#Visualising the Linear Regression Results
library(ggplot2)

ggplot() +
  geom_point(aes(x = dataset$Level,y = dataset$Salary), 
             colour = 'red') + 
  geom_line(aes(x = dataset$Level,y = predict(lin_reg, newdata = dataset)), 
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression') + 
  xlab('Level') +
  ylab('Salary')

#Visualising the Polynomial Regression Results

ggplot() +
  geom_point(aes(x = dataset$Level,y = dataset$Salary), 
             colour = 'red') + 
  geom_line(aes(x = dataset$Level,y = predict(poly_reg, newdata = dataset)), 
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression') + 
  xlab('Level') +
  ylab('Salary')

#Predicting a new result with Linear Regression
y_pred = predict(object = lin_reg, newdata = data.frame(Level = 6.5))
y_pred
#330378.8 

#Predicting a new result with Polynomial Regression
y_pred = predict(object = poly_reg, newdata = data.frame(Level = 6.5, 
                                                         Level2=6.5^2,
                                                         Level3=6.5^3,
                                                         Level4=6.5^4))
y_pred
#158862.5 very close as the employee said

