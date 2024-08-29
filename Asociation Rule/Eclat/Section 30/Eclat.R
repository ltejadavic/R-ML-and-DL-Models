#Eclat

#Data Preprocessing
library(arules)
dataset = read.csv('Market_Basket_Optimisation.csv', header = F)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', 
                            rm.duplicates = T)

#there are 5 transactions with 1 duplicate
summary(dataset)
itemFrequencyPlot(dataset, topN=10)

#Training Eclat on the dataset
rules = eclat(data = dataset, parameter = list(support = 0.004, minlen = 2))

#Visualising the results
inspect(sort(rules, by = 'support')[1:10])