#Apriori

#Data Preprocessing
install.packages("arules")
library(arules)
dataset = read.csv('Market_Basket_Optimisation.csv', header = F)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', 
                            rm.duplicates = T)

#there are 5 transactions with 1 duplicate
summary(dataset)
itemFrequencyPlot(dataset, topN=10)

#Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support = 0.003, 
                                                 confidence = 0.2))

#Visualising the results
inspect(sort(rules, by = 'lift')[1:10])