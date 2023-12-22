# Eclat

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

#Training Eclat model on dataset
rules = eclat(dataset, parameter = list(support = 0.004, minlen = 2))

#we use minlen = 2 so that it only looks at baskets with 2 or more items
#in console we see # of sets, not # of rules

#Visualising the results (Sort subsets by decreasing support)
inspect(sort(rules, by = 'support')[1:10])

#we get sets of items most frequently purchased together
#closely correlates to the most frequently purchased items
