# Apriori

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)
#header = FALSE tells R the first line is not the name of the column
#each line is a transaction w/ a certain set of products

#we will transform the data set to mke 1 column for each product (120 products total)
#the lines will still be each transaction, but there will be 0 or 1 to
#signify if they bought the product or not
#this makes a SPARSE matrix, which has more 0's than anything else
#we use arules for this

dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

#Training Apriori model on dataset
rules = apriori(dataset, parameter = list(support = 0.004, confidence = 0.2))

#Visualising the Rules made by the algorithm
inspect(sort(rules, by = 'lift')[1:10])

#alter the support to consider product bought 3 or 4 times a day, 
#decrease confidence to find more novel rules


