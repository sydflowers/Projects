# Multiple Linear Regression
# In this example we look at spending for R&D, Marketing, and Administration to predict how our dependent variable Profit changes
# This could be a useful analysis for a firm to decide what companies to invest in
# For the multiple variables we will use multiple linear regression


# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'), 
                       labels = c(1, 2, 3))


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling (We skip because our regressor function does this for us)
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
# regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
# ^This formula says Profit is a linear combination of all independent variables, and can be written as: 
# regressor = lm(formula = Profit ~ .)
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Now type summary(regressor) in the console down there v
# R has ranked the ind. variables by lowest P, so highest significance
# State 2 and State 3 are the dummy variables, the caTools made them automatically for backward elimination
# It knew to do this with the State column b/c we encoded it as a factor
# It even removed the one dummy variable to avoid the trap

# Reminder - P lower than 5% is significant

# We see R&D Spend has the highest significance. The rest have no relevance
# So we could make this a simple linear regression with just 1 variable and get the same predictions

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# ^y_pred is our vector of predicted results
# To view it, type y_pred below v 
# We see the predicted profit for the 10 test set budgets
# They look pretty close! 

# Building the optimal model with Backward Elimination
# Above we used all the variables. We can use BE to find the best predictors among our variables
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
               data = dataset)
summary(regressor)

# Steps of BE: 
# 1. Pick a significance level (5%)
# 2. Fit full model with all possible predictors
# 3. If highest P value is > SL (5%) remove it, otherwise FIN
# 4. Fit model without this variable
# 5. Repeat

# Step 3: Remove State 3 and State 2 since P > 5% (0.05)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
               data = dataset)
summary(regressor)

# Remove Administration since it's the highest and P > 0.05
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, 
               data = dataset)
summary(regressor)

# We see Marketing.Spend is very close to our significance level of 5%
# Remove Marketing.Spend since P > 0.05
regressor = lm(formula = Profit ~ R.D.Spend, 
               data = dataset)
summary(regressor)

# We are left with only R&D Spend, since it's P < 0.05, this is our model


# Automatic implementation of Backward Elimination can also be done:
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)

#NOTE: Adjusted R-squared for Regression Models
# R Squared will increase if you keep adding variables, even if those variables don't improve the model
# So we use Adjusted R-squared, it penalizes you for unnecessary variables so all variables are justified
# If we look again at these^ models' adjusted R squared we can see the best model
# has both R&D Spend and Marketing

#NOTE: Interpreting Linear Regression Coefficients: 
# Positive values under "Estimate" mean the ind. and dep. variables are directly related (rather than inversely)
# Meaning if you increase x, y will also increase
# If the value under "Estimate" for x1 is greater than x2, you know the impact on the dep. variable is greater per unit of x1 then per unit of x2
# Know that x1 may be in dollars and x2 is in thousands of dollars, be careful not to directly compare them
# For every 1 unit of x1 you increase, the dep. variable will also increase by the "Estimate" value in units of the dep. variable (in a direct relationship)
# If you decrease x1 by 1 unit the dep. will decrease by the "Estimate" value (we also call this the coefficient)
