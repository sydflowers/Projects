# Simple Linear Regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# We want to see if Salary has a linear relationship with YearsExperience
# Independent variable is YearsExp, dependent is Salary

# Splitting the dataset into the Training set and Test set using our pre-pro template
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)
# Our simple linear regression model does not require feature scaling

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# ~ means Salary is proportional to YearsExp
# Now we can type summary(regressor) into the console and we get stats on our fit
# The P value of YearsExperience is very small (lowhttp://127.0.0.1:30633/graphics/plot_zoom_png?width=918&height=825er than 5%) so it's very significant
# The 3 *'s also show it's highly impactful on the dependent variable (Salary)


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
# newdata contains the data we want to predict results for, so we set it = to test_set
# now we can type y_pred in the console to see what was predicted for our test values


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of Experience') +
  ylab('Salary')
# This will plot points for real Training Set data and line for predicted training set data
# xlab and ylab name the axes



# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')