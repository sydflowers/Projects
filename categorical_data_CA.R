# Data Preprocessing

# Importing the dataset
dataset = read.csv('Data.csv')

# Taking care of missing data, replacing it with the mean of the rest of the values in the column
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)

# Now the missing value in the Age column has been replaced by the average of that column
dataset$Salary = ifelse(is.na(dataset$Salary), 
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)), 
                        dataset$Salary)

# Now we have done the same thing for the missing value in the Salary column



# Encoding categorical data, often done to assign numerical values to text data
# Here we assign values to Country and Purchased columns
dataset$Country = factor(dataset$Country, 
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))

# Now Country names have been replaced with our variables
dataset$Purchased = factor(dataset$Purchased, 
                         levels = c('No', 'Yes'),
                         labels = c(0, 1))

# Splitting the dataset into Training and Test sets
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)

# Type split into the console and hit enter to see which data went where
# True means it went to the training, and False goes to the test set
# The 0.8 Split ratio means 80% of the data will go to training and 20% to test
# This is a good ratio, 50:50 is the worst ratio

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Now our new tables for our training and test sets are created

# Feature Scaling - Putting our values in the same range to ensure equal evaluation by the model
# The range of salaries is much greater than the range of ages
# Thus a difference in salary would be weighted more heavily by the model
# We can normalize or standardize our values to correct this

training_set = scale(training_set)
test_set = scale(test_set)

# If we run this, it tells us X must be numeric
# This is because we transformed Country and Purchased from text to factors
# A factor is not a numeric number in R
# We will exclude these columns from the feature scaling using an index[]
# The index specified to only use columns 2 and 3 (Age and Salary)

training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])

# Now Age and Salary have been converted from values between -1 and 1 
# Based on the original range of that column
