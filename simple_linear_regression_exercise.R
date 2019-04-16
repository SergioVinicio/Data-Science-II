# Simple Linear Regression

# Importing the dataset
# read 'Salary_Data.csv' into dataset

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
x <- set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# (not used here)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# Predicting the Test set results
# use y_pred to predict results from test set

# Visualising the Training set results
# install.packages('ggplot2')
# install.packages('Rcpp')
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
# use ggplot2 library
# plot the test set results
