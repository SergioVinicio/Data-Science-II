# Multiple Linear Regression
setwd("~/Tareas universidad/4 Semestre/Data Science II/Ejercicios/3 Ejercicio")
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

# Feature Scaling
# (not used here)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
datanueva<- data.frame(y_pred)

# Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend ,
               data = dataset)
summary(regressor)

library(ggplot2)
ggplot()+
  geom_point(aes(x=test_set$R.D.Spend, y=test_set$Profit),colour = 'red')+
  geom_point(aes(x=test_set$R.D.Spend, y=datanueva$y_pred))
