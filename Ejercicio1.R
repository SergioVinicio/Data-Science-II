setwd("~/Tareas universidad/4 Semestre/Data Science II/Ejercicios/1 Ejercicio")
library(readr)
dataset <- read_csv("~/Tareas universidad/4 Semestre/Data Science II/Ejercicios/1 Ejercicio/Salary_Data.csv")

library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)




regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

y_pred = predict(regressor, newdata = test_set)

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')


library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = test_set$YearsExperience, y = predict(regressor, newdata = test_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') +
  xlab('Years of experience') +
  ylab('Salary')

##EXTRA

m <- SharedData$new(mpg)

p1 <- ggplot(m, 
             aes(displ, fill = class)) + 
  geom_density()

p2 <- ggplot(m, 
             aes(displ, hwy, fill = class)) + 
  geom_point()

subplot(p1, p2) %>% 
  highlight("plotly_click") %>% 
  hide_legend()



