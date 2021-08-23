library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)

data(iris)
iris = data.frame(iris)

## Pisahkan data menjadi training dan testing
set.seed(100)
sample <- sample.split(iris, 0.75)
data_train <- subset(iris, sample == TRUE)
data_test <- subset(iris, sample == FALSE)

## Decision Tree
iris_model <- rpart(formula = Species ~ .,
                    data = data_train, 
                    method = 'class')

pred <- predict(iris_model, newdata = data_test, type = 'class')
confusionMatrix(data = pred, reference = data_test$Species)

## Visualize the Tree
fancyRpartPlot(iris_model, main = "Iris")

