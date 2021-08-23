library(e1071)
library(caTools)
library(caret)

## Load Iris Dataset
data(iris)

## Pisahkan data menjadi training dan testing
sample <- sample.split(iris, 0.75)
data_train <- subset(iris, sample == TRUE)
data_test <- subset(iris, sample == FALSE)

## Lakukan feature scaling
data_train <- scale(data_train[, 1:5])
data_test <- scale(data_test[, 1:5])

## Buat model naive bayes
set.seed(100)

nb_classifier = naiveBayes(Species ~ ., data = data_train)
nb_classifier

## Lakukan prediksi pada data test

pred = predict(nb_classifier, newdata = data_test)

confusionMatrix(pred, data_test$Species)

