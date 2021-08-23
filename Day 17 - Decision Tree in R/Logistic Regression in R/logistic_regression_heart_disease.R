install.packages('caret')
install.packages("cowplot")
install.packages("e1071")
library(ggplot2)
library(caret)
library(cowplot)
library(tidyverse)
library(caTools)
library(e1071)

## Import Dataset
library(readr)
data <- read_csv("heart.csv")

## Cek tipe data dalam setiap kolom
str(data)

## Ubah kolom sex sehingga 0 menjadi Female, 1 menjadi Male
data$sex <- ifelse(test=data$sex == 0, yes = "Female", no = "Male")
data$sex <- as.factor(data$sex)

## Ubah kolom sex, cp, fbs, restecg, exang, slope menjadi kategorikal
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

## Ubah kolom ca dan thal menjadi kategorikal
data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)

## ubah kolom target menjadi kategorikal
data$target <- as.factor(data$target)

#####################################
## Data Exploration

xtabs(~ target + sex, data=data)
xtabs(~ target + cp, data=data)
xtabs(~ target + fbs, data=data)
xtabs(~ target + restecg, data=data)
xtabs(~ target + exang, data=data)
xtabs(~ target + slope, data=data)
xtabs(~ target + ca, data=data)
xtabs(~ target + thal, data=data)

#####################################

## Split data menjadi training dan test
set.seed(100)
sample = sample.split(data$target, SplitRatio = 0.75)
data_train = subset(data, sample == TRUE)
data_test = subset(data, sample == FALSE)

## Buat logistic regression dengan training data
model_1 <- glm(target ~ sex, data=data_train, family = "binomial")
summary(model_1)

"""
Penjelasan:

Terdapat banyak metrik yang terlihat di bagian Summary.
Beberapa hal yang perlu diperhatikan untuk pemula:

- Intercept. Intercept = 1.1350 adalah nilai log(odds) jika input semuanya 0.
Dalam hal ini, karena model logistik kita hanya mengikutsertakan kolom 
jenis kelamin, maka Intercept = 1.1350 adalah log(odds) jika pasien
berjenis kelamin wanita (karena Sex = 0 artinya 'Female')

- Perhatikan skor AIC: 297.49. AIC ini menjadi pembanding seberapa
baik atau tidaknya model pada suatu dataset tertentu

(Catatan: yang bisa dibandingkan adalah nilai AIC dari model yang 
berasal dari dataset yang sama. Jika dataset berbeda, tidak dapat
membandingkan nilai AIC antarmodel. 

Nilai AIC yang semakin rendah menandakan model lebih baik.)
"""

## Membuat Confusion Matrix bagi logistic regression model pertama

pdata_1 <- predict(model_1, newdata = data_test, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata_1>0.5)), reference = as.factor(data_test$target))

"""
Dapat kita lihat, bahwa ketika model digunakan untuk memprediksi
data testing, akurasinya tidak terlalu baik (hanya 58%). 

Mari kita buat model lain dan bandingkan akurasinya.
"""

## Model 2: Model dengan seluruh variabel

model_2 <- glm(target ~ ., data = data_train, family = "binomial")
summary(model_2)

"""
Ketika banyak variabel yang dijadikan input, maka model akan belajar
'lebih dalam'. AIC pada model kedua ini menurun menjadi 176.16, dan sekilas,
kita melihat bahwa model ini lebih baik. Mari kita coba lihat confusion Matrix nya.
"""

pdata_2 <- predict(model_2, newdata = data_test, type = 'response')
confusionMatrix(data = as.factor(as.numeric(pdata_2 > 0.4)), reference = as.factor(data_test$target))

"""
Confusion Matrix dari model kedua ini menunjukkan bahwa model
mendapatkan akurasi di atas 80% pada data test, yaitu data-data
yang belum pernah 'dipelajari' sebelumnya
"""

## Visualisasi kurva logistik regression

predicted.data <- data.frame(
  probability.of.heartdisease = model_2$fitted.values,
  heart_disease = data_train$target
)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.heartdisease, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.heartdisease)) +
  geom_point(aes(color=heart_disease), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted Probability of Heart Disease")

