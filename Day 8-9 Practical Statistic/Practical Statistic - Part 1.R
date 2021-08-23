#load sample data
library(datasets)

iris

#sample head of the data
head(iris)


#simple descriptive
summary(iris)


#holistic table plot
plot(iris)


#example of plot
plot(iris$Petal.Length, iris$Sepal.Width,
     col = 'red',
     pch = 20,
     xlab = 'Petal Length')
iris

plot(iris$Species, iris$Sepal.Length)


#Histogram
summary(iris)
hist(iris$Petal.Length) #outlier
hist(iris$Sepal.Length)


#New Data
Default_Fin

summary(Default_Fin)

hist(Default_Fin$`Bank Balance`)


