library(dplyr)
#calculate variance single variable
var(iris$Sepal.Length)

sqrt(var(iris$Sepal.Length))

var(iris$Petal.Length)

summary(iris)

#calculate variance & covariance
var(iris)

plot(iris)

#calculate correlation
cor(iris) # error

iris[, c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')]

cor(iris%>%select(-Species)) # select column

plot(iris)
ve <- c(10:100)
sample(ve,5)
qnorm(0.80,lower.tail = T)
pnorm(0.8416212,lower.tail = T)
#other example
var(Default_Fin)

cor(Default_Fin)

plot(Default_Fin)

