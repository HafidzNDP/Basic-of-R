#Description : Linier regresion in R


# one variable
eruption_lm = lm(eruptions ~ waiting, data=faithful) 
eruption_stdres = rstandard(eruption_lm)
eruption_res = resid(eruption_lm)

# output interpretation
summary(eruption_lm)

# plotting the regression line
library(ggplot2)
ggplot(faithful, aes(x=waiting, y=eruptions)) +
  geom_point() +
  stat_smooth(method=lm) +
  labs(
    title = 'Regressing Eruptions using Waiting Time'
  )


# residual plot
# standard
plot(faithful$waiting, eruption_res, 
          ylab="Residuals", xlab="Waiting Time", 
          main="Old Faithful Eruptions") 
abline(0, 0)

# using ggplot
library(ggplot2)
eruption_res_df <- data.frame(waiting=faithful$waiting,
                              residual=eruption_res)

ggplot(eruption_res_df, aes(x=waiting, y=residual))+
  geom_point(alpha=0.5)+
  geom_hline(yintercept=0, color='blue')+
  labs(
    title='Residual Plot: Regressing Eruption'
  )


# normality of residuals
qqnorm(eruption_stdres, 
              ylab="Standardized Residuals", 
              xlab="Normal Scores", 
              main="Old Faithful Eruptions") 
qqline(eruption_stdres)


# multiple variable 
data = read.csv("Linear Regresion.css")

# split training and test data
# install.packages("caTools")
library(caTools)
set.seed(123)
sample = sample.split(data$admit_prob, SplitRatio = .80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

# modeling
admit_lm1 <- lm(admit_prob ~ ., data=train)
summary(admit_lm1)

# diagnostic studies

# residual plot
# multiple variable: residuals vs predicted values!
admit_lm1_res <- resid(admit_lm1)
# to check assumptions: linearity, constant variance, and independence
plot(predict(admit_lm1, train), admit_lm1_res, 
     ylab="Residuals", xlab="Fitted Values", 
     main="Admit Probability") 
abline(0, 0)

# exercise: draw using ggplot2!

# QQ plot
# normality of residuals
admit_lm1_stdres = rstandard(admit_lm1)
qqnorm(admit_lm1_stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Admit Probability") 
qqline(admit_lm1_stdres)

# VIF
#install.packages('car')
library(car)
vif(admit_lm1)

# model evaluation
# prediction
predicted <- predict(admit_lm1, test)
actual_pred <- data.frame(cbind(actual=test$admit_prob, predicted=predicted))

# MAE
MAE <- mean(abs(actual_pred$actual-actual_pred$predicted))
MAE

# MAPE
MAPE <- mean(abs((actual_pred$predicted - actual_pred$actual))/actual_pred$actual) 
MAPE


# pairplot + correlation analysis
library(psych)
pairs.panels(train, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
) # correlated features: gre, toefl, gpa. Choose: GPA

# dropping gre and toefl on train and test
library(dplyr)
drop_cols <- c('gre_score',
               'toefl_score')

train_nocorr <- train %>% select(-drop_cols)
test_nocorr <- test %>% select(-drop_cols)

# modeling
admit_lm2 <- lm(admit_prob ~ ., data=train_nocorr)
summary(admit_lm2)

# diagnostic studies

# residual plot
admit_lm2_res <- resid(admit_lm2)
# to check assumptions: linearity, constant variance, and independence
plot(predict(admit_lm2, train_nocorr), admit_lm2_res, 
     ylab="Residuals", xlab="Fitted Values", 
     main="Admit Probability") 
abline(0, 0)

# draw using ggplot2
res_df <- data.frame(fitted_value=predict(admit_lm2, train_nocorr), 
                     residual=resid(admit_lm2))

ggplot(res_df, aes(x=fitted_value, y=residual)) +
  geom_point() +
  geom_hline(yintercept=0)

# QQ plot
# normality of residuals
admit_lm2_stdres = rstandard(admit_lm2)
qqnorm(admit_lm2_stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Admit Probability") 
qqline(admit_lm2_stdres)

# VIF
vif(admit_lm2)

# model evaluation
# prediction
predicted <- predict(admit_lm2, test_nocorr)
actual_pred <- data.frame(cbind(actual=test_nocorr$admit_prob, predicted=predicted))

# MAE
MAE <- mean(abs(actual_pred$actual-actual_pred$predicted))
MAE

# MAPE
MAPE <- mean(abs((actual_pred$predicted - actual_pred$actual))/actual_pred$actual) 
MAPE
