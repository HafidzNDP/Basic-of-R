# Description: Penalized linear regression in R
# Author: Pararawendy Indarjo
# Date: 2021-07-03

# import data
data <- read.csv("Linear Regresion.css")


# split training and test data
library(caTools)
set.seed(123)
sample <- sample.split(data$admit_prob, SplitRatio = .80)
train <- subset(data, sample == TRUE)
test <- subset(data, sample == FALSE)

# correlation study
library(psych)
pairs.panels(train, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
) # correlated features: gre, toefl, gpa. Choose: GPA

# drop correlated columns
library(dplyr)
drop_cols <- c('gre_score',
               'toefl_score')

train <- train %>% select(-drop_cols)
test <- test %>% select(-drop_cols)

# feature preprocessing
# to ensure we handle categorical features
x <- model.matrix(admit_prob ~ ., train)[,-1]
y <-  train$admit_prob

# Ridge Regression with lambda = 0.1
# fit ridge regression (alpha = 0)
library(glmnet)
ridge_reg <- glmnet(x, y, alpha = 0, lambda = 0.1)
coef(ridge_reg)

# evaluation
# Make predictions on the test data
x_test <- model.matrix(admit_prob ~., test)[,-1]
y_test <- test$admit_prob

predictions <- predict(ridge_reg, x_test)

# Rsquared <-  cor(true_values, predicted_values)^2
# on training data
R2 <- as.numeric(cor(y, predict(ridge_reg, x)) ^ 2)
R2

# RMSE <- sqrt(mean((true_values - predicted_values)^2))
# on test data
RMSE <- sqrt(mean((y_test - predict(ridge_reg, x_test))^2))
RMSE

# LASSO with lambda = 0.1
# fit LASSO (alpha = 1)
lasso_reg <- glmnet(x, y, alpha = 1, lambda = 0.1)
coef(lasso_reg)

# REVISED workflow
# further split train data to train and validation
set.seed(123)
sample <- sample.split(data$admit_prob, SplitRatio = .80)
pre_train <- subset(data, sample == TRUE)
sample_train <- sample.split(pre_train$admit_prob, SplitRatio = .80)

# train-validation data
train <- subset(pre_train, sample_train == TRUE)
validation <- subset(pre_train, sample_train == FALSE)

# test data
test <- subset(data, sample == FALSE)

# drop correlated columns
drop_cols <- c('gre_score',
               'toefl_score')

train <- train %>% select(-drop_cols)
validation <-  validation %>% select(-drop_cols)
test <- test %>% select(-drop_cols)

# feature preprocessing
# to ensure we handle categorical features
x <- model.matrix(admit_prob ~ ., train)[,-1]
y <-  train$admit_prob

# ridge regression
# fit multiple ridge regression with different lambda
# lambda = [0.01, 0.1, 1, 10]
ridge_reg_pointzeroone <- glmnet(x, y, alpha = 0, lambda = 0.01)
coef(ridge_reg_pointzeroone)

ridge_reg_pointone <- glmnet(x, y, alpha = 0, lambda = 0.1)
coef(ridge_reg_pointone)

ridge_reg_one <- glmnet(x, y, alpha = 0, lambda = 1)
coef(ridge_reg_pointone)

ridge_reg_ten <- glmnet(x, y, alpha = 0, lambda = 10)
coef(ridge_reg_ten)

# comparison on validation data
# to choose the best lambda

# Make predictions on the validation data
x_validation <- model.matrix(admit_prob ~., validation)[,-1]
y_validation <- validation$admit_prob

RMSE_ridge_pointzeroone <- sqrt(mean((y_validation - predict(ridge_reg_pointzeroone, x_validation))^2))
RMSE_ridge_pointzeroone # 0.05931718 --> best

RMSE_ridge_pointone <- sqrt(mean((y_validation - predict(ridge_reg_pointone, x_validation))^2))
RMSE_ridge_pointone # 0.06427553 

RMSE_ridge_one <- sqrt(mean((y_validation - predict(ridge_reg_one, x_validation))^2))
RMSE_ridge_one # 0.1012343

RMSE_ridge_ten <- sqrt(mean((y_validation - predict(ridge_reg_ten, x_validation))^2))
RMSE_ridge_ten # 0.1283507

# true evaluation on test data
# using the best model --> RMSE_ridge_pointzeroone
x_test <- model.matrix(admit_prob ~., test)[,-1]
y_test <- test$admit_prob

# RMSE
RMSE_ridge_best <- sqrt(mean((y_test - predict(ridge_reg_pointzeroone, x_test))^2))
RMSE_ridge_best # 0.06423617

# MAE
MAE_ridge_best <- mean(abs(y_test-predict(ridge_reg_pointzeroone, x_test)))
MAE_ridge_best

# MAPE
MAPE_ridge_best <- mean(abs((predict(ridge_reg_pointzeroone, x_test) - y_test))/y_test) 
MAPE_ridge_best

############## LASSO
# lasso regression
# fit multiple lasso regression with different lambda
# lambda = [0.01, 0.1, 1, 10]
lasso_reg_pointzeroone <- glmnet(x, y, alpha = 1, lambda = 0.01)
coef(lasso_reg_pointzeroone) 

lasso_reg_pointone <- glmnet(x, y, alpha = 1, lambda = 0.1)
coef(lasso_reg_pointone) 

lasso_reg_one <- glmnet(x, y, alpha = 1, lambda = 1)
coef(lasso_reg_pointone)

lasso_reg_ten <- glmnet(x, y, alpha = 1, lambda = 10)
coef(lasso_reg_ten)

# comparison on validation data
# to choose the best lambda
# Make predictions on the validation data
RMSE_lasso_pointzeroone <- sqrt(mean((y_validation - predict(lasso_reg_pointzeroone, x_validation))^2))
RMSE_lasso_pointzeroone # 0.06016375 --> best

RMSE_lasso_pointone <- sqrt(mean((y_validation - predict(lasso_reg_pointone, x_validation))^2))
RMSE_lasso_pointone # 0.1095918 

RMSE_lasso_one <- sqrt(mean((y_validation - predict(lasso_reg_one, x_validation))^2))
RMSE_lasso_one # 0.1330481

RMSE_lasso_ten <- sqrt(mean((y_validation - predict(lasso_reg_ten, x_validation))^2))
RMSE_lasso_ten # 0.1330481

# true evaluation on test data
# using the best model --> RMSE_lasso_pointone
RMSE_lasso_best <- sqrt(mean((y_test - predict(lasso_reg_pointzeroone, x_test))^2))
RMSE_lasso_best # 0.0642024

