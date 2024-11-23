library(kernlab)
library(ROCR)
library(neuralnet)
library(GGally)
library(class)
library(e1071)
library(nnet)
library(ISLR)
library(tidyverse)
library(funModeling)
library(caret)
library(gbm)
library(xgboost)
library(DiagrammeR)
library(rpart)
library(mlbench)
library(cli)
library(tree)
library(rpart.plot)
library(randomForest)
library(pROC)

# classification models

#logistic regression

df1 <- Default

glimpse(df1)
plot_num(df1)
freq(df1)

education_index <-  createDataPartition(df1$default, p= 0.8, list = FALSE, times = 1)
education <- df1[education_index,]
test <- education <- df1[-education_index,]


education_x <- education %>% dplyr::select(-default) #bağımsız değişkenler
education_y <- education$default #bağımlı değişken

test_x <- test %>% dplyr::select(-default) #bağımsız değişkenler
test_y <- test$default #bağımlı değişken


as.numeric(education$default) - 1

lr_model <- glm(default~.,
                data = education,
                family = "binomial")

summary(lr_model)
predict(lr_model, type = "response") #response (-'den kurtarıyoruz)

lr_model_guess <- ifelse(predict(lr_model, type = "response") > 0.5, "Yes", "No")
head(lr_model_guess)

calculate_error <- function(real, guess) {
  mean(real!=guess)
}
calculate_error(education$default, lr_model_guess)
1 - calculate_error(education$default, lr_model_guess) #doğru tahmin oranı

lr_table <- table(
  guess = lr_model_guess,
  real = education$default
)
lr_table


confusionMatrix(lr_table, positive = "Yes")

plot(as.numeric(education$default) -1~balance,data=education, ylim=c(-0.3,1), col="red", pch="I")
abline(h=0, lty=3)
abline(h=0.5, lty=3)
abline(h=1, lty=3)

lr_model <- glm(default~balance,
                data = education,
                family = "binomial")

curve(predict(lr_model, data.frame(balance=x), type = "response"), add = TRUE, lwd = 3, col="blue")


lr_model <- glm(default~.,
                data = education,
                family = "binomial")
lr_test_guess <- predict(lr_model,newdata = test_x, type = "response")
roc(test_y~lr_test_guess, plot = TRUE, print.auc = TRUE)


lr_control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

set.seed(3232)

lr_model_tuning <- train(
  education_x, education_y,
  method = "glm",
  trControl = lr_control
)

lr_model_tuning
defaultSummary(data.frame(
  obs = test_y,
  pred = predict(lr_model_tuning, test_x)
))

