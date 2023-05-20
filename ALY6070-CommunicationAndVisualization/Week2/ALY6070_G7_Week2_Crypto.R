my_packages = c("plyr", "plotly", "ggplot2", "psych", "tidyr", "tidyverse","dplyr","lubridate","readr","caret","caTools","glmnet","randomForest")
#install.packages(my_packages)
lapply(my_packages, require, character.only = T)

crypto_data <- read.csv("/Users/abidikshit/R_Projects/Data/crypto.csv", header = T)
cat("Number of Rows before cleanup:", nrow(crypto_data), "\n") # Printing string and variable row count on the same line
cat("Number of Columns before cleanup:", ncol(crypto_data), "\n") 
cat("Blank cells count before cleanup:", sum(!complete.cases(crypto_data))) # Displaying Blank Cells Count for uncleaned data

crypto_data$Date <- as.Date(crypto_data$Date)

headTail(crypto_data, top = 4, bottom = 4, ellipsis = F)

summary(crypto_data)
str(crypto_data)

class <- crypto_data$Class
predictors <- crypto_data[, -ncol(crypto_data)]

set.seed(123)
train <- sample(nrow(crypto_data), nrow(crypto_data) * 0.8)
crypto_train <- crypto_data[train, ]
crypto_test <- crypto_data[-train, ]

headTail(crypto_train, top = 4, bottom = 4, ellipsis = F)
headTail(crypto_test, top = 4, bottom = 4, ellipsis = F)

# Perform Lasso
x_train <- model.matrix(Close ~ ., data = crypto_train)[,-1]
y_train <- crypto_train$Close

x_test <- model.matrix(Close ~ ., data = crypto_test)[,-1]
y_test <- crypto_test$Close

fit <- glmnet(x_train, y_train, alpha = 1)

plot(fit, xvar = "lambda", label = TRUE)

cv_fit <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- cv_fit$lambda.min

predictions <- predict(fit, newx = x_test, s = best_lambda)

summary(predictions)

plot(y_test, predictions, pch = 20, cex = 0.7, xlab = "Actual Close Price", ylab = "Predicted Close Price")
abline(a = 0, b = 1, col = "red")

# Perform Random Forest
rf <- randomForest(Close ~ ., data = crypto_train)

varImpPlot(rf, main = "Variable Importance Plot", type = 2, col = "coral")

## NA
