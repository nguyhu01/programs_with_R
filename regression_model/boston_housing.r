install.packages("mlbench")

#--- Load required libraries
library(ggplot2)
library(caret)
library(glmnet)
library(mlbench)

data(BostonHousing)

#--- Load the Boston Housing dataset
data(BostonHousing)
housing_data <- BostonHousing

#--- Perform EDA
summary(housing_data)
pairs(housing_data)

#--- Split the data
set.seed(123)
train_index <- createDataPartition(housing_data$price, p = 0.7, list = FALSE)
train_data <- housing_data[train_index, ]
test_data <- housing_data[-train_index, ]

#--- Fit a multiple linear regression model
model <- lm(price ~ ., data = train_data)

#--- Print the model summary
summary(model)

#--- Perform feature selection using stepwise regression
step_model <- step(model, direction = "both")
summary(step_model)

#--- Apply regularization using Lasso (L1)
x <- model.matrix(price ~ ., train_data)[, -1]
y <- train_data$price
cv_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_model)

#--- Make predictions on the test set
test_predictions <- predict(step_model, newdata = test_data)

#--- Evaluate model
mse <- mean((test_data$price - test_predictions)^2)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

#--- Visualize the relationship between variables
ggplot(train_data, aes(x = floor_area, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Floor Area", y = "Price", title = "Floor Area vs. Price")
