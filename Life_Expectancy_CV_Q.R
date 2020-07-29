library(pacman)
pacman::p_load(readr, caret, tidyverse, dplyr)

# Get and set working directory
getwd()
setwd("C:/Users/Chris/Projects")

# Read in the data
led <-read_csv("./StackQ/LifeExpectancyData.csv")
# Check for NAs
sum(is.na(led))
# Set all NAs to 0
led[is.na(led)] <- 0
# Rename `Life expectancy` to life_exp to avoid using spaces
led <-led %>% rename(life_exp = `Life expectancy`)
# Partition training and test sets
set.seed(123, sample.kind = "Rounding")
test_index <- createDataPartition(y = led$life_exp, times = 1, p = 0.2, list = F)
led_train <- led[-test_index,]
led_test <- led[test_index,]
# Add RMSE as unit of error measurement
RMSE <-function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
# Create a linear model
led_lm <- lm(life_exp ~ GDP, data = led_train)
# Create prediction
lm_preds <-predict(led_lm, led_test)
# Check RMSE
RMSE(led_test$life_exp, lm_preds)
# The linear Model achieves an RMSE of 9.81141

# Create a Random Forest Model with Repeated Cross Validation
led_cv <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                      search = "random")
# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_rf <- train(life_exp ~ GDP, data = led_train,
                  method = "rf", ntree = 150, trControl = led_cv,
                  tuneLength = 5, nSamp = 1000, 
                  preProcess = c("center","scale"))
# Create Prediction
rf_preds <-predict(train_rf, led_test)
# Check RMSE
RMSE(led_test$life_exp, rf_preds)
# The rf Model achieves an RMSE of 9.828415

# kNN Model:
knn_cv <-trainControl(method = "repeatedcv", repeats = 1)
# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_knn <- train(life_exp ~ GDP, method = "knn", data = led_train,
                   tuneLength = 10, trControl = knn_cv,
                   preProcess = c("center","scale"))
# Create the Prediction:
knn_preds <-predict(train_knn, led_test)
# Check the RMSE:
RMSE(led_test$life_exp, knn_preds)
# The kNN model achieves the lowest RMSE of 8.923281

# kNN Final Model:
knn_cv <-trainControl(method = "repeatedcv", repeats = 1)
# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_knn <- train(life_exp ~ GDP, method = "knn", data = led,
                   tuneLength = 10, trControl = knn_cv,
                   preProcess = c("center","scale"))
# Create the Prediction:
knn_preds <-predict(train_knn, led_test)
# Check the RMSE:
RMSE(led_test$life_exp, knn_preds)
