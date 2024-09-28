# Load the necessary libraries
library(caTools)

# Load the Dogecoin dataset
dogecoin_data <- read.csv("E:/homework/Crypto Project/dogecoin.csv")

# Load the Shiba Inu dataset
shiba_inu_data <- read.csv("E:/homework/Crypto Project/shiba-inu.csv")

# Convert the 'date' column to Date type if needed
dogecoin_data$date <- as.Date(dogecoin_data$date)
shiba_inu_data$date <- as.Date(shiba_inu_data$date)

# Data Preprocessing:
# We'll Focus on 'price' as the target and 'total_volume' as a feature for regression
dogecoin_data <- na.omit(dogecoin_data[, c("price", "total_volume")])
shiba_inu_data <- na.omit(shiba_inu_data[, c("price", "total_volume")])

# Split the data into training and testing sets (80% train, 20% test)
set.seed(123)  # Set seed for reproducibility
split_doge <- sample.split(dogecoin_data$price, SplitRatio = 0.8)
train_doge <- subset(dogecoin_data, split_doge == TRUE)
test_doge <- subset(dogecoin_data, split_doge == FALSE)

split_shiba <- sample.split(shiba_inu_data$price, SplitRatio = 0.8)
train_shiba <- subset(shiba_inu_data, split_shiba == TRUE)
test_shiba <- subset(shiba_inu_data, split_shiba == FALSE)

# Build linear regression models
model_doge <- lm(price ~ total_volume, data = train_doge)
model_shiba <- lm(price ~ total_volume, data = train_shiba)

# Summary of the models
summary(model_doge)
summary(model_shiba)

# Make predictions on the test sets
pred_doge <- predict(model_doge, newdata = test_doge)
pred_shiba <- predict(model_shiba, newdata = test_shiba)

# Calculate Mean Squared Error (MSE) and R-squared for both models
mse_doge <- mean((test_doge$price - pred_doge)^2)
r2_doge <- 1 - (sum((test_doge$price - pred_doge)^2) / sum((test_doge$price - mean(test_doge$price))^2))

mse_shiba <- mean((test_shiba$price - pred_shiba)^2)
r2_shiba <- 1 - (sum((test_shiba$price - pred_shiba)^2) / sum((test_shiba$price - mean(test_shiba$price))^2))

# Print the evaluation metrics
cat("Dogecoin Model - MSE:", mse_doge, ", R-squared:", r2_doge, "\n")
cat("Shiba Inu Model - MSE:", mse_shiba, ", R-squared:", r2_shiba, "\n")

# Plot actual vs predicted for Dogecoin
plot(test_doge$total_volume, test_doge$price, col = "blue", pch = 20, 
     main = "Dogecoin Price Prediction vs Actual", xlab = "Total Volume", ylab = "Price")
points(test_doge$total_volume, pred_doge, col = "red", pch = 20)
legend("topleft", legend = c("Actual Price", "Predicted Price"), col = c("blue", "red"), pch = 20)

# Plot actual vs predicted for Shiba Inu
plot(test_shiba$total_volume, test_shiba$price, col = "blue", pch = 20, 
     main = "Shiba Inu Price Prediction vs Actual", xlab = "Total Volume", ylab = "Price")
points(test_shiba$total_volume, pred_shiba, col = "red", pch = 20)
legend("topleft", legend = c("Actual Price", "Predicted Price"), col = c("blue", "red"), pch = 20)

