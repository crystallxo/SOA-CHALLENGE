library(mgcv)
library(ggplot2)
library(performance)
library(dplyr)
library(caTools)  # For train-test split

# Load data
df = read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv")

# Remove missing values
missing_values <- colSums(is.na(df))
df <- na.omit(df)  # Remove all rows with NA

# Create total loss variable
df$Total_Loss <- df$Loss_gf_Prop + df$Loss_gf_Liab + df$Loss_gf_BI

# Convert categorical variables
df$Region <- as.numeric(factor(df$Region))
df <- df %>% filter(Region == "1") # 1 for Flumevale, 2 for Lyndrassia, 3 for Navaldia
df$Region <- as.factor(df$Region)
df$Purpose <- as.factor(df$Purpose)
df$Hazard <- as.factor(df$Hazard)

# Split data into 80% training and 20% testing
set.seed(123)  # For reproducibility
split <- sample.split(df$Total_Loss, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

# Fit GAM model
gam_model <- gam(Total_Loss ~ 
                   s(Surface, k=20) +  # Increase flexibility
                   s(Distance_to_City, k=25),
                 data = train_data, 
                 family = gaussian(link = "identity"))

# Model summary and diagnostics
summary(gam_model)
gam.check(gam_model)

# Plot model
ggplot(train_data, aes(x = Surface, y = predict(gam_model, newdata = train_data))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Effect of Surface on Total Loss",
       x = "Surface Area",
       y = "Predicted Total Loss")

ggplot(train_data, aes(x = Distance_to_City, y = predict(gam_model, newdata = train_data))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Effect of Distance to City on Total Loss",
       x = "Distance to City",
       y = "Predicted Total Loss")

# Predictions on test data
predictions <- predict(gam_model, newdata = test_data, type = "response")

# Calculate MAE on test data
mae <- mean(abs(test_data$Total_Loss - predictions))
print(paste("Mean Absolute Error of GAM on test data:", round(mae, 2)))

# Actual vs. Predicted plot
ggplot(test_data, aes(x = Total_Loss, y = predictions)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Loss (GAM) - Test Set",
       x = "Actual Total Loss",
       y = "Predicted Total Loss")




# Load necessary library
library(Metrics)  # For RMSE
library(performance)  # For R²

# Predictions on training and test data
train_predictions <- predict(gam_model, newdata = train_data, type = "response")
test_predictions <- predict(gam_model, newdata = test_data, type = "response")

# RMSE Calculation
train_rmse <- rmse(train_data$Total_Loss, train_predictions)
test_rmse <- rmse(test_data$Total_Loss, test_predictions)

# R-squared Calculation
r_squared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  return (1 - (ss_residual / ss_total))
}
train_r2 <- r_squared(train_data$Total_Loss, train_predictions)
test_r2 <- r_squared(test_data$Total_Loss, test_predictions)

# Print results
print(paste("Training RMSE:", round(train_rmse, 2)))
print(paste("Test RMSE:", round(test_rmse, 2)))
print(paste("Training R²:", round(train_r2, 4)))
print(paste("Test R²:", round(test_r2, 4)))
