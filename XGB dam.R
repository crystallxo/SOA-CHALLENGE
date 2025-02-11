library(xgboost)
library(caret)
library(dplyr)
library(caTools)

# Load dataset
df <- read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv")

# Remove missing values
df <- na.omit(df)  

# Create target variable
df$Total_Loss <- df$Loss_gf_Prop + df$Loss_gf_Liab + df$Loss_gf_BI

# Convert categorical variables
df$Region <- as.numeric(factor(df$Region))
df <- df %>% filter(Region == 1) # 1 for Flumevale, 2 for Lyndrassia, 3 for Navaldia

df$Purpose <- as.numeric(factor(df$Purpose))
df$Hazard <- as.numeric(factor(df$Hazard))

# Select relevant features
features <- c("Surface", "Distance_to_City", "Purpose", "Hazard")
target <- "Total_Loss"
df <- df[, c(features, target)]

# Train-test split (80% train, 20% test)
set.seed(123)  
split <- sample.split(df$Total_Loss, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

# Convert to matrix format (XGBoost requires matrices)
train_matrix <- as.matrix(train_data[, features])
test_matrix <- as.matrix(test_data[, features])
train_labels <- train_data$Total_Loss
test_labels <- test_data$Total_Loss

# Convert to DMatrix (optimized format for XGBoost)
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)


# Set XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  # Regression task
  eval_metric = "rmse",            # Root Mean Squared Error
  eta = 0.1,                        # Learning rate
  max_depth = 6,                    # Tree depth
  subsample = 0.8,                   # Subsample ratio for boosting
  colsample_bytree = 0.8             # Feature sampling ratio
)

# Train model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,  # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,  # Stop if no improvement
  verbose = 1
)


# Predictions
train_preds <- predict(xgb_model, dtrain)
test_preds <- predict(xgb_model, dtest)

# RMSE Calculation
train_rmse <- sqrt(mean((train_labels - train_preds)^2))
test_rmse <- sqrt(mean((test_labels - test_preds)^2))

# R-squared Calculation
train_r2 <- 1 - sum((train_labels - train_preds)^2) / sum((train_labels - mean(train_labels))^2)
test_r2 <- 1 - sum((test_labels - test_preds)^2) / sum((test_labels - mean(test_labels))^2)

# Print results
print(paste("Training RMSE:", round(train_rmse, 2)))
print(paste("Test RMSE:", round(test_rmse, 2)))
print(paste("Training R²:", round(train_r2, 4)))
print(paste("Test R²:", round(test_r2, 4)))
