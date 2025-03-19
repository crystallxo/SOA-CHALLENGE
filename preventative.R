library(dplyr)
library(tidyr)

# Load data
data <- read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/damclean-modif.csv")

# Calculate Age
data$Age <- 2025 - data$Year_Completed

# Impute missing values
data <- data %>%
  mutate(across(c(Loss_gf_Prop, Loss_gf_Liab, Loss_gf_BI), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Ensure no NAs remain
stopifnot(sum(is.na(data$Loss_gf_Prop)) == 0)
stopifnot(sum(is.na(data$Loss_gf_Liab)) == 0)
stopifnot(sum(is.na(data$Loss_gf_BI)) == 0)

# Compute Total Loss
data$Total_Loss <- data$Loss_gf_Prop + data$Loss_gf_Liab + data$Loss_gf_BI

# Apply log transformation to handle skewness
data <- data %>%
  mutate(
    Log_Prob_Fail = log(Prob_Fail + 1e-6),
    Log_Total_Loss = log(Total_Loss + 1e-6),
    Log_Age = log(Age + 1)
  )

# Compute quantiles on transformed data
prob_fail_q75 <- quantile(data$Log_Prob_Fail, 0.75, na.rm = TRUE)
prob_fail_q25 <- quantile(data$Log_Prob_Fail, 0.25, na.rm = TRUE)

loss_q75 <- quantile(data$Log_Total_Loss, 0.75, na.rm = TRUE)
loss_q25 <- quantile(data$Log_Total_Loss, 0.25, na.rm = TRUE)

age_q75 <- quantile(data$Log_Age, 0.75, na.rm = TRUE)
age_q25 <- quantile(data$Log_Age, 0.25, na.rm = TRUE)

# Convert Hazard to character if needed
data$Hazard <- as.character(data$Hazard)

# Tier Classification (Using Transformed Data)
data <- data %>%
  mutate(Tier = case_when(
    # **Tier 1: Highest Risk**
    (Log_Prob_Fail > prob_fail_q75 & Log_Total_Loss > loss_q75) |
      (Log_Prob_Fail > prob_fail_q25 & Log_Total_Loss > loss_q25 & Log_Age > age_q75 & Hazard == "high") ~ "Tier 1",
    
    # **Tier 2: Moderate Risk**
    (Log_Prob_Fail > prob_fail_q25 & Log_Total_Loss > loss_q25) |
      (Log_Age > age_q75 & Hazard == "significant") ~ "Tier 2",
    
    # **Tier 3: Lower Risk**
    TRUE ~ "Tier 3"
  ))

# Show tier distribution
table(data$Tier)
count <- nrow(subset(data, Region == "Navaldia"))
