library(readr)
library(dplyr)
library(ggplot2)
library(VIM)
library(randomForest)

data = read_csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-data.csv")

duplicated_rows <- data %>%
  group_by(ID) %>%
  filter(n() > 1)

print(duplicated_rows) # no dupe


# Remove the unecessary columns
remove_col <- c("ID", "Regulated Dam", "Years Modified", 
                "Drainage (km2)", "Spillway", "Assessment Date", "Primary Type", 
                "Last Inspection Date")  
data <- data %>%
  select(-all_of(remove_col))


# Rename Columns
data <- data %>%
  rename(
    Purpose = `Primary Purpose`, Height = `Height (m)`, Volume = `Volume (m3)`, Year_Completed = `Year Completed`,
    Surface = `Surface (km2)`, Inspection_Freq = `Inspection Frequency`,
    Distance_to_City = `Distance to Nearest City (km)`, Assessment = Assessment, Prob_Fail = `Probability of Failure`,
    Loss_gf_Prop = `Loss given failure - prop (Qm)`, Loss_gf_Liab = `Loss given failure - liab (Qm)`,
    Loss_gf_BI = `Loss given failure - BI (Qm)`, Length = `Length (km)`
  )


sum(is.na(data))



# Length Median Imputation
data$Height[data$Length == 0] <- NA
Length_clean <- na.omit(data$Length)
ks.test(height_clean, "pnorm", mean = mean(Length_clean), sd = sd(Length_clean)) #pvalue kecil, skewed

data$Length[is.na(data$Length)] <- median(Length_clean, na.rm = TRUE)



# Mode imputation on Primary Purpose (5% missing)
mode_value <- names(sort(table(data$Purpose), decreasing = TRUE))[1]
data$Purpose[is.na(data$Purpose)] <- mode_value


# Height Median Imputation (11% missing)
data$Height[data$Height == 0] <- NA
height_clean <- na.omit(data$Height)
ks.test(height_clean, "pnorm", mean = mean(height_clean), sd = sd(height_clean)) #pvalue kecil, skewed

data$Height[is.na(data$Height)] <- median(height_clean, na.rm = TRUE)


# Volume Imputation
data$Volume[data$Volume == 0] <- NA
sum(is.na(data$Volume)) / nrow(data) * 100 #56% missing values

data$log_Volume <- log1p(data$Volume)  # Apply log transformation before KNN

data_imputed <- kNN(data, variable = "log_Volume", k = 5) # Impute log-transformed volume
data_imputed$Volume <- expm1(data_imputed$log_Volume)  # Convert back to original scale
data_imputed <- data_imputed[, colnames(data)]
hist(data_imputed$log_Volume, main="Log Transformed Volume (After KNN)", breaks=50)
hist(data_imputed$Volume, main="Back to Original Scale", breaks=50)
data$Volume <- data_imputed$Volume
remove_col <- c("log_Volume")  
data <- data %>%
  select(-all_of(remove_col))

# Surface Imputation (14% missing)
numerical_data <- data %>% select(where(is.numeric))
correlations <- cor(numerical_data, use = "complete.obs")
correlations["Surface", ] #low

median_surface <- median(data$Surface, na.rm = TRUE)
data$Surface <- ifelse(is.na(data$Surface), median_surface, data$Surface)
sum(is.na(data$Surface))  # Should be 0


# Median Imputation for Inspection_Freq (17% missing)
data$Inspection_Freq[data$Inspection_Freq == 0] <- NA
median_inspection_freq <- median(data$Inspection_Freq, na.rm = TRUE)
data$Inspection_Freq[is.na(data$Inspection_Freq)] <- median_inspection_freq


# Median imputation for 'Year_Completed' (20% missing)
year_completed_clean <- na.omit(data$Year_Completed)
data$Year_Completed[is.na(data$Year_Completed)] <- median(year_completed_clean, na.rm = TRUE)


# Imputation for distance (66% missing)
data$Distance_to_City[data$Distance_to_City == 0] <- NA
par(mfrow=c(1,2))  # Set up plotting area for two histograms
hist(data$Distance_to_City, main = "Distance to Nearest City", xlab = "Distance (km)")

data_imputed <- kNN(data, variable = "Distance_to_City", k = 5)
data_imputed <- data_imputed[, colnames(data)]  # Keep the same column names

par(mfrow=c(1,2))
hist(data$Distance_to_City, main="Imputed Distance to City", col="red", breaks=50)

data_imputed$log_Distance_to_City <- NULL
data$Distance_to_City <- data_imputed$Distance_to_City


# Assessment (3% missing)
data$Assessment[is.na(data$Assessment)] <- "Not Rated"


# Impute missing Loss BI using random forest (65% missing)
subset_data <- data[, c("Loss_gf_BI", "Hazard", "Region", "Assessment", "Prob_Fail", "Volume")]
#Split the dataset into observed (non-missing) and missing (NA) rows for Loss_gf_BI
observed_data <- subset_data[!is.na(subset_data$Loss_gf_BI), ]
missing_data <- subset_data[is.na(subset_data$Loss_gf_BI), ]

rf_model <- randomForest(Loss_gf_BI ~ ., data = observed_data, ntree = 100, mtry = floor(sqrt(ncol(observed_data))))

predicted_values <- predict(rf_model, missing_data)

subset_data$Loss_gf_BI[is.na(subset_data$Loss_gf_BI)] <- predicted_values

summary(subset_data$Loss_gf_BI)

hist(subset_data$Loss_gf_BI, main = "Histogram of Imputed Loss_gf_BI", xlab = "Loss_gf_BI", col = "lightblue") #slightly right skewed

data$Loss_gf_BI[is.na(data$Loss_gf_BI)] <- subset_data$Loss_gf_BI[is.na(data$Loss_gf_BI)]

importance(rf_model)  # Show which features are most useful





# Median imputation for loss prop
loss_prop_clean <- na.omit(data$Loss_gf_Prop)
data$Loss_gf_Prop[is.na(data$Loss_gf_Prop)] <- median(loss_prop_clean, na.rm = TRUE)

# Median imputation for loss liab
loss_liab_clean <- na.omit(data$Loss_gf_Liab)
data$Loss_gf_BI[is.na(data$Loss_gf_Liab)] <- median(loss_liab_clean, na.rm = TRUE)



write.csv(data, "C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv", row.names = FALSE)

