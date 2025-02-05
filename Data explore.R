library(readr)
library(dplyr)
library(ggplot2)

data = read_csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-data.csv")
#data asli

# Remove the unecessary columns
remove_col <- c("ID", "Years Modified", 
                "Assessment Date", "Primary Type", 
                "Last Inspection Date")  
data <- data %>%
  select(-all_of(remove_col))


# Delete Assessment missing rows
data <- data %>%
  filter(!is.na(Assessment))


# Rename Columns
data <- data %>%
  rename(
    Purpose = `Primary Purpose`, Height = `Height (m)`, Volume = `Volume (m3)`, Year_Completed = `Year Completed`,
    Surface = `Surface (km2)`, Inspection_Freq = `Inspection Frequency`,
    Distance_to_City = `Distance to Nearest City (km)`, Assessment = Assessment, Prob_Fail = `Probability of Failure`,
    Loss_gf_Prop = `Loss given failure - prop (Qm)`, Loss_gf_Liab = `Loss given failure - liab (Qm)`,
    Loss_gf_BI = `Loss given failure - BI (Qm)`
  )



# Convert all factors to numeric
data$Assessment <- as.numeric(factor(data$Assessment))
data$Region <- as.numeric(factor(data$Region))
data$Purpose <- as.numeric(factor(data$Purpose))
data$`Regulated Dam` <- as.numeric(factor(data$`Regulated Dam`))
data$Hazard <- as.numeric(factor(data$Hazard))
data$Spillway <- as.numeric(factor(data$Spillway))



data$Total_Loss <- data$Loss_gf_Prop + data$Loss_gf_Liab + data$Loss_gf_BI

# Calculate the Spearman correlation matrix
correlation_matrix <- cor(data, use = "complete.obs", method = "spearman")




library(ggplot2)
library(reshape2)

#data = read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv")


melted_correlation_matrix <- melt(correlation_matrix)

ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Spearman Correlation Heatmap", x = "", y = "", fill = "Correlation")




library(knitr)
library(kableExtra)

kable(correlation_matrix, format = "html", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)




data = read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv")
data <- na.omit(data)  # Remove all rows with NA


data$Total_Loss <- data$Loss_gf_Prop + data$Loss_gf_Liab + data$Loss_gf_BI


