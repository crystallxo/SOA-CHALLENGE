library(readr)
library(dplyr)
library(ggplot2)

data = read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/dam-clean.csv")
data <- na.omit(data)  # Remove all rows with NA


data$Total_Loss <- data$Loss_gf_Prop + data$Loss_gf_Liab + data$Loss_gf_BI

data$Assessment <- as.numeric(factor(data$Assessment))
data$Region <- as.numeric(factor(data$Region))
data$Purpose <- as.numeric(factor(data$Purpose))
data$Hazard <- as.numeric(factor(data$Hazard))

Flumevale_data <- data %>% filter(Region == "1")
Lyndrassia_data <- data %>% filter(Region == "2")
Navaldia_data <- data %>% filter(Region == "3")


# Flumevale
Flumevale_data <- Flumevale_data[, sapply(Flumevale_data, sd) > 0]
correlation_matrix <- cor(Flumevale_data, use = "complete.obs", method = "spearman")
melted_correlation_matrix <- melt(correlation_matrix)
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Flumevale Corrmap", x = "", y = "", fill = "Correlation")

Flumevale_data$Scale_Loss <- Flumevale_data$Total_Loss/(0.54*Flumevale_data$Surface)


# Lyndrassia
Lyndrassia_data <- Lyndrassia_data[, sapply(Lyndrassia_data, sd) > 0]
correlation_matrix <- cor(Lyndrassia_data, use = "complete.obs", method = "spearman")
melted_correlation_matrix <- melt(correlation_matrix)
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Lyndrassia Corrmap", x = "", y = "", fill = "Correlation")


# Navaldia
Navaldia_data <- Navaldia_data[, sapply(Navaldia_data, sd) > 0]
correlation_matrix <- cor(Navaldia_data, use = "complete.obs", method = "spearman")
melted_correlation_matrix <- melt(correlation_matrix)
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Navaldia Corrmap", x = "", y = "", fill = "Correlation")




# Tabel correlations
library(knitr)
library(kableExtra)

kable(correlation_matrix, format = "html", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
#jgn lupa ganti per region dlu