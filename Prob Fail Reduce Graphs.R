# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the dataset
data <- read.csv("C:/Users/raffa/Desktop/My files/UPH/8) Semester 6/Kapita Selekta/soa study case/damclean-modif.csv")

# Simulated example data (Replace with actual data)
set.seed(123)
data <- data %>%
  mutate(
    Prob_Fail_10_15 = Prob_Fail * runif(n(), 0.85, 0.90),
    Prob_Fail_20_25 = Prob_Fail * runif(n(), 0.75, 0.80),
    Prob_Fail_25_30 = Prob_Fail * runif(n(), 0.70, 0.75)
  )

# Define function to create separate plots
plot_reduction <- function(before, after, reduction_label) {
  df <- tibble(
    Stage = factor(rep(c("Before Reinforcement", "After Reinforcement"), each = nrow(data)),
                   levels = c("Before Reinforcement", "After Reinforcement")),  # Ensures correct order
    Prob_Fail = c(before, after)
  )
  
  ggplot(df, aes(x = Stage, y = Prob_Fail, fill = Stage)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, color = "black", alpha = 0.3) +
    labs(title = paste("Dam Failure Probability:", reduction_label),
         x = "Reinforcement Stage",
         y = "Probability of Failure (Prob_Fail)") +
    theme_minimal() +
    scale_fill_manual(values = c("Before Reinforcement" = "red", "After Reinforcement" = "blue")) +
    theme(legend.position = "none")
}

# Generate and display plots
p1 <- plot_reduction(data$Prob_Fail, data$Prob_Fail_10_15, "10-15% Reduction")
p2 <- plot_reduction(data$Prob_Fail, data$Prob_Fail_20_25, "20-25% Reduction")
p3 <- plot_reduction(data$Prob_Fail, data$Prob_Fail_25_30, "25-30% Reduction")

print(p1)
print(p2)
print(p3)
