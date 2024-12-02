# Load libraries
library(ggplot2)
library(dplyr)

# Load dataset
data <- read.csv("data/dataset.csv")

# Perform a simple analysis
summary <- data %>% summarize(mean_value = mean(variable, na.rm = TRUE))
print(summary)

# Plot
ggplot(data, aes(x = variable)) +
  geom_histogram() +
  labs(title = "Distribution of Variable")
