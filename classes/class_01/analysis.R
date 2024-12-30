# Load libraries
library(ggplot2)
library(dplyr)

# Load dataset
data <- read.csv("/work/class_01/dataset.csv")

# Summarize the data: Calculate mean score
summary <- data %>%
  group_by(group) %>%
  summarize(mean_score = mean(score, na.rm = TRUE))
print(summary)

# Plot overall score distribution
overall_plot <- ggplot(data, aes(x = score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    title = "Distribution of Math Test Scores",
    x = "Score",
    y = "Frequency"
  ) +
  theme_minimal()

# Display the overall distribution plot
print(overall_plot)

# Analyze the distribution of scores by group
group_plot <- ggplot(data, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(
    title = "Distribution of Math Test Scores by Group",
    x = "Group",
    y = "Score"
  ) +
  theme_minimal()

# Display the group distribution plot
print(group_plot)

# Save the plots to files
ggsave(filename = "/work/class_01/overall_distribution_plot.png", plot = overall_plot, width = 6, height = 4)
ggsave(filename = "/work/class_01/group_distribution_plot.png", plot = group_plot, width = 6, height = 4)
