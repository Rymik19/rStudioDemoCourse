# Load required libraries
library(ggplot2)
library(dplyr)

# Function to load and validate the dataset
load_and_validate_data <- function(file_path) {
  # Load dataset
  data <- read.csv(file_path)
  
  # Validate columns
  required_columns <- c("id", "score", "group")
  if (!all(required_columns %in% colnames(data))) {
    stop("Error: Dataset does not contain the required columns: id, score, group.")
  }
  
  # Check for missing values
  if (any(is.na(data$score))) {
    warning("Warning: The dataset contains missing scores. These will be ignored in the analysis.")
  }
  
  return(data)
}

# Function to summarize the dataset
summarize_data <- function(data) {
  summary <- data %>%
    group_by(group) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      median_score = median(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      count = n()
    )
  return(summary)
}

# Function to generate plots
create_plots <- function(data) {
  # Overall score distribution
  overall_plot <- ggplot(data, aes(x = score)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(
      title = "Distribution of Math Test Scores",
      x = "Score",
      y = "Frequency"
    ) +
    theme_minimal()
  
  # Group-wise boxplot
  group_plot <- ggplot(data, aes(x = group, y = score, fill = group)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.7) +
    labs(
      title = "Distribution of Math Test Scores by Group",
      x = "Group",
      y = "Score"
    ) +
    theme_minimal()
  
  # Density plot
  density_plot <- ggplot(data, aes(x = score, fill = group)) +
    geom_density(alpha = 0.4) +
    labs(
      title = "Density Plot of Math Test Scores by Group",
      x = "Score",
      y = "Density"
    ) +
    theme_minimal()
  
  return(list(overall = overall_plot, group = group_plot, density = density_plot))
}

# Function to save plots
save_plots <- function(plots, output_dir) {
  ggsave(filename = file.path(output_dir, "overall_distribution_plot.png"), plot = plots$overall, width = 6, height = 4)
  ggsave(filename = file.path(output_dir, "group_distribution_plot.png"), plot = plots$group, width = 6, height = 4)
  ggsave(filename = file.path(output_dir, "density_plot.png"), plot = plots$density, width = 6, height = 4)
}

# Main function to orchestrate the analysis
main <- function() {
  # File paths
  input_file <- "/work/class_01/dataset.csv"
  output_dir <- "/work/class_01"
  
  # Load and validate data
  data <- load_and_validate_data(input_file)
  
  # Summarize data
  summary <- summarize_data(data)
  print(summary)
  
  # Generate plots
  plots <- create_plots(data)
  
  # Display plots
  print(plots$overall)
  print(plots$group)
  print(plots$density)
  
  # Save plots to files
  save_plots(plots, output_dir)
}

# Run the main function
main()
