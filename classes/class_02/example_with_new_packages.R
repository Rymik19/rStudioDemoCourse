# Load necessary libraries
library(janitor) # For data cleaning and summarization
library(plotly)  # For interactive plots

# Create a simple dataset
data <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "Alice", "Bob"),
  Category = c("A", "B", "A", "B", "A"),
  Score = c(88, 92, 85, 78, 94)
)

# Clean the data
# Tabulate occurrences of names and categories using the Janitor package
clean_data <- data %>%
  tabyl(Name, Category) # Function from the Janitor package

# Display the cleaned data
cat("Tabulated Data:\n")
print(clean_data)

# Summarize the scores by category
category_summary <- data %>%
  group_by(Category) %>%
  summarise(Average_Score = mean(Score))

cat("\nCategory Summary:\n")
print(category_summary)

# Visualize the summary with an interactive bar plot using the Plotly package.
bar_plot <- plot_ly(
  category_summary,
  x = ~Category,
  y = ~Average_Score,
  type = "bar",
  text = ~paste("Average Score:", round(Average_Score, 1)),
  textposition = 'outside'
) %>%
  layout(
    title = "Average Score by Category",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Average Score")
  )

# Display the interactive plot
print(bar_plot)
