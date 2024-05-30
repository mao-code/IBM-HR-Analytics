library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(cowplot)
library(ggthemes)

# Load file
df <- read.csv("./w16/HR-Employee-Attrition.csv", header = TRUE)
head(df)

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)

# Digitalize categorical data to be calculated (Yes/No -> 1/0)
df$Attrition <- ifelse(df$Attrition == "Yes", 1, 0)

# Calculate point-biserial correlation for binary-continuous pairs
pbcor <- function(x, y) {  
  n <- length(y)
  mean_y1 <- mean(y[x == 1])
  mean_y0 <- mean(y[x == 0])
  s <- sd(y)
  pb_correlation <- (mean_y1 - mean_y0) / s * sqrt(sum(x) * (n - sum(x)) / n^2)
  return(pb_correlation)
}

# Initialize correlation matrix between "Attrition" and possible related indexes
variables <- df[c("Attrition", "MonthlyIncome", "Age", "YearsAtCompany")]
correlation_matrix <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(names(variables), names(variables)))

# Fill the matrix with appropriate correlation coefficients
correlation_matrix[1, 1] <- 1  # Correlation of Attrition with itself
correlation_matrix[1, 2] <- pbcor(variables$Attrition, variables$MonthlyIncome)
correlation_matrix[1, 3] <- pbcor(variables$Attrition, variables$Age)
correlation_matrix[1, 4] <- pbcor(variables$Attrition, variables$YearsAtCompany)
correlation_matrix[2, 1] <- correlation_matrix[1, 2]
correlation_matrix[3, 1] <- correlation_matrix[1, 3]
correlation_matrix[4, 1] <- correlation_matrix[1, 4]

# Fill the rest with Pearson correlation
correlation <- cor(variables[, -1], method = "pearson")
correlation_matrix[2:4, 2:4] <- correlation

# Convert matrix to dataframe for ggplot
correlation_df <- melt(correlation_matrix, na.rm = TRUE)

# Plot heatmap
ggplot(data = correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
  labs(title = "Correlation Heatmap of Index") +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the title
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + # Remove axis titles
  coord_fixed() # Fix aspect ratio



# Summarize data: average income by JobRole and Attrition
summary_df <- df %>%
  group_by(Department, Attrition) %>%
  summarize(AverageIncome = mean(MonthlyIncome), .groups = 'drop')

# Convert Attrition back to factor for plotting
summary_df$Attrition <- factor(summary_df$Attrition, levels = c(0, 1), labels = c("No", "Yes"))

# Plot bar chart
ggplot(summary_df, aes(x = Department, y = AverageIncome, fill = Attrition, label = paste0("$", round(AverageIncome, 2)))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -1.2, size = 3) +
  scale_fill_manual(values = c("Yes" = "hotpink2", "No" = "skyblue2")) +
  labs(title = "Average Monthly Income by Department and Attrition Status", x = "Department", y = "Average Monthly Income") +
  theme(axis.text.x = element_text(angle = 43, hjust = 1, size = 9)) + # Adjust text size and angle
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, max(summary_df$AverageIncome) * 1.2) + # Increase y-axis limit for better label placement
  theme(legend.position = "top", legend.title = element_blank()) # Move legend to top and remove title



# Convert Attrition and JobRole to factors
df$Attrition <- as.factor(df$Attrition)
df$JobRole <- as.factor(df$JobRole)

# Boxplot with Attrition on the X-axis and Job Satisfaction on the Y-axis faceted by JobRole
box.attrition <- df %>% select(Attrition, JobSatisfaction, JobRole) %>%
  ggplot(aes(x = Attrition, y = JobSatisfaction, fill = Attrition, group = Attrition)) +
  geom_boxplot(color = "black") +
  theme_minimal() +
  facet_wrap(~JobRole) +
  scale_fill_manual(values = c("lightblue", "indianred2"), labels = c("No", "Yes")) +
  ggtitle("Boxplot of Job Satisfaction by Attrition and Job Role") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("No", "Yes"))  # Change the labels for Attrition variable

# Combine the plots in a single output
plot_grid(box.attrition)