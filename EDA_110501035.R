# Read the CSV file
data <- read.csv("data.csv")

# Select variables related to attrition
selected_vars <- data[c("Attrition", "Age", "JobSatisfaction", "MonthlyIncome", "YearsAtCompany")]

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Convert Attrition to a factor variable
selected_vars$Attrition <- as.factor(selected_vars$Attrition)

# 1. Distribution of Attrition
# This plot shows the overall distribution of attrition within the organization.
# It helps us understand the proportion of employees who have left the organization (Attrition = Yes)
# versus those who have stayed (Attrition = No).
attrition_plot <- ggplot(selected_vars, aes(x = Attrition, fill = Attrition)) + 
  geom_bar() + 
  ggtitle("Distribution of Attrition") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=5) +
  labs(x = "Attrition", y = "Count")

# 2. Attrition vs Monthly Income
# This plot examines the relationship between attrition and monthly income.
# By visualizing the density of monthly income for both groups (Attrition = Yes and No),
# we can identify if income levels play a role in employee turnover.
# Interestingly, we observe that in the income range of 15000-17500, there are no attritions,
# but attritions appear again in the range of 17500-20000. This is an intriguing observation.
attrition_monthly_income_plot <- ggplot(selected_vars, aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Attrition vs Monthly Income") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  labs(x = "Monthly Income", y = "Density")

# 3. Attrition vs Age
# This plot explores the relationship between attrition and the age of employees.
# By visualizing the density of age for both groups (Attrition = Yes and No),
# we can determine if certain age groups are more prone to leaving the organization.
attrition_age_plot <- ggplot(selected_vars, aes(x = Age, fill = Attrition)) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Attrition vs Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  labs(x = "Age", y = "Density")

# 4. Attrition vs Years at Company
# This plot investigates the relationship between attrition and the number of years an employee
# has been with the company. By visualizing the density of years at the company for both groups
# (Attrition = Yes and No), we can see if tenure influences employee attrition.
attrition_years_at_company_plot <- ggplot(selected_vars, aes(x = YearsAtCompany, fill = Attrition)) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Attrition vs Years at Company") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  labs(x = "Years at Company", y = "Density")

# Combine all plots into one grid
# This combined plot grid provides a comprehensive view of the factors related to employee attrition.
# By examining these plots together, we can gain insights into how different variables such as
# monthly income, age, and years at the company contribute to attrition.
plot_grid(attrition_plot, attrition_monthly_income_plot, attrition_age_plot, attrition_years_at_company_plot, ncol = 2)
