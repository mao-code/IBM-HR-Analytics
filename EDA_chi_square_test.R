# Load the dataset
data <- read.csv("data.csv")

########## Chi-Square Test ##########
# Creating a contingency table
contingency_table <- table(data$Attrition, data$Department)

# Performing the Chi-Square test
chi_square_test <- chisq.test(contingency_table)

# Displaying the results
chi_square_test

# Results:
# X-squared = 10.796, df = 2, p-value = 0.004526

# Statistically Significant Association: 
# Since the p-value (0.0045) is less than 0.05, we reject the null hypothesis. 
# This means there is a statistically significant association between Attrition and Department.
# Interpretation of Association: 
# The association suggests that the attrition rates vary significantly across different departments.