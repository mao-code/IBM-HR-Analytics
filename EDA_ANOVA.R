library(tidyverse)

data <- read.csv("data.csv")

# Perform ANOVA
anova_result <- aov(MonthlyIncome ~ JobRole, data = data)
summary(anova_result)

# Df          Sum Sq   Mean       Sq          F value   Pr(>F)    
# JobRole     8        2.657e+10  3.321e+09   810.2     <2e-16 ***
# Residuals   1461     5.989e+09  4.099e+06            

# Significant Effect: The very low p-value (<2e-16) suggests that JobRole has a statistically significant effect on MonthlyIncome.
# Reject Null Hypothesis: You reject the null hypothesis, which states that the means of MonthlyIncome are the same across different job roles. There is strong evidence that the average MonthlyIncome varies by JobRole.
