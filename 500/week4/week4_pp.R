
setwd("~/Workspace/mcdaniel/500/week4")
data_pp4 <- read.csv("pp_dataset1.csv", sep=",")

new_colnames <- c("us_calori", "us_diet", "ca_calori", "can_diet")
names(data_pp4) <- new_colnames

result_q1 = t.test(data_pp4$us_calori,data_pp4$ca_calori,var.equal=T,paired=F)
result_q1$p.value #[1] 0.05089138

# Q1 CORRECT
## Answer False, Fail to reject null hypothesis


# Q2 CORRECT
result_q1$statistic 
## Answer 2.03

#Q3 FAILED
## first t-statistic

## t-test? hypotheis testing? 

#Q4 CORRECT
## Answer: t-test

#Q5 FAILED
## first Answer 0.05
result_q1$p.value #[1] 0.05089138

# Significance level and degrees of freedom
alpha <- 0.05
df <- 30

# Calculate the critical t-value for the upper tail
critical_t <- qt(1 - alpha/2, df)

# Print the rounded value
round(critical_t, 2)
# Second Answer [1] 2.04

#Q6 FAILED
# first Answer: Test for Homogeneity
# Second Answer: Goodness of fit test

#Q7 FAILED 0.8 pt
## Independent and Categorical

#Second answer
# Categorical data: Chi-squared tests are used for categorical data.
# A simple random sample from the full population: The data should be a random sample.
# A minimum of 5 values for each expected frequency: A common rule is that expected frequencies should be at least 5 for the test to be valid.
# Independent observations: Observations should be independent.


#######################
# Q2 at Docs
#######################


#Q8 CORRECT
data_q2 <- read.csv("PP_q2_data.csv", sep=",")

print(data_q2)

## Answer 4

# Q9 CORRECT
## H0: data fits expected dist
## H1: !=

observed <- data_q2$Observed
expected <- data_q2$Expected

# Perform the Chi-Squared Goodness-of-Fit Test
test_result_chi <- chisq.test(observed, p = expected/sum(expected), )

# Print the results
print(test_result_chi)

# Chi-squared test for given probabilities
# 
# data:  observed
# X-squared = 14.933, df = 4, p-value = 0.004842
# 0.00

# Q10 CORRECT
# False

#Q11 FAILED
# First Goodnessoffit
# Second Test of independence

#Q12 CORRECT
# True 

#Q13 FAILED
# X-squared = 14.93

#######################
# Q3 at Docs
#######################

# Q14 CORRECT
# Load necessary libraries
# install.packages("readr")
library(readr)
# Load the data
data <- read_csv("pp_q3.csv")

# Extract the contingency table (excluding the Total row and column)
contingency_table <- data[1:2, 2:3]  # Adjusted to exclude the 'Unnamed: 0' column
row.names(contingency_table) <- data$`Unnamed: 0`[1:2]

# Ensure that the table contains only numeric values
contingency_table <- as.data.frame(lapply(contingency_table, as.numeric))

# Perform the Chi-Squared Test of Independence
test_result <- chisq.test(contingency_table,correct=FALSE)
test_result$p.value
#Answer [1] 0.4750144

# Q15 FAILED
# First False
# Second must be True

# Q16 FAILED
# First test of independence
# Second homogeneity

# Q17 FAILED
# First: only the sampling used
# Second: sampling used and number ofvariables

#######################
# Q4 at Docs
#######################
library(readr)
# Load the data
data4 <- read_csv("pp_q4.csv")

contingency_table4 <- as.matrix(data4[, -1])
contingency_table4

# Perform the Chi-Squared Test of Homogeneity
test_result4 <- chisq.test(contingency_table4, correct=FALSE)

# Q18 FAILED
test_result4$statistic # Second 0.57
# X-squared 
# First 0.5102876 

# Q19 CORRECT
library(readr)
# Load the data
data4 <- read_csv("pp_q4.csv")

contingency_table4 <- as.matrix(data4[, -1])
contingency_table4

# Perform the Chi-Squared Test of Homogeneity
test_result4 <- chisq.test(contingency_table4, correct=FALSE)

# Print the results
print(test_result4$p.value) # [1] 0.97

# Q20 FAILED
# First Truth
# Second must be False

#Q21 CORRECT
# All..?


#Q22 CORRECT
# Load necessary libraries
# install.packages(c("readxl", "tidyverse"))
library(readxl)
library(tidyverse)

# Load the data
data <- read_excel("pp_q5.xlsx")

# Reshape the data to a long format
data_long <- data %>%
  gather(key = "Restaurant", value = "Deliveries")

# Perform the one-way ANOVA
anova_result <- aov(Deliveries ~ Restaurant, data = data_long)

# Print the results
summary(anova_result) # 0.0045

# Df Sum Sq Mean Sq F value  Pr(>F)   
# Restaurant   2   8553    4276   6.518 0.00446 **
# Residuals   30  19683     656      

#Answer 0.0045

#Q23 CORRECT
# True

#Q24 CORRECT
# ANOVA

# Q25 True CORRECT