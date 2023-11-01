# Load necessary libraries
library(ggplot2)
library(car)


data <- read.csv("batRouge.csv")

head(data)

traditional_houses <- subset(data, traditional==1)

model_multi <- lm(price ~ sqft+age, data=traditional_houses)
summary(model_multi)

#Q1 
# No

#Q2
# 72.73


model_multi_full <- lm(price ~ sqft+age, data=data)
summary(model_multi_full)


#Q3
# false

#Q4
# 18.24


mean(data$price) - mean(traditional_houses$price)
#Q5 failed
# 90.97 x 
# 11392.62

# Q6
# -755.04

#Q7
#False


data$sqft2 <- data$sqft^2
data$age2 <- data$age^2

# Fit the model with higher order terms
model_higher_order <- lm(price ~ sqft + age + sqft2 + age2, data=data)

# View the summary of the model to check the statistical significance of the coefficients
summary(model_higher_order)

#Q8
#False

#Q9
# -5.578e+01
# −55.7842

number <- -55.7842
formatted_number <- formatC(number, format = "e", digits = 4)
print(formatted_number)

#Q10
# 2.315e-02
# 0.0231528

#Q11
# -2.798e+03
# −2797.79


#Q12
# 3.016e+01
# 30.1603

# Given coefficients from the model
beta1 <- -55.7842 #-5.578e+01
beta3 <- 0.0231528 #2.315e-02
beta2 <- -2797.79 # -2.798e+03
beta4 <- 30.1603 #3.016e+01



# House size for which we want to compute the marginal effect
sqft_smallest <- 662

# Calculate the marginal effect for sqft for the largest house
marginal_effect_sqft_smallest <- beta1 + 2 * beta3 * sqft_smallest

# Print the result
marginal_effect_sqft_smallest

#Q13
# -25.13

# House size for which we want to compute the marginal effect
sqft_largest <- 7897

# Calculate the marginal effect for sqft for the largest house
marginal_effect_sqft_largest <- beta1 + 2 * beta3 * sqft_largest

# Print the result
marginal_effect_sqft_largest
# Q14
# 309.89


# House size for which we want to compute the marginal effect
sqft_2300 <- 2300

# Calculate the marginal effect for sqft for the largest house
marginal_effect_sqft_2300 <- beta1 + 2 * beta3 * sqft_2300

# Print the result
marginal_effect_sqft_2300
#Q15
# 50.72

# Find the age of the oldest house
age_oldest <- max(data$age)

# Calculate the marginal effect for age for the oldest house
marginal_effect_age_oldest <- beta2 + 2 * beta4 * age_oldest

# Print the result
marginal_effect_age_oldest

#Q16
# 2027.86

# Find the age of the oldest house
age_newest <- min(data$age)

# Calculate the marginal effect for age for the oldest house
marginal_effect_age_newest <- beta2 + 2 * beta4 * age_newest

# Print the result
marginal_effect_age_newest

#Q17 failed
# -5.578e+01+2*2.315e-02*7897 =  309.85
# -2737.68 x


# Calculate the marginal effect for age for the oldest house
marginal_effect_age_20 <- beta2 + 2 * beta4 * 20
# Print the result
marginal_effect_age_20
#Q18
# -1591.6


# Extract the variance-covariance matrix from the model
var_cov_matrix <- vcov(model_higher_order)

# Compute the standard error for the marginal effect at specific sqft values
compute_se_marginal_effect <- function(sqft_value, var_cov_matrix) {
  var_beta1 <- var_cov_matrix["sqft", "sqft"]
  var_beta2 <- var_cov_matrix["sqft2", "sqft2"]
  cov_beta1_beta2 <- var_cov_matrix["sqft", "sqft2"]
  
  se <- sqrt(var_beta1 + 4 * sqft_value^2 * var_beta2 + 4 * sqft_value * cov_beta1_beta2)
  return(se)
}

# Compute SE for specific sqft values (e.g., 662, 7897, 2300)
se_662 <- compute_se_marginal_effect(662, var_cov_matrix)
se_7897 <- compute_se_marginal_effect(7897, var_cov_matrix)
se_2300 <- compute_se_marginal_effect(2300, var_cov_matrix)

list(se_662 = se_662, se_7897 = se_7897, se_2300 = se_2300)
# Calculate the 95% confidence interval
compute_confidence_interval <- function(marginal_effect, se, df) {
  t_value <- qt(0.975, df)
  lower_bound <- marginal_effect - t_value * se
  upper_bound <- marginal_effect + t_value * se
  return(list(lower_bound = lower_bound, upper_bound = upper_bound))
}

# Degrees of freedom
df <- nrow(data) - 5  # 4 predictors + intercept

# Marginal effects at specific sqft values
marginal_effect_662 <- coef_sqft + 2 * coef_sqft2 * 662
marginal_effect_7897 <- coef_sqft + 2 * coef_sqft2 * 7897
marginal_effect_2300 <- coef_sqft + 2 * coef_sqft2 * 2300

# 95% CI for the specific sqft values
ci_662 <- compute_confidence_interval(marginal_effect_662, se_662, df)
ci_7897 <- compute_confidence_interval(marginal_effect_7897, se_7897, df)
ci_2300 <- compute_confidence_interval(marginal_effect_2300, se_2300, df)

list(ci_662 = ci_662, ci_7897 = ci_7897, ci_2300 = ci_2300)


#Q19
# 45.72072

upper_bound_marginal_effect <- marginal_effect_at_mean + qt(0.975, df) * std_error_marginal_effect
upper_bound_marginal_effect

#Q20
# 67.24

# Load necessary library
library(ggplot2)

# 1. Create the OLS model with interaction term between sqft and owner
model_ols <- lm(price ~ sqft + bedrooms + baths + age + owner + sqft:owner, data=data)
ols_summary <- summary(model_ols)
print(ols_summary)

# 2. Create the log-linear model with the same predictors
model_log_linear <- lm(log(price) ~ sqft + bedrooms + baths + age + owner + sqft:owner, data=data)
log_linear_summary <- summary(model_log_linear)
print(log_linear_summary)

# 3. Generate a Q-Q plot from the log-linear model results to check normality
qqnorm(resid(model_log_linear))
qqline(resid(model_log_linear))

#Q21 
#False

#Q22
#True

#Q23
# 0.72? 0.63 from gretl

#Q24
#True

#Q25 
#True

#Q26
#True

# Load the necessary libraries
library(ggplot2)

# Assuming the data is already loaded into 'data'

# 1. Creating the OLS Model:

# Create the interaction term
data$sqft_owner <- data$sqft * data$owner

# Create the OLS model
model_ols <- lm(price ~ sqft + bedrooms + baths + age + owner + sqft_owner, data=data)
print(summary(model_ols))

# 2. Creating the Log-Linear Model:

# Create the log-linear model
model_log_linear <- lm(log(price) ~ sqft + bedrooms + baths + age + owner + sqft_owner, data=data)
print(summary(model_log_linear))

# 3. Generating the Q-Q Plot:

# Generate Q-Q plot for residuals of the log-linear model
qqnorm(residuals(model_log_linear))
qqline(residuals(model_log_linear))

# 4. Calculating the Marginal Effects:

coef <- coefficients(model_ols)
marginal_effect_2000sqft <- coef["sqft"] + coef["sqft_owner"] * 2000
marginal_effect_2500sqft <- coef["sqft"] + coef["sqft_owner"] * 2500


print(paste("Marginal effect for 2000 sqft owner-occupied house: ", marginal_effect_2000sqft))
print(paste("Marginal effect for 2500 sqft owner-occupied house: ", marginal_effect_2500sqft))

#Q27
#0.00030878

#Q28
# -0.10

#Q29
#.False

# #Q30
# True

#Q31
# false

#Q32
# False

#Q33
#True

#Q34
# False

#Q35
# #False