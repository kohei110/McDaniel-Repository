#setwd("~/Workspace/mcdaniel/500/final")
# install.packages("corrplot")
library(corrplot)
data <- read.csv("ultimateHousing.csv")
 
 head(data)
 
 # Q1 How many observations about the real estate market in Ultimate city have been acquired?
 nrow(data)
 #1500
 
 # Q2 How many explanatory data (independent variables) are available?
 # 6
  
 
 # Q3 How many categorical variables are availble? (Review)
 # 2 
 
 # Q4 Doe it initially look like you will need to make any adjustments to account for 
 # differences in units of measure, e.g. from data in 1000's to data in 1's?
 # no
 
 # Q5 Are any of the variables in the data highly correlated? (Review)
cor_matrix <- cor(data)

corrplot(cor_matrix, method = 'color')
cor_matrix

# Yes

# Q6 If you answered yes to Q5, which variable would you consider dropping first? (Review)

# lgelot
# Age
# I answered "no" to Q5
# Bedrooms
# Pool
# Baths <- 

# if we concern about multicolinearity, we should drop baths, since it has 0.72 correlation.
# if we simply just want to drop non-informative variable against independent variable, it will be Age.

# Q7 data contain two variables that might be used as indicator variables; lotsize(lgelot) and pool.
# What proportion of the data is lgelot?(leave your answer as a decimal number, not percent)
print(sum(data$lgelot)/nrow(data))
# 0.063 ? 

# Q8 data contain two variables that might be used as indicator variables; lotsize(lgelot) and pool.
# What proportion of the data is pool?(leave your answer as a decimal number, not percent)
print(sum(data$pool)/nrow(data))

# Q9 is the distribution of your dependent variable normal?

hist(data$price,breaks = 30)
# I think this is No

#Q10 If you answered no to Question 9, what transformation would you probably 
# use to help force compliance with the assumption of normality?

# take the natural log of the variable <- 
# quadratic
# inverse(1/vaiable)
# square root

#Q11 Consider the different kinds of models we have studied(simple linear, multivariable linear, non-linear, either simple or multivariable etc)
# Given the tools you have available today, which one of the following is the "test" you would use to determin whuch type of model is best?


# test of independence
# comparing the sums of squares errors to find the least error <- 
# Test for homogeneity
# Goodness-of-fit test
# none of them above

#Q12 Test different models to determine which model you should use to compute the information Ulitimate Realty needs.
# That is, consider the differences in the sum-of-squares-error value, frequency plots to check normality, scatter plots
# to check linearity, etc From the choices below select which model you have found is the best overall.

# simple log quadratic
# Multivariable quadratic
# Simple log-linear(non-linear, logarithmic)
# Multivariable linear
# Simple linear
# Multivariable log-linear
# Multivariable log-quadratic <-
# Simple quadratic(non-linear, polynomial)
# all the modes tested will provide the same accuracy and quality of results
# None of the models tested will provide results.



# Simple Linear Model
model_simple_linear_sqft <- lm(price ~ sqft, data=data)
sse_simple_linear_sqft <- sum(resid(model_simple_linear_sqft)^2)

# model_simple_linear_bedrooms <- lm(price ~ bedrooms, data=data)
# sse_simple_linear_bedrooms <- sum(resid(model_simple_linear_bedrooms)^2)
# 
# model_simple_linear_baths <- lm(price ~ baths, data=data)
# sse_simple_linear_baths <- sum(resid(model_simple_linear_baths)^2)
# 
# model_simple_linear_lgelot <- lm(price ~ lgelot, data=data)
# sse_simple_linear_lgelot <- sum(resid(model_simple_linear_lgelot)^2)
# 
# model_simple_linear_age <- lm(price ~ age, data=data)
# sse_simple_linear_age <- sum(resid(model_simple_linear_age)^2)
# 
# model_simple_linear_pool <- lm(price ~ pool, data=data)
# sse_simple_linear_pool <- sum(resid(model_simple_linear_pool)^2)

# Multivariable Linear Model
model_multivariable_linear <- lm(price ~ sqft + bedrooms + baths + lgelot + age + pool, data=data)
sse_multivariable_linear <- sum(resid(model_multivariable_linear)^2)

# Simple Log-Linear Model
model_simple_loglinear_sqft <- lm(log(price) ~ sqft, data=data)
sse_simple_loglinear_sqft <- sum(resid(model_simple_loglinear_sqft)^2)

# model_simple_loglinear_bedrooms <- lm(log(price) ~ bedrooms, data=data)
# sse_simple_loglinear_bedrooms <- sum(resid(model_simple_loglinear_bedrooms)^2)
# 
# model_simple_loglinear_baths <- lm(log(price) ~ baths, data=data)
# sse_simple_loglinear_baths <- sum(resid(model_simple_loglinear_baths)^2)
# 
# model_simple_loglinear_lgelot <- lm(log(price) ~ lgelot, data=data)
# sse_simple_loglinear_lgelot <- sum(resid(model_simple_loglinear_lgelot)^2)
# 
# model_simple_loglinear_age <- lm(log(price) ~ age, data=data)
# sse_simple_loglinear_age <- sum(resid(model_simple_loglinear_age)^2)
# 
# model_simple_loglinear_pool <- lm(log(price) ~ pool, data=data)
# sse_simple_loglinear_pool <- sum(resid(model_simple_loglinear_pool)^2)

# Multivariable Log-Linear Model
model_multivariable_loglinear <- lm(log(price) ~ sqft + bedrooms + baths + lgelot + age + pool, data=data)
sse_multivariable_loglinear <- sum(resid(model_multivariable_loglinear)^2)

# Simple Quadratic Model
model_simple_quadratic_sqft <- lm(price ~ sqft + I(sqft^2), data=data)
sse_simple_quadratic_sqft <- sum(resid(model_simple_quadratic_sqft)^2)

# model_simple_quadratic_bedrooms <- lm(price ~ bedrooms + I(bedrooms^2), data=data)
# sse_simple_quadratic_bedrooms <- sum(resid(model_simple_quadratic_bedrooms)^2)
# 
# model_simple_quadratic_baths <- lm(price ~ baths + I(baths^2), data=data)
# sse_simple_quadratic_baths <- sum(resid(model_simple_quadratic_baths)^2)
# 
# model_simple_quadratic_lgelot <- lm(price ~ lgelot + I(lgelot^2), data=data)
# sse_simple_quadratic_lgelot <- sum(resid(model_simple_quadratic_lgelot)^2)
# 
# model_simple_quadratic_age <- lm(price ~ age + I(age^2), data=data)
# sse_simple_quadratic_age <- sum(resid(model_simple_quadratic_age)^2)
# 
# model_simple_quadratic_pool <- lm(price ~ pool + I(pool^2), data=data)
# sse_simple_quadratic_pool <- sum(resid(model_simple_quadratic_pool)^2)

# Multivariable Quadratic Model
model_multivariable_quadratic <- lm(price ~ sqft + bedrooms + baths + lgelot + age + pool + I(sqft^2), data=data)
sse_multivariable_quadratic <- sum(resid(model_multivariable_quadratic)^2)

# Simple Log-Quadratic Model
model_log_simple_quadratic_sqft <- lm(log(price) ~ sqft + I(sqft^2), data=data)
sse_log_simple_quadratic_sqft <- sum(resid(model_log_simple_quadratic_sqft)^2)

# model_log_simple_quadratic_bedrooms <- lm(log(price) ~ bedrooms + I(bedrooms^2), data=data)
# sse_log_simple_quadratic_bedrooms <- sum(resid(model_log_simple_quadratic_bedrooms)^2)
# 
# model_log_simple_quadratic_baths <- lm(log(price) ~ baths + I(baths^2), data=data)
# sse_log_simple_quadratic_baths <- sum(resid(model_log_simple_quadratic_baths)^2)
# 
# model_log_simple_quadratic_lgelot <- lm(log(price) ~ lgelot + I(lgelot^2), data=data)
# sse_log_simple_quadratic_lgelot <- sum(resid(model_log_simple_quadratic_lgelot)^2)
# 
# model_log_simple_quadratic_age <- lm(log(price) ~ age + I(age^2), data=data)
# sse_log_simple_quadratic_age <- sum(resid(model_log_simple_quadratic_age)^2)
# 
# model_log_simple_quadratic_pool <- lm(log(price) ~ pool + I(pool^2), data=data)
# sse_log_simple_quadratic_pool <- sum(resid(model_log_simple_quadratic_pool)^2)

# Multivariable Log-Quadratic Model
model_log_multivariable_quadratic <- lm(log(price) ~ sqft + bedrooms + baths + lgelot + age + pool + I(sqft^2), data=data)
sse_log_multivariable_quadratic <- sum(resid(model_log_multivariable_quadratic)^2)

model_log_multivariable_quadratic_all <- lm(log(price) ~ sqft + bedrooms + baths + lgelot + age + pool + I(sqft^2)+ I(bedrooms^2) + I(baths^2) + I(lgelot^2) + I(age^2) + I(pool^2), data=data)
sse_log_multivariable_quadratic_all <- sum(resid(model_log_multivariable_quadratic_all)^2)


model_log_multivariable_quadratic_q13 <- lm(log(price) ~ sqft + I(sqft^2) + bedrooms + baths + age +I(age^2), data=data)
sse_log_multivariable_quadratic_q13 <- sum(resid(model_log_multivariable_quadratic_q13)^2)

# # Diagnostic Plots for Each Model (example for the simple linear model)
# par(mfrow=c(2,2))
# plot(model_simple_linear)
# 
# # ... Repeat diagnostic plots for other models
# 
# # Compare Sum of Squared Errors (Residuals)
# models <- list(model_simple_linear, model_multivariable_linear, model_simple_loglinear, model_multivariable_loglinear, model_simple_quadratic, model_multivariable_quadratic)
# sse_values <- sapply(models, function(model) sum(resid(model)^2))
# sse_values

# Q13 As often happens, regardless of what you think is best, your boss decides that you will use a multivariable log-quadratic model as follows.
# Use the variable price, sqft, sqft_sq, age, age_sq, beadrooms, baths, And they says for you take the natural log of the price variable.
# model_log_multivariable_quadratic_q13 <- lm(log(price) ~ sqft + I(sqft^2) + bedrooms + baths + age +I(age^2), data=data)

# Except for the ends of the plot, the QQ plot generally indicates that the assumption/condition of normality has been met?

library(car)
qqPlot(model_log_multivariable_quadratic_q13, main="QQ Plot")
# yes

# Q14 As often happens, regardless of what you think is best, your boss decides that you will use a multivariable log-quadratic model as follows.
# Use the variable price, sqft, sqft_sq, age, age_sq, beadrooms, baths, And they says for you take the natural log of the price variable.
# model_log_multivariable_quadratic_q13 <- lm(log(price) ~ sqft + I(sqft^2) + bedrooms + baths + age +I(age^2), data=data)

# the plot of residual versus fits indicates that assumption/condition of lnearity has been met?
plot(fitted(model_log_multivariable_quadratic_q13), resid(model_log_multivariable_quadratic_q13), 
     xlab="Fitted values", ylab="Residuals", main="Residuals vs Fitted Values")
abline(h=0, col="red")

# YEs?

# Q15 Build and use the model your boss wants to answer the following questions.
# Consider the intercept and all coefficients of this model to select the best choice from the list below.

summary(model_log_multivariable_quadratic_q13)
# There is no way to tell anything about statistiacal significance from the result
# the intercept and all coeeficients are statisticaally significant
# The variables baths is not statistically sigiificant <-
# neither the intercept nor any of the cefficients are statistically significant.

#Q16 The R-squared value indicates that very little of the variance in the data is explained by the model.
# False


#Q17 Hypothesis test can be conducted using a t-statistic and/or its related p-value. 
# if the t=statistics falls in the rejection or the p-values is less than the level of significance of the test
# the null hypothesis must be rejected. The alternative, for example a test to establish that a relationship between
# an explanatory(independent) variable and a response(dependent) variable exists is accepted
# True

#Q18 The boss has conviinced you that this is a pretty good model for Ultimate Realty. Because the boss is particularly interested in how size and age affects house prices.
# evaluate those parameters and use the results to answer the following questions.
# Test the coefficient of size to determine if size is related to price. Based on a hypothesis test using the null hypothesis, i.e. the coefficient equals zero. 
# There is not sufficient evidence(stastical significance) that a relationship exists between the explanatory variable and the response variable. Therefore, size can be removed from the regression model without any impact on the results.

# False x
# True

# Q19 Does the age of a house really affect its price? Again based on a hypothesis test for this model and its variables,
# the coefficient of age equals zero indicating that there is no relationship between the explanatory variable, age, and the response variable price.
summary(model_log_multivariable_quadratic_q13)
# True

# Q20 One of Ultimate's sales agents has just spoken with Mr. Roadrunner about listing his house. The house is 10 years old
# it has 2000 square feet of living area, and 3 bedrooms. How much should Ultimate's agent tell Mr. Roadrunner he should list his house for i.e. price?

model_log_multivariable_quadratic_q20 <- lm(log(price) ~ sqft + I(sqft^2) + age +I(age^2)  + bedrooms, data=data)
summary(model_log_multivariable_quadratic_q20)

model_summary <- summary(model_log_multivariable_quadratic_q20)
coef_intercept <- model_summary$coefficients[1]
coef_sqft <- model_summary$coefficients[2]
coef_sqft_sq <- model_summary$coefficients[3]
coef_age <- model_summary$coefficients[4]
coef_age_sq <- model_summary$coefficients[5]

coef_bed <- model_summary$coefficients[6]

min(data$sqft)

# Mr. Roadrunner's house characteristics
sqft <- 20
bedrooms <- 3
age <- 10
# bath <- 0 

predict_result <- predict(model_log_multivariable_quadratic_q20, 
                          newdata = data.frame(sqft = 20, age = 10, bedrooms = 3), 
                          interval = "confidence", level = 0.95, se.fit = TRUE)
predict_result
# Compute the predicted natural log of the price
log_price_predicted <- coef_intercept + 
  coef_sqft * sqft + 
  coef_sqft_sq * sqft^2 + 
  coef_bed * bedrooms + 
  coef_age * age + 
  coef_age_sq * age^2

# Convert the predicted log(price) to actual price
price_predicted <- exp(log_price_predicted)
price_predicted

# 147865.3

# Q21
# Predict the log(price) and get the standard error of the prediction
predict_result <- predict(model_log_multivariable_quadratic_q20, 
                          newdata = data.frame(sqft = 20, age = 10, bedrooms = 3), 
                          interval = "confidence", level = 0.95, se.fit = TRUE)

# Calculate the 95% confidence interval for the actual price
ci_lower_bound <- exp(predict_result$fit[1, "lwr"])
ci_upper_bound <- exp(predict_result$fit[1, "upr"])

# Q22
# Q23
# f. Predict the price increase for an additional 200 sqft
sqft_new <- 20 + 2
log_price_predicted_new <- coef_intercept + 
  coef_sqft * sqft_new + 
  coef_sqft_sq * sqft_new^2 + 
  coef_bed * bedrooms + 
  coef_age * age + 
  coef_age_sq * age^2

log_price_predicted_new <- exp(log_price_predicted_new)

log_price_predicted_new - price_predicted

# Convert the predicted log(price) to actual price for the increased sqft
price_predicted_new <- exp(log_price_predicted_new)
price_increase <- price_predicted_new - price_predicted

list(ci_lower_bound = ci_lower_bound, ci_upper_bound = ci_upper_bound, price_increase = price_increase)

# Q24 The boss would like to look at some different results. He/she says it is because there are two explanatory variables that have not yet been explored
# large lot size(lgelot) and pool. He/She wants you to remove the second-order terms from the model, add these variables and the variable baths to the model and generate an interaction term between 
# large lot and size(lgelot x sqft). This results in a model expressed as :

model_log_multivariable_quadratic_q24 <- lm(log(price) ~ sqft + bedrooms + baths +
                                              lgelot + age + pool + lgelot*sqft, data=data)
summary(model_log_multivariable_quadratic_q24)
# False

#Q25 How much more, in a percentage, is the price of a house on a large lot(greater than 0.5 acres) than a house on a small lot?
# 1. Set the values for other variables

# Using median values for continuous variables and mode for categorical ones
median_sqft <- median(data$sqft)
median_age <- median(data$age)
median_bedrooms <- median(data$bedrooms)
median_baths <- median(data$baths)
mode_pool <- as.numeric(names(sort(table(data$pool), decreasing = TRUE)[1]))

# 2 & 3. Predict the log(price) for lgelot scenarios

# For lgelot == 1
log_price_predicted_large_lot <- predict(model_new, 
                                         newdata = data.frame(sqft = median_sqft, 
                                                              bedrooms = median_bedrooms, 
                                                              baths = median_baths, 
                                                              lgelot = 1, 
                                                              age = median_age, 
                                                              pool = mode_pool))

# For lgelot == 0
log_price_predicted_small_lot <- predict(model_new, 
                                         newdata = data.frame(sqft = median_sqft, 
                                                              bedrooms = median_bedrooms, 
                                                              baths = median_baths, 
                                                              lgelot = 0, 
                                                              age = median_age, 
                                                              pool = mode_pool))

# 4. Convert the predicted log(price) to actual price
price_predicted_large_lot <- exp(log_price_predicted_large_lot)
price_predicted_small_lot <- exp(log_price_predicted_small_lot)

# 5. Calculate the percentage difference
percentage_difference <- ((price_predicted_large_lot - price_predicted_small_lot) / price_predicted_small_lot) * 100

percentage_difference
# 42.68

# Q26 what is the percent increase in price for an additional 100 square feet of living space if the house is not on a large lot(holding all other variable constant)?
# Given coefficient for sqft
# 1. Set the values for other variables

# Using median values for continuous variables and mode for categorical ones
median_sqft <- median(data$sqft)
median_age <- median(data$age)
median_bedrooms <- median(data$bedrooms)
median_baths <- median(data$baths)
mode_pool <- as.numeric(names(sort(table(data$pool), decreasing = TRUE)[1]))

# 2. Predict the log(price) for the current square footage with lgelot == 0

log_price_predicted_current <- predict(model_new, 
                                       newdata = data.frame(sqft = median_sqft, 
                                                            bedrooms = median_bedrooms, 
                                                            baths = median_baths, 
                                                            lgelot = 0, 
                                                            age = median_age, 
                                                            pool = mode_pool))

# 3. Predict the log(price) for the current square footage + 100 with lgelot == 0

log_price_predicted_plus100 <- predict(model_new, 
                                       newdata = data.frame(sqft = median_sqft + 1, 
                                                            bedrooms = median_bedrooms, 
                                                            baths = median_baths, 
                                                            lgelot = 0, 
                                                            age = median_age, 
                                                            pool = mode_pool))

# 4. Convert the predicted log(price) to actual price
price_predicted_current <- exp(log_price_predicted_current)
price_predicted_plus100 <- exp(log_price_predicted_plus100)

# 5. Calculate the percentage difference for the 100 square feet difference
percentage_increase_100sqft_small_lot <- ((price_predicted_plus100 - price_predicted_current) / price_predicted_current) * 100

percentage_increase_100sqft_small_lot
# 6.06

# Q27 what is the percent increase in price for an additional 100 square feet of living space if the house is on a large lot?

# 1. Set the values for other variables
# Using median values for continuous variables and mode for categorical ones
median_sqft <- median(data$sqft)
median_age <- median(data$age)
median_bedrooms <- median(data$bedrooms)
median_baths <- median(data$baths)
mode_pool <- as.numeric(names(sort(table(data$pool), decreasing = TRUE)[1]))

# 2. Predict the log(price) for the current square footage with lgelot == 1

log_price_predicted_current_large_lot <- predict(model_new, 
                                                 newdata = data.frame(sqft = median_sqft, 
                                                                      bedrooms = median_bedrooms, 
                                                                      baths = median_baths, 
                                                                      lgelot = 1, 
                                                                      age = median_age, 
                                                                      pool = mode_pool))

# 3. Predict the log(price) for the current square footage + 100 with lgelot == 1

log_price_predicted_plus100_large_lot <- predict(model_new, 
                                                 newdata = data.frame(sqft = median_sqft + 1, 
                                                                      bedrooms = median_bedrooms, 
                                                                      baths = median_baths, 
                                                                      lgelot = 1, 
                                                                      age = median_age, 
                                                                      pool = mode_pool))

# 4. Convert the predicted log(price) to actual price
price_predicted_current_large_lot <- exp(log_price_predicted_current_large_lot)
price_predicted_plus100_large_lot <- exp(log_price_predicted_plus100_large_lot)

# 5. Calculate the percentage difference for the 100 square feet difference
percentage_increase_100sqft_large_lot <- ((price_predicted_plus100_large_lot - price_predicted_current_large_lot) / price_predicted_current_large_lot) * 100

percentage_increase_100sqft_large_lot # 4.37