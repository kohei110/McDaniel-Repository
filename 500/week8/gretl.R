# Load necessary libraries
library(ggplot2)
library(car)

# 1.	Generate descriptive statistics for the variables price, sqft, and age.  Use these statistics to answer the following questions.
# Q1　Failed
data <- read.csv("batRouge.csv")

head(data)
# a.	The price variable is:
# Numeric Discrete　 x
# Numeric Continuous

# Q2
# b.	The values in the price variable are in:
# Dollars (USD)

# Q3
# c.	The values in the sqft variable are in:
# i.	1’s

# Q4
# d.	The values in the age variable are in:
# i.	1’s

# Q5 Failed
# e.	Based on the answers above, no adjustment or transformation should be required to interpret the results of analyses using these variables
# False x 
# True

# Q6
traditional_houses <- subset(data, traditional == 1)
nrow(traditional_houses)
# 582

# Q7
cor.test(traditional_houses$price,traditional_houses$sqft)
# True

# Q8
# c.	The value of the correlation coefficient is _________.      
round(0.7995538,2)
# 0.80

# Q9
# Assuming your data is loaded and filtered for traditional houses
plot(traditional_houses$sqft, traditional_houses$price, 
     main="House Price vs. House Size (Traditional Style Homes)",
     xlab="House Size (sqft)", ylab="House Price", 
     pch=20, col=rgb(0,0,1,0.5))
abline(lm(price ~ sqft, data=traditional_houses), col="red")  # Adding a regression line

# Yes

# Q10

# 	Generate a histogram of the natural log of house prices, i.e. ln⁡(price). Use this to answer the following questions. 
# Now is the data skewed?

hist(traditional_houses$price)
# right

# Q11 failed
# Based on your answer about any apparent skew after taking the natural log 
# of the price variable, do you believe you may have to further transform your data 
# to meet the assumptions required to build a regression model?  (Yes/No) 
  
# yes x
# no

#Q12 
# 	Generate a histogram of the natural log of house prices, i.e. ln⁡(price). Use this to answer the following questions. 

log_prices <- log(traditional_houses$price)

# Histogram of the natural log of house prices
hist(log_prices, 
     main="Histogram of Natural Log of House Prices", 
     xlab="Natural Log of House Price", 
     col="lightblue", 
     border="black", 
     breaks=30) # Number of bins

# Q13
# b.	Based on your answer about any apparent skew after taking the natural log of the price variable, do you believe you may have to further transform your data to meet the assumptions required to build a regression model?  (Yes/No)   
# No

# Compute the natural log of the price
traditional_houses$log_price <- log(traditional_houses$price)
# 1. Linearity Check: Scatter plot of ln(price) against sqft
ggplot(traditional_houses, aes(x=sqft, y=log_price)) +
  geom_point(alpha=0.6) +
  ggtitle("Scatter Plot of Natural Log of Price vs. Sqft") +
  xlab("House Size (sqft)") +
  ylab("Natural Log of House Price") +
  theme_minimal()

# 2. Normality Check: Histogram of Residuals and Q-Q plot
model_ln <- lm(log_price ~ sqft, data=traditional_houses)
residuals_ln <- resid(model_ln)

# Histogram of Residuals
hist(residuals_ln, main="Histogram of Residuals", xlab="Residuals", col="lightblue", border="black", breaks=30)

# Q-Q plot
qqPlot(model_ln, main="Q-Q Plot of Residuals")

# 3. Homoscedasticity Check: Residuals vs. Fitted (Predicted) values
plot(model_ln, which=1)  # This will generate the residuals vs fitted plo

# c.	Which assumption could be violated?
# i.	Linearity <-?
# ii.	Normality
# iii.	Homoscedasticity
# iv.	Independence

# 7.	Create a scatter plot of the natural log of house price versus house size for traditional style homes that are owner occupied.  Is the relationship between price and size linear now?  (Yes/No)    
# Q15
head(data)
traditional_houses_occup <- subset(traditional_houses, owner == 1)

ggplot(traditional_houses_occup, aes(x=sqft, y=log_price)) +
  geom_point(alpha=0.6) +
  ggtitle("Scatter Plot of Natural Log of Price vs. Sqft") +
  xlab("House Size (sqft)") +
  ylab("Natural Log of House Price") +
  theme_minimal()

#Q16
# YEs

#Q17 How do these house prices vary with changes in size (change per square foot)?

# model <- lm(traditional_houses$price ~ traditional_houses$sqft)
# summary(model)

# Fit the linear model
model <- lm(price ~ sqft, data=traditional_houses)

# Print the coefficient for sqft (β1)
coef(model)[2]

# 73.77

# Q18: 
# No
summary(model)

#Q19 failed
#-28407.56 x
# # 6.871e+04


# 	Generate a quadratic model for this situation, that is price= α_0+α_1 〖sqft〗^2+e, and use this model to answer the following questions.  (Be sure to save the value of the sum of squares error (SSE) for this quadratic model.)  

#Q20
# Add a squared sqft column to the dataset
traditional_houses$sqft_squared <- traditional_houses$sqft^2

# Fit the quadratic model
model_quadratic <- lm(price ~ sqft_squared, data=traditional_houses)
summary(model_quadratic)

# a.	What is the intercept value?       
# 6.871e+04

#Q21 
# What is the coefficient of 〖sqft〗^2?        
# 1.206e-02

#Q22 
# Using the previously fitted quadratic model
alpha_1 <- coef(model_quadratic)["sqft_squared"]
marginal_effect_2000sqft <- 2 * alpha_1 * 2000

# Print the marginal effect
print(marginal_effect_2000sqft)

# 48.25

#Q23
# What is the expected price of the 2000square foot home?
alpha_0 <- coef(model_quadratic)["(Intercept)"]
expected_price_2000sqft <- alpha_0 + alpha_1 * (2000^2)

# Print the expected price
print(expected_price_2000sqft)

#116963

# Q24
# Compute the marginal effect for a home with 2000 sqft
alpha_1 <- coef(model_quadratic)["sqft_squared"]
marginal_effect_2000sqft <- 2 * alpha_1 * 2000

# Compute the expected price for a home with 2000 sqft
expected_price_2000sqft <- predict(model_quadratic, newdata=data.frame(sqft_squared=2000^2))

# Compute the elasticity
elasticity_2000sqft <- marginal_effect_2000sqft * (2000 / expected_price_2000sqft)

# Print the elasticity
print(round(elasticity_2000sqft,2))
# 0.83

# Q25
# Generate a scatter plot with both the linear and quadratic trend lines on it. Which seems to fit the data better?
# Quadratic fit

# Predictions from the models
traditional_houses$sqft_squared <- traditional_houses$sqft^2
traditional_houses$linear_pred <- predict(model, newdata=traditional_houses)
traditional_houses$quadratic_pred <- predict(model_quadratic, newdata=traditional_houses)

# Scatter plot with both trend lines
ggplot(traditional_houses, aes(x=sqft, y=price)) +
  geom_point(aes(color="Data Points")) +
  geom_line(aes(y=linear_pred, color="Linear Model")) +
  geom_line(aes(y=quadratic_pred, color="Quadratic Model")) +
  labs(title="House Price vs. House Size with Linear and Quadratic Trend Lines", 
       x="House Size (sqft)", 
       y="House Price", 
       color="Legend") +
  theme_minimal()

# Q26
# h.	Generate a plot of the residuals from both the linear and quadratic models.  Does homoscedasticity appear to be a problem?  (Yes/No)  
# Yes

# Residuals from the linear and quadratic models
linear_residuals <- resid(model)
quadratic_residuals <- resid(model_quadratic)

# Plot residuals for the linear model
ggplot(traditional_houses, aes(x=predict(model), y=linear_residuals)) +
  geom_point(aes(color="Linear Model Residuals")) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residuals for Linear Model", 
       x="Predicted Prices", 
       y="Residuals", 
       color="Legend") +
  theme_minimal()

# Plot residuals for the quadratic model
ggplot(traditional_houses, aes(x=predict(model_quadratic), y=quadratic_residuals)) +
  geom_point(aes(color="Quadratic Model Residuals")) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residuals for Quadratic Model", 
       x="Predicted Prices", 
       y="Residuals", 
       color="Legend") +
  theme_minimal()


# Q27
# Would this indicate that heteroscedascity or heteroskedascity is present in the data?

# Yes

#Q28
#Generate a log-linear model for this situation, that is ln⁡(price)= γ_0+γ_1 sqft+e, and use this model to answer the following questions.  
# a.	The house size in square feet is statistically significant  (Yes/No)    
# Yes
# Transform the price using natural logarithm
traditional_houses$ln_price <- log(traditional_houses$price)

# Fit the log-linear model
model_log_linear <- lm(ln_price ~ sqft, data=traditional_houses)

# Extract p-value for sqft
p_value_sqft <- summary(model_log_linear)$coefficients[2, 4]
print(p_value_sqft)

summary(model_log_linear)

# Q29
# YEs

#Q30
# No

#Q31 failed
# d.	Visually, the __________ model appears to be the best fit for the data.
# quadratic x 
# log

traditional_houses$linear_pred <- predict(model)
traditional_houses$quadratic_pred <- predict(model_quadratic)
traditional_houses$log_linear_pred <- exp(predict(model_log_linear))

summary(model) #  0.6387 
summary(model_quadratic) # 0.6761
summary(model_log_linear)
# Scatter plot with all trend lines
ggplot(traditional_houses, aes(x=sqft, y=price)) +
  geom_point(aes(color="Data Points")) +
  geom_line(aes(y=linear_pred, color="Linear Model")) +
  geom_line(aes(y=quadratic_pred, color="Quadratic Model")) +
  geom_line(aes(y=log_linear_pred, color="Log-linear Model")) +
  labs(title="House Price vs. House Size with Linear, Quadratic, and Log-linear Trend Lines", 
       x="House Size (sqft)", 
       y="House Price", 
       color="Legend") +
  theme_minimal()

# Q32
#　e.	Compare the sum of squares error (SSE) for each model and select the model listed below that actually results in the least error.  
# quadratic

# Calculate SSE for the linear model
sse_linear <- sum(resid(model)^2)

# Calculate SSE for the quadratic model
sse_quadratic <- sum(resid(model_quadratic)^2)

# Calculate SSE for the log-linear model
sse_log_linear <- sum(resid(model_log_linear)^2)

print(c(sse_linear, sse_quadratic, sse_log_linear))

# Q33
# 13.	Ultimately, the log-linear model results in higher house prices for very large houses.  (True/False)  
# True

#Q34
# 14.	Based on the results of the various tests for normality, ________ satisfy/satisfies the assumption of normality.  (Hint: these tests are based on the hypothesis that the data are normal to begin with, i.e. If the P-value is < 0.05 we must reject the null hypothesis.  In other words, when evaluating your results, keep in mind what it means to have a given hypothesis and the P-values you get from your results!)  
# none of them?

p_value_linear <- shapiro.test(resid(model))$p.value
p_value_quadratic <- shapiro.test(resid(model_quadratic))$p.value
p_value_log_linear <- shapiro.test(resid(model_log_linear))$p.value

print(c(p_value_linear, p_value_quadratic, p_value_log_linear))

# Q35
# None 
# QQ plots using ggplot2 in R
qqnorm(resid(model))
qqline(resid(model), col="red")
title("QQ Plot for Residuals of Linear Model")

qqnorm(resid(model_quadratic))
qqline(resid(model_quadratic), col="red")
title("QQ Plot for Residuals of Quadratic Model")

qqnorm(resid(model_log_linear))
qqline(resid(model_log_linear), col="red")
title("QQ Plot for Residuals of Log-linear Model")

# Q36
# No

# Q37
# 179779.41.
# Subset data based on 'owner' variable
owner_occupied_data <- subset(data, owner == 1)
vacant_rental_data <- subset(data, owner == 0)

# Calculate mean prices for both subsets
mean_price_owner_occupied <- mean(owner_occupied_data$price)
mean_price_vacant_rental <- mean(vacant_rental_data$price)
mean_price_owner_occupied
# Print the results
cat("Mean price for owner-occupied houses:", mean_price_owner_occupied, "\n")
cat("Mean price for vacant/rental houses:", mean_price_vacant_rental, "\n")

# Q38
# 131030.26

#Q39
#yes

# Q40
# 123370.84

# Fit the linear model for traditional houses
model <- lm(price ~ sqft, data=traditional_houses)

# Predict the price for a 2000 square foot house and get its 95% confidence interval
prediction <- predict(model, newdata=data.frame(sqft=2000), interval="confidence", level=0.95)
upper_limit <- prediction[3]

print(upper_limit)

# Q41
# 114901.83
lower_limit <- prediction[2]

print(lower_limit)

#Q42
# 1.562955e-130
prediction <- predict(model, newdata=data.frame(sqft=2000), interval="confidence", level=0.95)
lower_limit <- prediction[2]
upper_limit <- prediction[3]

# P-value for sqft
p_value_sqft <- summary(model)$coefficients["sqft", "Pr(>|t|)"]

cat("Lower Limit:", lower_limit, "\n") # 1.1490e+05
cat("Upper Limit:", upper_limit, "\n") # 1.2337e+05
cat("P-value for sqft:", p_value_sqft, "\n")

number <- 123370.8
formatted_number <- formatC(number, format = "e", digits = 4)
print(formatted_number)

# Q43
# True

# You've already fitted the quadratic model for traditional houses
# Calculate the marginal effect for 2000 sqft house
marginal_effect <- 2 * coef(model_quadratic)["sqft_squared"] * 2000
summary(model_quadratic)
# Standard error for the marginal effect
se_marginal_effect <- 2 * 2000 * summary(model_quadratic)$coefficients["sqft_squared", "Std. Error"]

# Calculate the t-statistic
t_statistic <- (marginal_effect - 75) / se_marginal_effect

# Get the p-value for one-tailed t-test
p_value <- pt(t_statistic, df=model_quadratic$df.residual)

cat("Marginal Effect:", marginal_effect, "\n")
cat("t-statistic:", t_statistic, "\n")
cat("p-value:", p_value, "\n")


# Q44
# True

# Q45
# 





