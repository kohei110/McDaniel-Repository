library(tidyverse)
library(stats)
library(ggplot2)
# install.packages("car")
library(car)

data <- read.csv("cafeteria.csv")
# Fit a linear regression model
model <- lm(sales ~ price + adv_cost, data = data)

# Summary of the model
summary(model)

# Checking Linearity
# Scatter plots for sales vs price and sales vs adv_cost
par(mfrow=c(1,2))
plot(data$price, data$sales, main="Sales vs Price", xlab="Price", ylab="Sales")
plot(data$adv_cost, data$sales, main="Sales vs Adv_cost", xlab="Adv_cost", ylab="Sales")

# Checking Normality
# Q-Q plot for residuals
qqPlot(model, main="Q-Q Plot of Residuals")

# Checking Homoscedasticity
# Residuals vs Fitted plot
plot(model$fitted.values, resid(model), main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red", lty=2)


# 1.	Considering the descriptive statistics for the cafeteria.gdt data, this dataset does NOT have sufficient observations (or records) to build an OLS model without worry about sample size.  Therefore, we will have to modify the distributions used for tests of the model to compensate.  
# True (failed)
# False 

# 2. Thinking about consistent units and the listed units for sales, price and advertising costs (adv_cost), an adjustment may need to be made to values in price for a direct comparison to the other variables sales (revenue) and advertising cost (adv_cost).
# False(failed)
# True


# 3. The simple linear regression model for this situation is:
# sales revenue=β_0+ β_1 price+ β_2 adv_cost+ε
# True(failed)
#False

# 4.	Using the data in the data file cafeteria.csv, consider the descriptive statistics, frequencies, and develop a simple linear model to represent these data.  
# 	The assumption of normality is satisfied.
# true(failed)
# False

# 5.	The assumption of linearity is satisfied.
# true

# 6 The assumption of homogeneity or homoscedasticity is satisfied.
#true (failed)
# False

# 7.	Analyze the coefficients related to the explanatory (independent) variables.  
# all
summary(model)

# 8.	The value of the intercept of the regression line is _________.          
# 118.9136
# 9.	The coefficient of price is _________.            
# -7.9079     
# 10.	The coefficient of adv_cost is _________.         
# 1.8626     
# 11.	How much annual revenue will be generated if the average price per meal is $5.50 and the advertising costs are capped at $1,200?  In this case, be sure to round correctly to the dollar (USD).   (Hint: you’ve done this before when considering other variables and ensuring you are using consistent units, i.e. remember that the problem statement says that values are in 1,000’s of dollars (USD).)       
# Given values
given_price <- 5.50
given_adv_cost <- 1.2  # since values are in 1,000's of dollars


new_data <- data.frame(price = given_price, adv_cost = given_adv_cost)

# Predict sales using the existing model and the new data
predicted_sales <- predict(model, newdata = new_data)
predicted_sales <- predicted_sales * 1000
# Print the predicted sales
cat("Predicted Sales (in thousands):", predicted_sales)
# 77655.51
# R 
# Call:
#   lm(formula = sales ~ price + adv_cost, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.4825  -3.1434  -0.3456   2.8754  11.3049 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 118.9136     6.3516  18.722  < 2e-16 ***
#   price        -7.9079     1.0960  -7.215 4.42e-10 ***
#   adv_cost      1.8626     0.6832   2.726  0.00804 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.886 on 72 degrees of freedom
# Multiple R-squared:  0.4483,	Adjusted R-squared:  0.4329 
# F-statistic: 29.25 on 2 and 72 DF,  p-value: 5.041e-10


# Gretl
# ? m1<-ols sales const price adv_cost --vcv
# 
# m1: OLS, using observations 1-75
# Dependent variable: sales
# 
# coefficient   std. error   t-ratio   p-value 
# --------------------------------------------------------
#   const       118.914       6.35164     18.72     2.21e-29 ***
#   price        −7.90785     1.09599     −7.215    4.42e-10 ***
#   adv_cost      1.86258     0.683195     2.726    0.0080   ***
#   
#   Mean dependent var   77.37467   S.D. dependent var   6.488537
# Sum squared resid    1718.943   S.E. of regression   4.886124
# R-squared            0.448258   Adjusted R-squared   0.432932
# F(2, 72)             29.24786   P-value(F)           5.04e-10
# Log-likelihood      −223.8695   Akaike criterion     453.7390
# Schwarz criterion    460.6915   Hannan-Quinn         456.5151
# 
# Covariance matrix of regression coefficients:
#   
#   const         price      adv_cost
# 40.3433      -6.79506     -0.748421  const
# 1.2012    -0.0197422  price
# 0.466756  adv_cost

# 12. Determine the number of degrees of freedom for this problem.  (Hint: remember the formula df = N-K where N is the number of observations and K is the number of unknown coefficients.)  The number of degrees of freedom is _________.  
# 72

# 13&14.	The 95% confidence interval for price has a  
# a.	lower limit of _________ and,          
# b.	an upper limit of _________.          

# Extracting the coefficient and standard error for 'price'
coef_price <- coef(model)["price"]
std_err_price <- summary(model)$coefficients["price", "Std. Error"]

# Degrees of freedom for the t-distribution
df_t <- df.residual(model)

# Critical t-value for 95% CI
t_critical <- qt(0.975, df_t)  # Using 0.975 for upper tail in a two-tailed test

# Calculate the lower and upper bounds of the 95% CI for 'price'
lower_bound <- coef_price - t_critical * std_err_price
upper_bound <- coef_price + t_critical * std_err_price

cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")


# ? scalar bL = $coeff(price) - critical(t, $df, 0.025) * $stderr(price)
# Generated scalar bL = -10.0927
# ? scalar bU = $coeff(price) + critical(t, $df, 0.025) * $stderr(price)
# Generated scalar bU = -5.72303

# Based on the computed 95% confidence interval this means that if the price per meal is “decreased” by 10 cents or 0.10 that means that sales (revenue) would be increased by between _______ and,... 
# 
# (Hint: remember that the units are 1,000’s of dollars (USD).) 

lower_bound <- -10.0927
upper_bound <- -5.72303

# Calculate the increase in sales (in units of 1,000s of dollars) for a $0.10 decrease in price
increase_sales_lower_bound <- 0.10 * lower_bound
increase_sales_upper_bound <- 0.10 * upper_bound

cat("For a $0.10 decrease in price, sales would increase by between:", increase_sales_lower_bound, "and", increase_sales_upper_bound, "(in 1,000's of dollars)\n")

# 1009.27 (upper)
# 572.30 (lower)

# 17. 
#-7.215 

# 18.	Using the same hypothesis test or computing with gretl, the t-ratio for the advertising costs (adv_cost) is 
# 2.726  


# 19
# True

# 20
# true

# 21 
# false (failed)
# True

# 22
	# Assume a level of significance of α=0.05 and find the relevant critical value.  Treat the null hypothesis as an equality, i.e. H_0: β_2=1 (because that will be the limiting value) and find the critical value.  
	# The critical value for H_0: β_2=1 is __________.            
# 1.263

# 23
# Yes (failed)
# No
