library(ggplot2)
setwd("/Users/nkohei/Workspace/mcdaniel/500/week5")

data <- read.csv("correctHousing.csv")

## Question1 
# Of the variables of interest, RM is the ____ variable and CMEDV is the ___ variable. Enter either independent or dependent 

# independent 
# dependent

## Question2 Failed
# The relationship between the variables CMEDV and RM appears to be linear. 

# False
# True 
# Plotting a scatter plot between CMEDV and RM
ggplot(data, aes(x=RM, y=CMEDV)) + 
  geom_point(alpha=0.5) + 
  ggtitle('Scatter plot between CMEDV and RM') +
  xlab('RM') +
  ylab('CMEDV') +
  theme_minimal()

# Compute the Pearson correlation coefficient between CMEDV and RM
correlation_coefficient <- cor(data$CMEDV, data$RM, method="pearson")
print(paste("Pearson correlation coefficient:", correlation_coefficient))
 
## Question3
# What is the mean value of CMEDV? 

mean(data$CMEDV) #[1] 25.01


## Question4
# What is the mean value of RM?
mean(data$RM) #[1]  6.37

## Question5
# What is the interquartile range of CMEDV? 

q1 <- quantile(data$CMEDV,0.25)
q3 <- quantile(data$CMEDV,0.75)
q3-q1 # 13.88 


## QUESTION 6 
# What is the value of the standard deviation of RM?
sd(data$RM) # [1] 0.81

## QUESTION 7 
# Calculate the correlation coefficient between the variables of interest, 
# CMEDV and RM. What is the value you calculated for the correlation coefficient? 

cor(data$CMEDV, data$RM) # [1] 0.76

## QUESTION 8
# The correlation coefficient is a measure of the strength of the relationship between two variables. 
# False

## QUESTION 9 
# Residuals are the difference between the values of independent variables at different points in time. 
# False?

## QUESTION 10 
# The least squares method of regression to find the line best fitting the data minimizes the (select the best answer below). 
# - Sum of the difference of independent variables squared 
# - Sum of the dependent variable squared 
# - Sum of squared residuals <-
# - Sum of residuals 

## QUESTION 11 
# What are the assumptions required for conducting a linear regression. Select all that apply. 

# - Normality, i.e. the residuals are normally distributed. 
# - Homoscedasticity, i.e. residuals are roughly equal and scattered about zero. 
# - The standard deviations of the dependent variable vary over time resulting in heteroscedasticity. 
# - At least one of the independent variables depends on other independent variables.
# - There are a number of explainable outliers in the data resulting in heteroscedasticity. 
# - Linearity, i.e. the relationship between dependent variable and independent variable(s) is linear. 
# - The correlation coefficient between variables equals zero. 
# - The number of observations is small and the data follow a Student's t-d istributi on. 
# - Independence, i.e. observations are independent. 

# Normality
# Homoscedasticity
# Linearity
# Independence

## QUESTION 12 
# Estimate a simple linear regression model using least squares using the OLS command in gretl as shown in your handout. 
# The estimated regression equation is Y = — 26 — 8.00x  
# True 


## R script example
# Perform linear regression using OLS
model <- lm(CMEDV ~ RM, data=data)

# Print summary statistics of the model
summary(model)

# Plot the regression line on the scatter plot
ggplot(data, aes(x=RM, y=CMEDV)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm", color="blue") + 
  ggtitle('Regression of CMEDV on RM') +
  xlab('RM') +
  ylab('CMEDV') +
  theme_minimal()

# Residual diagnostics
plot(model, which=1:4)


## QUESTION 13 
# Based on the estimated regression you obtained before, what is the value of the slope coefficient? 
# 8.00

## QUESTION 14 
# The estimated slope coefficient tells you how much the dependent variable, in this case home value, 
# varies with changes in the independent variable, in this case the average number of rooms in owner-occupied homes. 
# True

## QUESTION 15 
# Is the estimated slope coefficient statistically significant? Enter yes or no
# yes

## QUESTION 16 
# In this case, the P-value equals 4.52e-073 ...or something very, very small and much smaller 
# than the designated 0.05 level of significance. 
# True 


## QUESTION 17 
# The coefficient of determination or r-squared, is a measure of how much of 
# the variability in the data is explains the response, i.e. the dependent variable. 
# True

## QUESTION 18 The value of r-squared for our current model is? 
# 0.59

## QUESTION 19 
# At what point or value is the coefficient of determination or r-squared considered a strong indicator? 
# The truth is that a good value for r-squared depends on what the model you are developing is intended to do. 
# If the model is intended to represent a lot of engineering or technical applications then
# usually somewhere Between 0.50 and 0.70 is considered good. 


# However, if you are developing a model for a final consumer product where safety is involved 
# you'll want a much higher r-squared, e.g. 0.90 or even a lot higher than that. 
# Most basic R&D projects are good with an r-squared value of around 0.2. 
# In this case, r-squared is only intended to give enough confidence to refine something to 
# the next step or phase which should have a higher r-squared. 
# In the social sciences, r-squared from 0.10 to 0.30 is often considered good. 
# 5o, it depends... 

# 0.90
# 0.70 
# It depends <-
# 0.50 


## QUESTION 20 
# Calculate a 95% confidence Interval for the estimated slope coefficient. 
# What is the value of the lower bound for the 95% confidence interval of the slope coefficient? 
# -0.89 # from doc data
# 7.44 from sample data 

# t_values <- summary(model)$coefficients[, "t value"][2]
# stderr <- summary(model)$coefficients[, "Std. Error"][2]
# coeff <- summary(model)$coefficients[, "Estimate"][2]
# 
# ub <- coeff + (t_values * stderr)
# lb <- coeff + (t_values * stderr)

# Compute 95% confidence interval for the RM coefficient
conf_interval <- confint(model, "RM", level=0.95)

# Print the confidence interval
print(conf_interval) 


## Question 21 
# Calculate a 95% confidence interval for the estimated slope coefficient. 
# What is the value of the upper Pound for the 96% confidence interval of the slope coefficient?
# -0.27 # from doc data
# 8.43

