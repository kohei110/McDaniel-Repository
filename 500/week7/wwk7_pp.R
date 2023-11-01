library(tidyverse)
library(stats)
library(ggplot2)
library(car)

setwd("~/Workspace/mcdaniel/500/week7")

data <- read.csv("example5.3.csv")
data <- data %>%
  mutate(
    x1sq = x1^2,
    x2sq = x2^2,
    x1x2 = x1*x2
  )

so_model <- lm(y ~ x1 + x2 + x1sq + x2sq + x1x2, data=data)
summary(so_model)

# 7.	BBQ1: NONE of the variables in the second-order model are statistically significant.  
# False

# 8.	BBQ2:  The R-squared value for the second-order OLS model is _________.       
# 0.9931

# 9.	BBQ3:  The R-squared value for the simple linear OLS model is ________.       
simple_ols <- lm(y ~ x1 + x2, data=data)
summary(simple_ols)
# 0.2115

# 10.	BBQ4:  Other than the intercept, which term or terms is/are statistically significant in the simple linear OLS model?
# x1

ols2 <- lm(y ~ x1 + x2 + x1x2, data=data)
ols3 <- lm(y ~ x1 + x2 + x1sq, data=data)
ols4 <- lm(y ~ x1 + x2 + x2sq, data=data)
ols5 <- lm(y ~ x1 + x2 + x1x2 + x1sq, data=data)
ols6 <- lm(y ~ x1 + x2 + x1x2 + x2sq, data=data)
summary(ols2)
# 11.	BBQ5:  The value of R-squared for the OLS model containing an interaction (x1*x2) term is _________.       
# 0.287

# 12.	BBQ6:  Which term or terms is/are statistically significant in this model?  Check all that apply.
# none


# 13.	BBQ7:  The value of R-squared for the OLS model containing a x1-squared term is _________.        
summary(ols3)
# 0.3378

# 14.	BBQ8:  Which term or terms is/are statistically significant in the simple linear OLS model?  Check all that apply.
# x1sq
#None


# 15.	BBQ9:  The value of R-squared for the OLS model containing a x2-squared term is _________.       \
summary(ols4)
#0.7914

#16.	BBQ10:  Which term or terms is/are statistically significant in ths model?  Check all that apply.
# check below
summary(ols4)


# 17 BBQ11: The value of R-squared for the OLS model containing interaction as well as the x1-squared and x2-squared terms is _________.  Hint: remember to include the linear terms for x1 and x2 as well as the x12 and x22 terms.
ols7 <- lm(y ~ x1 + x2 + x1sq + x2sq, data=data)
summary(ols7)

# 0.9176

# 18.	BBQ12:  Which term or terms is/are statistically significant in this model?  Check all that apply.
# check below
summary(ols7)

# 19.	BBQ13:  The value of R-squared for the OLS model containing interaction and x2-squared terms is _________.       
summary(ols6)

# 0.8668

# 20.	BBQ14:  Which term or terms is/are statistically significant in the simple linear OLS model?  Check all that apply.

# 21.	BBQ15:  Returning to the initial model including all terms (interaction and second-order terms), what is the expected quality of the product if the temperature is 85 degrees F and the pressure is 57 psi?  (Hint: what do you do with the interaction and second-order terms now?  Since x1=85 and x2=57, then x1x2=4845.  Similarly, x1sq=7225.  I’ll leave it to you to calculate x2sq. Once you have these values you predict 
# the point estimate the same way we did in the gretl exercise when we predicted the sales for price=$5.50 and adv_cost=$1200, without the conversion for units to $1,000 USD.)
new_data <- data.frame(
  x1 = 85,
  x2 = 57,
  x1sq = 85^2,
  x2sq = 57^2,
  x1x2 = 85 * 57
)

# Predicting the quality using the model
predicted_quality <- predict(so_model, newdata = new_data)

# Printing the predicted quality
print(predicted_quality)
# 94.63637 

# 22.	BBQ16:  Based on the data available and model developed this is a reasonable value for product quality.
# True


# 23.	BBQ17:  Now compute the expected value when the pressure is increased to 60 psi.  The computed product quality is _________.        

new_data2 <- data.frame(
  x1 = 85,
  x2 = 60,
  x1sq = 85^2,
  x2sq = 60^2,
  x1x2 = 85 * 60
)
# Predicting the quality using the model
predicted_quality2 <- predict(so_model, newdata = new_data2)

# Printing the predicted quality
print(predicted_quality2)

# 75.13704

# 24.	BBQ18:  How does the product quality change if the pressure is increased to 60 psi?  
# decreased


# 25.	BBQ19:  Based on the data available and model developed this is a reasonable outcome.  
# True

# 26.	BBQ20:  Consider the F-value and related P-value for testing the entire model.  Does the enter model with interaction and second-order terms appear to be a good model for this data?
summary(so_model)

# True


# 27.	BBQ21:  One way to determine whether or not all the terms (including the interaction and second-order terms) should be retained in the model is to compare the “complete” model with a reduced model.  The easiest way to complete this comparison is to open the model window for Model ml and record F(5,21) and its associated P-value.  Then open the model window for the Model ml2, the multivariable OLS model, and record F(2,24) and its associated P-value.  Based on your comparison of the complete and reduced models, should all terms in the complete model be retained?  (Hint: see section 4.13 and Example 4.11 in your textbook.)  
# Yes



monthly <- 700/12
monthly_base <- monthly * 1.3
monthly_fee <- monthly_base * 1.2
print(monthly_fee*0.75)