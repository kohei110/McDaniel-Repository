small_data <- read.csv("smallHousingSample.csv")
full_data <- read.csv("correctHousing.csv")

nrow(full_data)

## Question 1 
# Considering descriptive statistics for the small Boston housing dataset, 
# was the mean home value ___ greater than, equal to or less than the computed 
# home value using the larger dataset? 
# Enter greater than, equal to, or less than. 

# less than

mean(small_data$CMEDV)
mean(full_data$CMEDV)


## Question 2
# Considering descriptive statistics, was the mean (average) number of rooms greater than, equal to, or less than 
# the computed average number of rooms using the larger dataset? 

mean(small_data$RM)
mean(full_data$RM)

#Enter greater than, equal to, or less than 
# greater than

## QUESTION 3 
# Was the correlation between home values and the average number of roams greater than, equal to, or less than 
# the correlation value you computed using the larger dataset? 
cor(small_data$CMEDV, small_data$RM)
cor(full_data$CMEDV, full_data$RM)
# Enter greater than, equal to or less than 

# greater than

## QUESTION 4
# Was the intercept value with this smaller sample greater than, equal to, or less than the intercept value 
# you obtained in your greti assignment using the larger dataset? 
model_small <- lm(CMEDV ~ RM, data=small_data)
model_full <- lm(CMEDV ~ RM, data=full_data)

summary(model_small)
summary(model_full)

# Enter greater than, equal to, or less than 
# less than

## QUESTION 5 
# Was the coefficient of the slope greater than, equal to, or less than the value obtained from the larger dataset? 
# Enter greater than, equal to, or less than 

# greater than

## QUESTION 6 
# I have read the text in the Word notes document. 

# yes

## QUESTION 7
# Interpret the estimated slope coefficient you computed from your reduced sample data file. 
# Enter the home value you compute if you have 10 rooms. 

# Note that everyone will get a different answer here too! 
# I got 58.31 as my (estimated) home value which is more than double the mean value of CMEDV. 
# Enter my value for the mean CMEDV to get credit for this question.
# 64.22
new_data <- data.frame(RM = 10)
predicted_CMEDV <- predict(model_small, newdata = new_data)
predicted_CMEDV

# QUESTION 8 
# This question is very long in the Word notes document. The actual question is in bold font. 
# For the simple dataset we're using the answer to this question you should get is yes. 

# After you have read and thought shot this question enter your answer to the question. 
# Are all the requirements and assumptions far conducting this OLS analysis satisfied? 

# yes

## QUESTION 9 
# Using the data and analysis I have shared answer the following question. 
# The correlation coefficient between the rate of crime represented by the variable trim 
# and the percentage of lower socio-economic station population represented by the variable lstat is ___?

cor(small_data$crim,small_data$lstat)
# 0.67

## QUESTION 10
# Using the data and analysis I have shared determine if the following statement is true or false. 
# The value of this correlation coefficient indicates that a strong relationship exists between the variables trim and Istat

# True or False 
# False

## QUESTION 11 
# Again. using the data and analysis I have shared, what is the r-squared value computed? 
# Be sure you are continuing to round your answers to two decimal places! 
# 0.76

## QUESTION 12 10 points SaveAnswer 
# Using the data and analysis I have shared, determine whether or not this r-squared value indicates 
# that the OLS model explains most of the variability in the data.

# Yes
