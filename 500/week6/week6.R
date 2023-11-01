setwd("~/Workspace/mcdaniel/500/week5")
data <- read.csv("correctHousing.csv")
small_data <- read.csv("smallHousingSample.csv")
#head(data)
nrow(small_data)

# Q1 Failed

#1.	The intercept value, -41.56, represents where the regression line would intercept the y-axis at x=0.  
#The question is, “What is that home value in whole USD dollars (no cents) _____?” 
#If you need to be sure to include the appropriate sign for this value.  
#(Hint: think about the axes of the 2-D regression line and what they represent.)

#s -41
#f-41.56 

#2.	Is the home value in Question 1 a realistic home value?  Yes or No?

# No

# Failed 
# 3.	How much do home values increase for an increase of an additional (one) room?  
# Enter whole USD dollars (no cents)?          

# s 11
#f 10

#failed
# 4.	How much do home values increase for each year beyond 1940?        

# s 0.03
# f -0.03

# 5.	How much do home values increase for each 10,000 USD increase in the tax rate?         
# -0.01

# 6.	It makes sense that, in questions 4 and 5, home values actually decrease 
# or have a negative slope coefficient as the age of the home increases and/or 
# the property-tax rate increases.  True or False?

# True

# 7.	Home values _________ when K-12 Pupil-teacher ratios increase.  
# Enter either increase or decrease.            

# increase

# 8.	Analogous to simple linear regression, if the data contain substantially more data points than the number of parameters (independent variables) the R-squared value for a multivariable linear regression model indicates how well the model fits the data.  Yes or No?         
# yes

# 9.	Enter the number of observations (in this last model with limited number of independent variables RM, AGE, TAX and PTRATIO).         
# 374

# 10.	Enter the number of independent variables.         
# 4

#11.	The adjusted R-squared value is _________.  (Hint: Be careful here because it seems pretty simple.  I calculated it incorrectly the first time and caught that when I checked it against the gretl output!)     
# 0.799524

# 12.	Consider an F-test to verify the overall utility of our multivariable linear regression model for home values.  Based on the gretl output our P-value is 3.5 e-128 or incredibly small, very near zero.  Therefore we cannot reject the null hypothesis.  This test has proven that the model does not have overall utility.  (Hint: if this is confusing read through your second textbook Section 4.6 including Example 4.3)  True or False?    
# False

# 13. Given the results of an F-test verifying a multivariable linear regression model’s overall utility we can also conclude that the model is the best model that can be built.  (Hint: this is covered in the second textbook same reference pages as for question 12.)  True or False?
# False

# Failed
# 14.	Conduct the appropriate test to evaluate whether or not the “major variable” in our multivariable linear regression model for home values is statistically significant.  The value of the statistic for that test is _________.   (Hint: this is discussed in your second textbook in Section 4.7.  However, in reality you will probably never do this by hand.  gretl already did this computation for you/us…  What does gretl give for the associated P-value?)         
# s 25000

# failed
# 16.	Is the result you computed for question 15 more than the mean of the dependent variable in our current multivariable linear regression model?  Yes or No.  (Hint: look through your gretl output!)  
# s Yes
# f No


