library(readr)
ContractData <- read_csv("ContractData.csv")

#QUESTION 1
#How many observations are there in the contract/road construction dataset?  

#31

#Q2
# How many independent variables are in the contract data/road construction dataset?  

# 3 or 4

#Q3 
# Fill in the exact variable name of the dependent variable in the contract data/road construction dataset.  Be careful, this field is case sensitive!  
  
#BIDStatusy

#Q4
#The possible values for the dependent variable, BIDStatusy are 0 or 1.  

# True

#Q5
# If BIDStatusy is coded with the number 1 that indicates the bid is a fixed bid.  If BIDStatusy is coded with the number 0 that indicates the bid is a competitive bid.  


# True

#Q6
# The reported mean value of BIDStatusy, 0.38710, indicates that (select the best choice to complete this sentence).
# there are more competitive bids than fixed bids.  
# there are no bids available in this dataset.  
# there are more fixed bids than competitive bids.  <-
# nothing.  You cannot tell anything about the bids from the mean of the variable BIDStatusy

#Q7
# The output of the logit model built for the contract data/road construction dataset indicates that the model is able to accurately predict _________ % of all cases (or observations).  Note that the answer is in a percentage from the model output.  

# gretl
#Next build a first model, i.e. model1
# ? model1 <- logit BIDStatusy 0 NumberofBiddersx1 \
# DifferenceBetweenWinningBidandE
# 
# model1: Logit, using observations 1-31
# Dependent variable: BIDStatusy
# Standard errors based on Hessian
# 
# coefficient  std. error    z       slope   
# ---------------------------------------------------------------
#   const                 1.42120    1.28677      1.104            
# NumberofBiddersx1    −0.755339   0.338802    −2.229  −0.154632 
# DifferenceBetwee~     0.112205   0.0513932    2.183   0.0229704
# 
# Mean dependent var   0.387097   S.D. dependent var   0.495138
# McFadden R-squared   0.447980   Adjusted R-squared   0.302985
# Log-likelihood      −11.42151   Akaike criterion     28.84302
# Schwarz criterion    33.14499   Hannan-Quinn         30.24536
# 
# Number of cases 'correctly predicted' = 26 (83.9%)
# f(beta'x) at mean of independent vars = 0.205
# Likelihood ratio test: Chi-square(2) = 18.5377 [0.0001]
# 
#            Predicted
#              0    1
#   Actual 0  17    2
#          1   3    9
# 
# model1 saved

#Q8
#3

#Q9
#A Type 2 error for the contract data/road construction logit model means that (select the best choice below to complete this sentence).  
# I choose 
# 3 "cases" incorrectly indicated that they were for competitive bids when in reality they were for fixed bids.  


#Q10
# there is insufficient evidence that the logistic regression model lacks fit. That is, consistent with the practice of failure to reject the null hypothesis when the P-value is greater than the level of significance we find the model (null hypothesis) has sufficient supporting evidence.



