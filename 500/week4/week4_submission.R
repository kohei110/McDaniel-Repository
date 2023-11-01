library(tidyverse)

setwd("~/Workspace/mcdaniel/500/week4")

# Q1 Test the hypothesis that the mean birthweight is different from 7 lbs.
# The computed P-value (two-tailed) is___ Enter your answer rounded to 
# two decimal places.

data <- read.csv("Maternal_and_infant_health_survey2002.csv", sep="\t")

# first attempt failed
result <- t.test(data$birthweight, mu=7)
print(result$p.value) # 0.02075675

sample_mean = mean(data$birthweight)
sample_sd = sd(data$birthweight)
df = nrow(data)-1

denom = (sample_sd/sqrt(df))
t_statistics = (sample_mean - 7) / denom
q1_pval = 2 * pt(q=-t_statistics, df=df, lower.tail=FALSE)

# second attempt not submit yet
# [1] 0.02138623 not sure

# Q2 Based on the computed P-value, we the null hypothesis. 
# Enter your answer as either "reject" or "fail to reject" in all lower case letters.
# first attempt: failed to reject
# second attempt: reject should be ok

#Q3  Test the hypothesis that the mean birthweight is less than 6.5 lbs. 
# The computed P-value (two-tailed) is ___ 
# Enter your answer rounded to two decimal places.

result_q3 = t.test(data$birthweight, mu=6.5, alternative = "less")
print(paste("p-val", result_q3$p.value)) 

#first attempt: p-val 0.0207567543904099
#second attempt: p-val 0.852169773466657 should be ok

# Q4: Decision based on the p-value
# [1] "We fail to reject the null hypothesis." correct

 
if (result_q3$p.value < 0.05) {
  print("We reject the null hypothesis.")
} else {
  print("We fail to reject the null hypothesis.")
}

# Q5  Test the hypothesis that the proportion of female babies is different 
# from 0.50.  The computed P-value is ____. 
# Enter your answer rounded to two decimal places.

# H0: female_babie = 0.5
# H1: female_babie != 0.5

count_female = sum(data$gender == 0)
n <- nrow(data)

prop_female = count_female / n # 0.47

# result_q5 <- prop.test(count_female, n , p=0.5)
# print(paste("p-val is", round(result_q5$p.value,2)))


result_q5_no_correction <- prop.test(count_female, n, p=0.5, correct=FALSE)
print(paste("p-val without continuity correction is", round(result_q5_no_correction$p.value,2)))
#first attempt: [1] "p-val is 0.62" failed
# second attempt: [1] "p-val without continuity correction is 0.55"

# second attempt by gretl
# 帰無仮説: 母集団の比率 = 0.5
# 標本のサイズ: n = 100
# 標本における比率 = 0.47
# 検定統計量: z = (0.47 - 0.5)/0.05 = -0.6
# 両側p値 = 0.5485
# (片側 = 0.2743)



#Q6 
# first attempt: reject failed
# second attempt: failed to reject

# Q7 Create a new variable named "smoke that equals 0 if the mother did not smoke any cigarettes during pregnancy 
# and equals 1fi the mother smoked at least one cigarette during pregnancy. 
# Test the hypothesis that mean birthweight differs between smokers and non-smokers. The computed P-value is Enter your answer rounded to four decimal places.
data$smoke <- ifelse(data$smoking == 1, 0, 1)

# Test the hypothesis that mean birthweight differs between smokers and non-smokers
smokers <- data[data$smoke == 1,]$birthweight
non_smokers <- data[data$smoke == 0,]$birthweight

# first attempt
result_q7 <- t.test(x=smokers, y=non_smokers, paired=F)
# Print the p-value
print(paste("The p-value is:", result_q7$p.value)) # [1] "The p-value is: 0.0209160540043737"


# second attempt
t.test(smokers,non_smokers,var.equal=T,paired=F)
# [r1] "The p-value is: 0.0022

# Two Sample t-test
# 
# data:  smokers and non_smokers
# t = -3.1421, df = 98, p-value = 0.002219
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.0806105 -0.4698173
# sample estimates:
#   mean of x mean of y 
# 5.557863  6.833077 

# Q8 Based on the computed P-value, we the null hypothesis. Enter your answer as either 'reject" or"fail to reject" in all lower case letters without the quotation marks.
# first attempt: failed to reject Failed
# second attempt: reject should be ok


# Q9 Test the hypothesis that the variables "gender" and "induced" are related (i.e. not independent). 
# The computed P-value is ____ Enter your answer rounded to two decimal places.
# HINT: The way you setup and conduct this hypothesis test is different! fI you are not sure, go back and review your lecture pdf files and your Software Presentation for this week!
  
table_data <- table(data$gender, data$induced)
result_q9 <- chisq.test(table_data)
print(paste("The p-value is:", result_q9$p.value)) 

result_q9

expected <- chisq.test(table_data)$expected
print(expected)

chi_squared <- sum((table_data - expected)^2 / expected)
print(chi_squared)

df <- (nrow(table_data) - 1) * (ncol(table_data) - 1)

p_value <- 1 - pchisq(chi_squared, df) #????
p_val_submit <- pchisq(chi_squared, df)
print(p_val_submit) 
print(p_value)

#first attempt [1] "The p-value is: 0.284891594863187" failed
# second attempt  0.16
# カイ二乗(1): 1.93091 = 0.164658 の右側の領域
# (to the left: 0.835342)

  
#Q10 Based on the computed P-value, we the null hypothesis. Enter your answer as either "reject" or "fail to reject" in all lower case letters.
# failed to reject CORRECT


#Q11 Test the hypothesis that mean birthweight differs across mother's race. 
# The computed P-value is ____.  
# Enter your answer rounded to two decimal places.

# Perform one-way ANOVA to test if mean birthweight differs across mother's race
result_q11 <- aov(birthweight ~ as.factor(race), data=data)
# Extract the p-value from the ANOVA result
p_value_q11 <- summary(result_q11)[[1]]["as.factor(race)", "Pr(>F)"]
# Print the p-value
print(paste("The p-value is:", p_value_q11))

summary(result_q11)[[1]] # 0.05 is correct

# Q12 Based on the computed P-value, we ____ the null hypothesis.  
# Enter your answer as "reject" or "fail to reject" in all lower case letters without the quotation marks.
#first attempt: fail to reject 
#second attempt: reject