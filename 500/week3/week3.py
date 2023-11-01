import numpy as np
import pandas as pd
from scipy.stats import t


# Question 1
# You have been asked to calculate a 95% confidence interval for a data sample.
# You know the mean of this sample but you do not know the population standard deviation.
# The probability distribution you should use in your calculations is the

# Answer: t-distribution


# Question 2
# Using the data below, calculate the 95% confidence interval. Round your answers to two decimal places.
# What is the lower bound for the confidence interval
gpa_data = [3.3, 2.9, 3, 3.1, 2.7, 2.6, 4, 3.8, 2.8, 3.6]

sample_std = np.std(gpa_data, ddof=1)
sample_mean = np.mean(gpa_data)
sample_size = len(gpa_data)

se = sample_std / np.sqrt(sample_size)
df = sample_size - 1
tvalue = t.ppf(0.975, df)

ci_95_lower = sample_mean - tvalue * se
ci_95_upper = sample_mean + tvalue * se

print("Lower bound for the confidence interval: ", round(ci_95_lower, 2))
print("Upper bound for the confidence interval: ", round(ci_95_upper, 2))
# Lower bound for the confidence interval:  2.84
# Upper bound for the confidence interval:  3.52

tvalue = t.ppf(0.95, df)

ci_90_lower = sample_mean - tvalue * se
ci_90_upper = sample_mean + tvalue * se

print("Lower bound for the confidence interval: ", round(ci_90_lower, 2))
print("Upper bound for the confidence interval: ", round(ci_90_upper, 2))
# Lower bound for the confidence interval:  2.9
# Upper bound for the confidence interval:  3.46

ci_range_90 = ci_90_upper - ci_90_lower
ci_range_95 = ci_95_upper - ci_95_lower

print(ci_range_90, ci_range_95)
# p95 is wider than p90


# Question 8
# Many students misinterpret a confidence interval, i.e. a 90% confidence interval,
# to mean that there is confidence that 90% of the data lie within this interval.
# That is not true. Fill in the blank in the following sentence to complete what is accurately
# meant in expressing a confidence interval for an analysis involving the true population mean of students' GPA.
# We estimate with 90% confidence that _____ lies between the lower bound of the confidence interval and the upper bound of the confidence interval.

# true population mean?

# Question 9
# Fill in the blanks in the following sentences about the differences between the Normal distribution and the Student-t distribution.

# The Student-t distribution has more ____  in its tails than the Normal distribution. You do not need to know the _____ standard deviation to use
# the Student-t distribution. Graphs of the Normal distribution and the Student-t distribution look very similar. However, the exact shape of the
# Student-t distribution depends on the ____. As the number of degrees of freedom increases the Student-t distribution becomes more and more like the Normaldistribution.

# probability
# population
# degrees of freedom

# Question 10
# If you have a binomial distribution you are dealing with a proportion problem.
# True

# Question 11
# 40%

# Question 12
# 4

# Question 13
# An _____ hypothesis typically makes a claim about a population that contradicts the null hypothesis.
# We generally do not say anything about the alternative hypothesis. The conclusion to an analysis
# normally says that we ___ or ____ the null hypothesis.
# A null hypothesis always will be set up with and ____ sign in it. The alternative hypothesis is set up with a ____ sign in it.

# alternative
# reject
# fail to reject
# H0
# H1

# Question 14
# This question assesses your understanding of Type1 and Type2 errors. A Type 1 error occurs when the decision is ____
# the null hypothesis even though it is true. This is sometimes called a false negative.
# A Type 2 error occurs when the decision is ____ the null hypothesis when it is really false. This is sometimes called a false positive.
# Here are two examples of these types of errors.

# An emergency crew thinks an accident victim is deceased when, in fact, he is acttually alive. This is a ____ error.
# An emergency crew thinks an accident victime is still alive when, in fact, she has already died. This is a ____ error.

# Alpha is the probability of Type1 error. Beta is the probability of Type2 error. The power of the test is 1 - ____.
# Note that increasing the sample size can increase the Power of the test

# to reject
# not to reject
# type 1
# type 2
# beta
