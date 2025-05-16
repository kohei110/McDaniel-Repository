#R script to compare OLS and TSLS results
#
#To run this script you will need to install the following packages;
#AER, systemfit, ivreg, and modelsummary.
Fertility <- read.csv("Fertility.csv")
View(Fertility)


library(AER)
library(systemfit)
library(ivreg)
library(modelsummary)

attach(Fertility)
model_ols <- lm(weeksm1 ~ morekids)
summary(model_ols)

model2_ols <- lm(morekids ~ samesex)
summary(model2_ols)

model_iv <- ivreg(weeksm1 ~ morekids | samesex)
summary(model_iv)

model2_iv <- ivreg(weeksm1 ~ morekids + agem1 + black + hispan + othrace | samesex + agem1 + black + hispan + othrace)
summary(model2_iv)

#R has a nice package for graphically showing the results of these models.
#The package is "modelsummary" which we brought in through the library
#commands at the beginning of the script.  

m_list <- list(OLS = model_ols, IV = model_iv, IV2 = model2_iv)
msummary(m_list)
modelplot(m_list, coef_omit = "Intercept experience")

#Hausman test for endogeneity of regressors
#Find the P-value to reject or not reject the null hypotheis
cv_diff <- coef(model_iv) - coef(model_ols)
vc_diff <- vcov(model_iv) - vcov(model_ols)
x2diff <- as.vector(t(cv_diff) %*% solve(vc_diff) %*% cv_diff)
pchisq(x2diff, df=2, lower.tail = FALSE)

#Use the P-value computed in the usual way!

