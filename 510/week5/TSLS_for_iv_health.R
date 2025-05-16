#R script to compare OLS and TSLS results
#
#To run this script you will need to install the following packages;
#AER, systemfit, and modelsummary.
#setwd("~/Workspace/McDaniel-Repository/510/week5")

iv_health <- read.csv("iv_health.csv")
View(iv_health)
names(iv_health) <- c("medexpense",
  "healthinsu",
  "age",
  "female",
  "blackhisp",
  "income",
  "illnesses",
  "ssiratio",
  "lowincome",
  "firmsize",
  "firmlocation",
  "educyr",
  "private",
  "hisp",
  "marry",
  "vegood",
  "good",
  "fair",
  "poor",
  "poverty",
  "midincome",
  "msa",
  "priolist",
  "black",
  "logmedexpense",
  "age2",
  "logincome",
  "vgh",
  "fph")
# install.packages("AER")
# install.packages("systemfit")
# install.packages("ivreg")
# install.packages("modelsummary")
library(AER)
library(systemfit)
library(ivreg)
library(modelsummary)
attach(iv_health)
y1 <- cbind(logmedexpense)
y2 <- y2 <- cbind(healthinsu)
x1 <- cbind(illnesses, age, logincome)
x2 <- cbind(ssiratio)
x2alt <- cbind(ssiratio, firmlocation)
summary(y1)
summary(y2)
summary(x1)
summary(x2)

#Conduct an OLS regression
olsreg <- lm(y1 ~ y2 + x1)
summary(olsreg)

#Conduct a two-stage least squares regression
instreg <- ivreg(y1 ~ y2 + x1 | x1 + x2)
summary(instreg)

#Conduct a two-stage least squares regression step-by-step
olsreg1 <- lm(y2 ~ x1 + x2)
summary(olsreg1)
y2hat <- fitted(olsreg1)
olsreg2 <- lm(y1 ~ y2hat + x1)
summary(olsreg2)
#And you get the same result step-by-step that you did using the
#package
instreg_o <- ivreg(y1 ~ y2 + x1 | x1 + x2alt)
summary(instreg_o)


#Hausman test for endogeneity of regressors
#Find the P-value to reject or not reject the null hypotheis
cv_diff <- coef(instreg) - coef(olsreg)
vc_diff <- vcov(instreg) - vcov(olsreg)
x2diff <- as.vector(t(cv_diff) %*% solve(vc_diff) %*% cv_diff)
pchisq(x2diff, df=2, lower.tail = FALSE)

#Use the P-value computed in the usual way!


#What about a system of equations?  Let's see what happens if
#we if medical expenses not only depended on health insurance?
#What if health insurance depended on another variable or variables?
#In this case we need to define some variables that are exogenous
#to the second equation.
x12 <- cbind(illnesses)

#Now we want a different instrument than we used in the first equation.
#Setup the system of equations: 
x22 <- cbind(firmlocation)

eq1 <- y1 ~ y2 + x1 + x2
eq2 <- y2 ~ y1 + x12 + x22
inst <- ~x1 + x2 + x22
system <- list(eq1 = eq1, eq2 = eq2)

reg_tsls <- systemfit(system, "2SLS", inst = inst, data = iv_health)
summary(reg_tsls)
