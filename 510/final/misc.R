library(ggplot2)
library(reshape2)

# Load the data
data <- read.csv("milner.csv")

model <- lm(Sales ~ AdvExp, data = data)

# Get the model summary
summary(model)

# Calculate SST, SSE, and SSR
y <- data$Sales
y_hat <- fitted(model)
y_bar <- mean(y)

SST <- sum((y - y_bar)^2)
SSE <- sum((y - y_hat)^2)
SSR <- SST - SSE

# Print the results
cat("SST:", SST, "\n")
cat("SSE:", SSE, "\n")
cat("SSR:", SSR, "\n")



# Extracting R-squared and Adjusted R-squared from the model summary
R2 <- summary(model)$r.squared
Adjusted_R2 <- summary(model)$adj.r.squared

# MSE (Mean Squared Error)
# MSE <- mean(residuals(model)^2)

# Calculating MSE
n <- nrow(data)  # Number of observations
p <- length(coefficients(model))  # Number of parameters (including the intercept)
MSE <- SSE / (n - p)

MSE 
# Standard deviation of the independent variable (AdvExp)
Std_Dev_AdvExp <- sd(data$AdvExp)

# Confidence interval for the slope
conf_int <- confint(model, "AdvExp", level = 0.95)

# Printing results, rounded to two decimal places
cat(sprintf("Total Sum of Squares (SST): %.2f\n", SST))
cat(sprintf("Sum of Squares due to Error (SSE): %.2f\n", SSE))
cat(sprintf("Sum of Squares due to Regression (SSR): %.2f\n", SSR))
cat(sprintf("R-squared (R2): %.2f\n", R2))
cat(sprintf("Adjusted R-squared (Adjusted_R2): %.2f\n", Adjusted_R2))
cat(sprintf("Mean Squared Error (MSE): %.2f\n", MSE))
cat(sprintf("Standard Deviation of Advertising Expense (AdvExp): %.2f\n", Std_Dev_AdvExp))
cat(sprintf("Confidence Interval for the slope of AdvExp: [%.2f, %.2f]\n", conf_int[1], conf_int[2]))


# Fit the unrestricted multivariable linear regression model
# unrestricted_model <- lm(Sales ~ Accounts + AdvExp + Poten + Share, data = data)
# Fit the unrestricted model
unrestricted_model <- lm(Sales ~ Time + Poten + AdvExp + Share + Change + Accounts + Work + Rating, data = data)

# Fit the restricted model (intercept only)
restricted_model <- lm(Sales ~ 1, data = data)

# Perform the F-test
f_test <- anova(restricted_model, unrestricted_model)
f_statistic <- f_test[2, "F value"]

# Summary of the unrestricted model for a detailed look
summary_unrestricted <- summary(unrestricted_model)

# Printing F-statistic from the unrestricted model summary
cat(sprintf("F-statistic for the unrestricted model: %.2f\n", summary_unrestricted$fstatistic[1]))

# 
# df1 <- summary_unrestricted$df[2]  # Degrees of freedom for the model (numerator)
# df2 <- summary_unrestricted$df[1]  # Degrees of freedom for residuals (denominator)
# 
# # Determine F-critical at alpha = 0.05
# # f_critical <- qf(0.95, df1, df2)
# f_critical <- qf(0.95, df1, df2)
# 
# # Printing results
# cat(sprintf("Degrees of freedom for the model: %d\n", df1))
# cat(sprintf("Degrees of freedom for residuals: %d\n", df2))
# cat(sprintf("F-critical at 0.05 significance level: %.2f\n", f_critical))

# Degrees of freedom for the model: Number of predictors
df1 <- length(coef(unrestricted_model)) - 1  # Subtract 1 to exclude the intercept

# Degrees of freedom for residuals: Total observations minus the number of coefficients (predictors + intercept)
df2 <- nrow(data) - length(coef(unrestricted_model))

# Determine F-critical at alpha = 0.05
f_critical <- qf(0.95, df1, df2)  # 95% percentile

# Printing degrees of freedom and F-critical value
cat(sprintf("Degrees of freedom for the model: %d\n", df1))
cat(sprintf("Degrees of freedom for residuals: %d\n", df2))
cat(sprintf("F-critical at 0.05 significance level: %.2f\n", f_critical))



data <- read.csv("diabetes.csv")

# Display the structure of the dataset
str(data)

# Print the number of observations
cat("Number of observations:", nrow(data), "\n")

# Assuming the last column is the dependent variable
# Print the number of independent variables
cat("Number of independent variables:", ncol(data) - 1, "\n")

# Calculate the correlation matrix for the dataset
cor_matrix <- cor(data[, -ncol(data)])  # Assuming the last column is the dependent variable

# Print the correlation matrix
print(cor_matrix)

# Calculate the correlation matrix for the dataset
cor_matrix <- cor(data[, -ncol(data)])  # Assuming the last column is the dependent variable

# Convert the correlation matrix into a format suitable for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Create a heatmap of the correlation matrix
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables")


# Calculate the correlation matrix for the dataset
cor_matrix <- cor(data)  # Assuming the last column is the dependent variable

# Convert the correlation matrix into a format suitable for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Create a heatmap of the correlation matrix
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables")



diabetes <- read.csv("diabetes.csv")

# Function to check normality using Shapiro-Wilk test and histograms
check_normality <- function(variable) {
  shapiro_test <- shapiro.test(variable)
  cat("Shapiro-Wilk normality test for", deparse(substitute(variable)), "\n")
  cat("p-value:", shapiro_test$p.value, "\n\n")
  
  hist(variable, main = paste("Histogram of", deparse(substitute(variable))))
}

# Check normality for each variable
check_normality(diabetes$Pregnancies)
check_normality(diabetes$Glucose) # normality from viz?
check_normality(diabetes$BloodPressure)# normality from viz??
check_normality(diabetes$SkinThickness)
check_normality(diabetes$Insulin)
check_normality(diabetes$BMI) # normality from viz?
check_normality(diabetes$DiabetesPedigreeFunction)
check_normality(diabetes$Age)
check_normality(diabetes$Outcome)



# Create scatterplots of Outcome against each independent variable
par(mfrow = c(3, 3))  # Create a 3x3 grid of plots

plot(diabetes$Pregnancies, diabetes$Outcome, main = "Pregnancies", xlab = "Pregnancies", ylab = "Outcome")
plot(diabetes$Glucose, diabetes$Outcome, main = "Glucose", xlab = "Glucose", ylab = "Outcome")
plot(diabetes$BloodPressure, diabetes$Outcome, main = "BloodPressure", xlab = "BloodPressure", ylab = "Outcome")
plot(diabetes$SkinThickness, diabetes$Outcome, main = "SkinThickness", xlab = "SkinThickness", ylab = "Outcome")
plot(diabetes$Insulin, diabetes$Outcome, main = "Insulin", xlab = "Insulin", ylab = "Outcome")
plot(diabetes$BMI, diabetes$Outcome, main = "BMI", xlab = "BMI", ylab = "Outcome")
plot(diabetes$DiabetesPedigreeFunction, diabetes$Outcome, main = "DiabetesPedigreeFunction", xlab = "DiabetesPedigreeFunction", ylab = "Outcome")
plot(diabetes$Age, diabetes$Outcome, main = "Age", xlab = "Age", ylab = "Outcome")

model <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = diabetes, family = binomial)
summary(model)

# Fit models with each independent variable
model_Pregnancies <- glm(Outcome ~ Pregnancies, data = diabetes, family = binomial)
model_Glucose <- glm(Outcome ~ Glucose, data = diabetes, family = binomial)
model_BloodPressure <- glm(Outcome ~ BloodPressure, data = diabetes, family = binomial)
model_SkinThickness <- glm(Outcome ~ SkinThickness, data = diabetes, family = binomial)
model_Insulin <- glm(Outcome ~ Insulin, data = diabetes, family = binomial)
model_BMI <- glm(Outcome ~ BMI, data = diabetes, family = binomial)
model_DiabetesPedigreeFunction <- glm(Outcome ~ DiabetesPedigreeFunction, data = diabetes, family = binomial)
model_Age <- glm(Outcome ~ Age, data = diabetes, family = binomial)

# Extract p-values for each variable
p_values <- c(
  summary(model_Pregnancies)$coefficients[2, 4],
  summary(model_Glucose)$coefficients[2, 4],
  summary(model_BloodPressure)$coefficients[2, 4],
  summary(model_SkinThickness)$coefficients[2, 4],
  summary(model_Insulin)$coefficients[2, 4],
  summary(model_BMI)$coefficients[2, 4],
  summary(model_DiabetesPedigreeFunction)$coefficients[2, 4],
  summary(model_Age)$coefficients[2, 4]
)

# Assign names to the p_values vector
names(p_values) <- c(
  "Pregnancies",
  "Glucose",
  "BloodPressure",
  "SkinThickness",
  "Insulin",
  "BMI",
  "DiabetesPedigreeFunction",
  "Age"
)

# Find the variable with the lowest p-value
best_single_variable <- names(p_values)[which.min(p_values)]
cat("The best single variable model is based on:", best_single_variable)

#Q32

# Perform forward-selection procedure
model_forward <- glm(Outcome ~ 1, data = diabetes, family = binomial)

for (i in 1:ncol(diabetes) - 1) {
  # Fit models with each remaining independent variable
  models <- lapply(setdiff(names(diabetes), c("Outcome", names(model_forward$coefficients)[-1])), function(var) {
    formula <- as.formula(paste("Outcome ~", paste(c(var, names(model_forward$coefficients)[-1]), collapse = " + ")))
    glm(formula, data = diabetes, family = binomial)
  })
  
  # Extract p-values for each variable
  p_values <- sapply(models, function(model) {
    summary(model)$coefficients[2, 4]
  })
  
  # Find the variable with the lowest p-value
  best_variable <- names(p_values)[which.min(p_values)]
  
  # Update the model with the selected variable
  formula <- as.formula(paste("Outcome ~", paste(c(best_variable, names(model_forward$coefficients)[-1]), collapse = " + ")))
  model_forward <- glm(formula, data = diabetes, family = binomial)
  
  # Print the selected variable and its t-ratio
  t_ratio <- summary(model_forward)$coefficients[2, 3]
  cat("Step", i, ": Selected variable:", best_variable, "| t-ratio:", t_ratio, "\n")
}

model_forward <- glm(Outcome ~ Glucose, data = diabetes, family = binomial)
coefficients(model_forward)


model_forward <- glm(Outcome ~ BMI, data = diabetes, family = binomial)
summary(model_forward)
coefficients(model_forward)


## Part 5


# Load the data
data <- read.csv("Employees Salary")
