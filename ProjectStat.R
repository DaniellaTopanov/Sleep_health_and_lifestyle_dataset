# Part A1

library(dplyr)

sleep_data <- read.csv("https://raw.githubusercontent.com/DaniellaTopanov/Sleep_health_and_lifestyle_dataset/refs/heads/main/Sleep_health_and_lifestyle_dataset.csv")

print(paste("Observations before cleaning:", nrow(sleep_data)))

my_project_data <- sleep_data %>%
  select(Sleep.Duration, Physical.Activity.Level, Age, Stress.Level,Quality.of.Sleep ,Heart.Rate)

clean_data <- na.omit(my_project_data)

print(paste("Observations after cleaning:", nrow(clean_data)))

summary(clean_data)

#Part A2

# Simple linear regression for main explanatory variable x
# State the variables
y <- my_project_data$Sleep.Duration
x <- my_project_data$Physical.Activity.Level

n <- length(y)

x_bar <- sum(x) / n
y_bar <- sum(y) / n

Sxy <- sum((x-x_bar)*(y-y_bar))
Sxx <- sum((x-x_bar)^2)

b1_hat <- Sxy/Sxx
b0_hat <- y_bar - (b1_hat*x_bar)

print(b1_hat)
print(b0_hat)

# Check with R 
simple_model <- lm(Sleep.Duration ~ Physical.Activity.Level, data = my_project_data)
summary(simple_model)

# SE of b1_hat
y_hat <- b0_hat + (b1_hat*x)
epsilon_hat <- y - y_hat

error_var <- (1/(n-2)) * sum((epsilon_hat)^2)
b1_hat_var <- error_var / Sxx
b1_hat_SE <- sqrt(b1_hat_var)

print(b1_hat_SE)

# P_value
t_stat <- b1_hat/b1_hat_SE
p_value <- 2 * pt(-abs(t_stat), df = n-2)
print(p_value)

# CI 95% for b1_hat
t_crit <-qt(0.975, df=n-2)
CI_upper <- b1_hat + (t_crit*b1_hat_SE)
CI_lower <- b1_hat - (t_crit*b1_hat_SE)
print(paste("95% confidence interval for the slope:","[",CI_lower ,",",CI_upper,"]"))

#R^2

SST <- sum((y-y_bar)^2)
SSE <- sum((y-y_hat)^2) 

r_squared <- 1 - (SSE/SST)
print(r_squared)


# Additional Simple Regression
y <- my_project_data$Sleep.Duration
z <- my_project_data$Stress.Level

n <- length(y)

z_bar <- sum(z) / n
y_bar <- sum(y) / n

Szy <- sum((z-z_bar)*(y-y_bar))
Szz <- sum((z-z_bar)^2)

a1_hat <- Szy/Szz
a0_hat <- y_bar - (a1_hat*z_bar)

print(a1_hat)
print(a0_hat)

# Check with R 
simple_model_a <- lm(Sleep.Duration ~ Stress.Level, data = my_project_data)
summary(simple_model_a)

# SE of a1_hat
y_hat_tilda <- a0_hat + (a1_hat*z)
epsilon_hat_tilda <- y - y_hat_tilda

error_var_a <- (1/(n-2)) * sum((epsilon_hat_tilda)^2)
a1_hat_var <- error_var_a / Szz
a1_hat_SE <- sqrt(a1_hat_var)

print(a1_hat_SE)

# P_value
t_stat_a <- a1_hat/a1_hat_SE
p_value_a <- 2 * pt(-abs(t_stat_a), df = n-2)
print(p_value_a)

# CI 95% for a1_hat
t_crit <-qt(0.975, df=n-2)
CI_upper_a <- a1_hat + (t_crit*a1_hat_SE)
CI_lower_a <- a1_hat - (t_crit*a1_hat_SE)
print(paste("95% confidence interval for the slope:","[",CI_lower_a ,",",CI_upper_a,"]"))

#R^2

SST <- sum((y-y_bar)^2)
SSE_a <- sum((y-y_hat_tilda)^2) 

r_squared_a <- 1 - (SSE_a/SST)
print(r_squared_a)

# Plots
plot(x, y, 
     main = "Sleep Duration vs Physical Activity",
     xlab = "Physical Activity Level", 
     ylab = "Sleep Duration",
     pch = 16, col = "darkgray")
abline(a = b0_hat, b = b1_hat, col = "blue", lwd = 2)

plot(y_hat, epsilon_hat, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values (y_hat)", 
     ylab = "Residuals",
     pch = 16, col = "darkgray")
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(x, epsilon_hat, 
     main = "Residuals vs Physical Activity (x)",
     xlab = "Physical Activity Level (x)", 
     ylab = "Residuals",
     pch = 16, col = "darkgray")
abline(h = 0, col = "red", lty = 2, lwd = 2)


# Part B

# Additional variables

# Reported summary for the variables

x1 <- my_project_data$Physical.Activity.Level
x2 <- my_project_data$Age
x3 <- my_project_data$Stress.Level
x4 <- my_project_data$Quality.of.Sleep
x5 <- my_project_data$Heart.Rate

sd(x1)
sd(x2)
sd(x3)
sd(x4)
sd(x5)

predictors <- c("Physical.Activity.Level", "Age", "Stress.Level", "Quality.of.Sleep", "Heart.Rate")
p<- length(predictors)

intercept <- rep(1,n)
X <- cbind(intercept, x1,x2,x3,x4,x5)
X_transpose <- t(X)
X_inv <- solve(X_transpose %*% X)
Xt_mult_y <- X_transpose %*% y
Beta_hat <- X_inv %*% Xt_mult_y
print(Beta_hat)

# For check purposes only
mult_model <- lm(y ~ x1+x2+x3+x4+x5)
summary(mult_model)

# standard error:

eps_hat <- (y-(X%*%Beta_hat))
eps_hat_transpose <- t(eps_hat)

residual_variance <- as.numeric( (1/(n-(p+1))) * (eps_hat_transpose %*% eps_hat) )
var_beta_hat <- residual_variance * X_inv
print(var_beta_hat)
SE_mult <- sqrt(diag(var_beta_hat))
print(SE_mult)

# P_values

t_stat_mult <- Beta_hat / SE_mult
p_value_mult <- 2* pt(-abs(t_stat_mult),df = n-(p+1))
print(p_value_mult)

# 95% CI

t_crit_mult <- qt(0.975, df = n-(p+1))
CI_lower_mult <- Beta_hat - (t_crit_mult * SE_mult)
CI_upper_mult <- Beta_hat + (t_crit_mult * SE_mult)
CI_table <- cbind(Beta_hat, CI_lower_mult, CI_upper_mult)
colnames(CI_table) <- c("Estimate", "Lower 95%", "Upper 95%")
print(CI_table)

# R squared and adjusted R

SSE_mult <- eps_hat_transpose %*% eps_hat
SST_mult <- (t(y) %*% y) - (n*(y_bar)^2)
Rsquared_mult <- 1 - (SSE_mult/SST_mult)
print(Rsquared_mult)

AdjustedRsquared <- 1 - ((SSE_mult/(n-(p+1))) / (SST_mult / (n-1)))
print(AdjustedRsquared)

# Hypothesis Testing:
alpha <- 0.05
R_matrix <- matrix(c(0,1,0,0,0,0), ncol = 1)
RT_matrix <- t(R_matrix)
r_vector <- c(0)

t_stat_hyp <- as.numeric(abs(((RT_matrix%*%Beta_hat)-r_vector)/(sqrt(RT_matrix%*%var_beta_hat%*%R_matrix))))
t_crit_hyp <- qt(1-(alpha/2), df = n-(p+1)) 

print(paste("t Statistic:", t_stat_hyp))
print(paste("Critical Value:", t_crit_hyp))

if (t_stat_hyp > t_crit_hyp) {
  print("We reject H0 at significance level of 0.05%")
} else {
  print("We don't reject H0")
}

# Additional Hypothesis
#H0: B3&B5 = 0, H1: at least one is not

q = 2
RT_matrix_joint <- matrix(c(0,0,0,1,0,0,
                           0,0,0,0,0,1), nrow = 2, byrow = TRUE)
R_matrix_joint <- t(RT_matrix_joint)

r_vector_joint <- c(0,0)

w_stat <- as.numeric((t(RT_matrix_joint%*%Beta_hat)-r_vector_joint) %*% solve(RT_matrix_joint%*%var_beta_hat%*%R_matrix_joint) %*% ((RT_matrix_joint%*%Beta_hat) - r_vector_joint))
chi_crit <- qchisq(1-alpha, df = q)

print(paste("Wald Statistic:", w_stat))
print(paste("Critical Value:", chi_crit ))

if (w_stat > chi_crit) {
  print("We reject H0. Stress Level and Heart Rate jointly affect Sleep Duration")
} else {
  print("We don't reject H0")
}

# Plots

y_hat_mult <- X %*% Beta_hat

# Residuals vs fitted values
plot(y_hat_mult,eps_hat,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values (y_hat)",
     ylab = "Residuals",
     pch = 16, col = "darkgrey")
abline(h = 0, col = "red", lwd = 2, lty= 2)

# Residuals vs main explanatory 
plot(x1 , eps_hat,
     main = "Residuals vs Physical Activity",
     xlab = "Phisical Activity Level",
     ylab = "Residuals",
     pch = 16, col = "darkgrey")
abline(h = 0, col = "red", lwd = 2, lty= 2)

# Residuals vs additional  explanatory 
plot(x2 , eps_hat,
     main = "Residuals vs Age",
     xlab = "Age",
     ylab = "Residuals",
     pch = 16, col = "darkgrey")
abline(h = 0, col = "red", lwd = 2, lty= 2)

# Robustness check

# We will ad another interaction term, x6 will be the multiplication of physical activity and age

x6 <- x1 * x2
p_rob <- 6
X_rob <- cbind(intercept,x1,x2,x3,x4,x5,x6)
X_rob_transpose <- t(X_rob)
X_rob_inv <- solve(X_rob_transpose%*%X_rob)
Beta_hat_rob <- X_rob_inv %*% (X_rob_transpose %*% y)
print(Beta_hat_rob)

# SE robustness
eps_hat_rob <- y - (X_rob %*% Beta_hat_rob)
eps_hat_rob_transpose <- t(eps_hat_rob)
var_residuals_rob <- as.numeric((1/(n-(p_rob+1))) * ((eps_hat_rob_transpose)%*%eps_hat_rob))
var_beta_hat_rob <- var_residuals_rob * X_rob_inv
SE_rob <- sqrt(diag(var_beta_hat_rob))
print(SE_rob)

# P-Values
t_stat_rob <- Beta_hat_rob / SE_rob
p_value_rob <- 2 * pt(-abs(t_stat_rob), df = n-(p_rob+1))
print(p_value_rob)

# For checking purposes only

rob_model <- lm(y ~ x1+x2+x3+x4+x5+x6)
summary(rob_model)

# Summary for robustness and the multiple regression
results_rob <- cbind(Beta_hat_rob, SE_rob, t_stat_rob, p_value_rob)
colnames(results_rob) <- c("Estimate", "SE", "t-stat", "p-value")
print("Robustness Check Model Results (with Activity*Age Interaction):")
print(round(results_rob, 5))

results_mult <- cbind(Beta_hat,SE_mult,t_stat_mult,p_value_mult)
colnames(results_mult) <- c("Estimate", "SE", "t-stat", "p-value")
print("Multiple Reggression Model Results :")
print(round(results_mult, 5))


