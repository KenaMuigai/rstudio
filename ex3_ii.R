# Exercise 3 (ii) - Simulation to compare OLS and WLS

set.seed(123)
n <- 50
x <- seq(-10, 10, length.out = n)
sigma <- (1 + x^2 / 5)^2
m <- 10000

# Storage vectors
ols_intercepts <- numeric(m)
ols_slopes <- numeric(m)
wls_intercepts <- numeric(m)
wls_slopes <- numeric(m)

for (i in 1:m) {
  epsilon <- rnorm(n, 0, sqrt(sigma))
  y <- 1 - 3 * x + epsilon
  
  # OLS estimation
  ols_fit <- lm(y ~ x)
  ols_intercepts[i] <- coef(ols_fit)[1]
  ols_slopes[i] <- coef(ols_fit)[2]
  
  # WLS estimation (weights = inverse variance)
  wls_fit <- lm(y ~ x, weights = 1 / sigma)
  wls_intercepts[i] <- coef(wls_fit)[1]
  wls_slopes[i] <- coef(wls_fit)[2]
}

# Summary statistics
results <- data.frame(
  Mean_OLS_Intercept = mean(ols_intercepts),
  Var_OLS_Intercept = var(ols_intercepts),
  Mean_WLS_Intercept = mean(wls_intercepts),
  Var_WLS_Intercept = var(wls_intercepts),
  Mean_OLS_Slope = mean(ols_slopes),
  Var_OLS_Slope = var(ols_slopes),
  Mean_WLS_Slope = mean(wls_slopes),
  Var_WLS_Slope = var(wls_slopes)
)

print(round(results, 6))

#The simulation shows that both OLS and WLS estimators are unbiased: 
#the average estimates are close to the true values 
#(intercept = 1, slope = –3). However, the empirical variances for 
#WLS are significantly lower than for OLS. This confirms that WLS is 
#more efficient under heteroscedasticity and is the preferred method 
#in this scenario.

#results 
#Mean_OLS_Intercept Var_OLS_Intercept Mean_WLS_Intercept
#1           1.002273          1.983281           1.001465
#Var_WLS_Intercept Mean_OLS_Slope Var_OLS_Slope Mean_WLS_Slope
#1          0.114187      -3.000031      0.120718       -2.99926
#Var_WLS_Slope
#1      0.031493

#Mean_OLS_Intercept Var_OLS_Intercept Mean_WLS_Intercept
#1           1.002273          1.983281           1.001465
#Var_WLS_Intercept Mean_OLS_Slope Var_OLS_Slope Mean_WLS_Slope
#1          0.114187      -3.000031      0.120718       -2.99926
#Var_WLS_Slope
#1      0.031493