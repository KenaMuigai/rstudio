set.seed(42)

n <- 50
x <- seq(-10, 10, length.out = n)
sigma <- (1 + x^2 / 5)^2
epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma))
y <- 1 - 3 * x + epsilon

ols_model <- lm(y ~ x)

plot(x, residuals(ols_model), 
     main = "Residuals from OLS vs x",
     xlab = "x", ylab = "Residuals", pch = 19)
abline(h = 0, col = "red", lwd = 2)

lines(lowess(x, residuals(ols_model)), col = "blue", lwd = 2)

#Theoretical Justification: The model exhibits heteroscedasticity, 
#as the error variance depends on), accounts for this structure and 
#yields estimators with minimum variance. 
#Therefore, WLS is the appropriate method for this model.