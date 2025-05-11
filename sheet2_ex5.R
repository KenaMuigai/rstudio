install.packages("car")     
library(readxl)
library(car)

data <- read_excel("WHRHDI2023.xlsx")

data$Happiness <- data$`Ladder.score`

model <- lm(Happiness ~ HDI + HDI2, data = data)

summary(model)

new_point <- data.frame(HDI = 0.85, HDI2 = 0.85^2)

predict(model, newdata = new_point, interval = "confidence", level = 0.9)

predict(model, newdata = new_point, interval = "prediction", level = 0.9)

summary_model <- summary(model)

b <- summary_model$coefficients

t_beta1 <- (b["HDI", "Estimate"] - 2) / b["HDI", "Std. Error"]
t_beta2 <- (b["HDI2", "Estimate"] - 2) / b["HDI2", "Std. Error"]

df <- df.residual(model)

# p-values
p_beta1 <- 2 * pt(-abs(t_beta1), df = df)
p_beta2 <- 2 * pt(-abs(t_beta2), df = df)

p_beta1
p_beta2

linearHypothesis(model, c("HDI = 2", "HDI2 = 2"))

predict(model, newdata = new_point, interval = "prediction", level = 0.90)
