# Approximately slides 91 - 121

library(forecast)
library(lmtest)


# Compare ACF and PACF of AR and MA processes -----------------------------

# Simulating an AR(1) process

n <- 600
u <- rnorm(n)

phi <- 0.7
Y_init <- 0
Y_AR1<- rep(Y_init, n)
for (t in 2:n) {
  Y_AR1[t] <- phi*Y_AR1[t-1] + u[t]
}
Y_AR1 <- ts(Y_AR1)
plot(Y_AR1)

par(mfrow = c(1, 2))
acf(Y_AR1)
pacf(Y_AR1)


# Simulating an AR(2) process
n <- 600
u <- rnorm(n)

phi_1 <- 0.5
phi_2 <- 0.3
Y_AR2 <- rep(Y_init, n)
for (t in 3:n) {
  Y_AR2[t] <- phi_1*Y_AR2[t-1] + phi_2*Y_AR2[t-2] + u[t]
}
Y_AR2 <- ts(Y_AR2)
plot(Y_AR2)

par(mfrow = c(1, 2))
Acf(Y_AR2)
Pacf(Y_AR2)




# Simulate an MA(1) process
u <- rnorm(n+1)

theta <- 0.9
Y_MA1 <- rep(Y_init, n)

for (t in 2:n){
  Y_MA1[t] <- theta * u[t - 1] + u[t]
}
Y_MA1 <- ts(Y_MA1)
plot(Y_MA1)

par(mfrow = c(1, 2))
Acf(Y_MA1)
Pacf(Y_MA1)


# Simulate an MA(2) process
u <- rnorm(n+2)

theta1 <- 0.9
theta2 <- 0.7
Y_MA2 <- rep(Y_init, n)

for (t in 3:n){
  Y_MA2[t] <- theta1 * u[t - 1] + theta2 * u[t - 2] + u[t]
}
Y_MA2 <- ts(Y_MA2)
plot(Y_MA2)

par(mfrow = c(1, 2))
Acf(Y_MA2)
Pacf(Y_MA2)

par(mfrow = c(4, 2), mar = c(2, 2.5, 4, 1.5) + 0.1, mgp = c(1.5, 0.5, 0)) 
Acf(Y_AR1, xlab = "") 
Pacf(Y_AR1, xlab = "")
Acf(Y_AR2, xlab = "")
Pacf(Y_AR2, xlab = "")
Acf(Y_MA1, xlab = "")
Pacf(Y_MA1, xlab = "")
Acf(Y_MA2, xlab = "")
Pacf(Y_MA2, xlab = "")


# Reset par() settings
dev.off()



# Case Study Savings Overfitting ------------------------------------------

savings <- read.csv("data/savings.csv")
savings <- ts(savings, start = c(1955, 1), end = c(1979, 4),
              frequency = 4)

plot(savings)

overf_mod <- Arima(savings, order = c(2, 0, 2))
summary(overf_mod)

plot(overf_mod$residuals)
abline(h = 0, col = "red")
Box.test(overf_mod$residuals, lag = 6, type = "Ljung-Box")

Acf(overf_mod$residuals)

lmtest::coeftest(overf_mod)

library(car)
linearHypothesis(overf_mod, c("ar2 = 0", "ma1 = 0"))

rest_mod <- Arima(savings , order = c(2, 0, 2), fixed=c(NA, 0, 0, NA, NA))
summary(rest_mod)

coeftest(rest_mod)

c(AIC(overf_mod), AIC(rest_mod))
c(BIC(overf_mod), BIC(rest_mod))        

