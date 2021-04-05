# Approximately slides 62 - 92

library(forecast)
library(lmtest)


# Simulating an AR(1) process ---------------------------------------------

n <- 600
u <- rnorm(n)

phi <- 0.9
Y_1 <- 0
Y<- rep(Y_1,n)
for (t in 2:n) {
  Y[t] <- phi*Y[t-1] + u[t]
}
Y <- ts(Y, frequency = 1)
#plot(Y)
#abline(0,0, col="blue")

# Play around with different values of phi and also Y_1. 
# Consider what happens when phi<-1, phi=-1, -1 < phi < 0
# phi = 0, 0< phi <1, phi=1, and phi>1.
# What happens in terms of the mean, what happens in terms of the variance?
# What about the autocorelation?


# Now consider also the auto-correlation function and the partial autocorrelation function.

par(mfrow=c(2,2))
plot(Y)
abline(0,0, col="blue")
Acf(Y)
Pacf(Y)

# And let's fit an AR(1) model to the data.
summary(arima(Y, order=c(1,0,0)))


# Reset par() settings
dev.off()


# Case study savings: Fitting an AR(1) model ------------------------------

savings <- read.csv("../data/savings.csv")
savings <- ts(savings, start = c(1955, 1), end = c(1979, 4),
   frequency = 4)
head(savings)

plot(savings)

### Two ways to fit an autoregressive model to data
### First, manually:

# Fit an autoregressie model manually
N <- length(savings)
Y <- savings[2:N]
X <- savings[1:(N-1)]

fit_savings <- lm(Y ~ X)
summary(fit_savings)

# Plot the fitted values
Z <- ts(fit_savings$fitted.values, start = c(1955, 2), end = c(1979, 4),
        frequency = 4)
lines(Z, col="red")

# We can see that the fitted value for phi corresponds to the regression parameter for X
phi_hat <- fit_savings$coefficients[2]
phi_hat

# To calculate an estimator for mu, we use
mu_hat <- fit_savings$coefficients[1]/(1-phi_hat)
mu_hat

# Estimator for variance 
sigma_u_hat <- sigma(fit_savings)
sigma_u_hat

sigma_hat <- sigma_u_hat / sqrt(1-phi_hat^2)
sigma_hat

# Compare with the sample quantities:
mean(Y)
sd(Y)

# Consider the residuals
plot(ts(fit_savings$residuals, start = c(1955, 2), end = c(1979, 4),
        frequency = 4))
abline(0,0, col="blue")

# Residual diagnostics

mean(fit_savings$residuals)
# This is always 0

plot(X, fit_savings$residuals)
abline(0,0, col = "blue")

# What about homosecedasticity?
plot(ts(fit_savings$residuals^2, start = c(1955, 2), end = c(1979, 4),
        frequency = 4))
# Not really.

plot(X, fit_savings$residuals^2)
# Also problematic.


# Normality? (This should be taken with a grain of salt since we deem the residuals not stationary.)
hist(fit_savings$residuals)
qqnorm(fit_savings$residuals)
qqline(fit_savings$residuals, col="red")
# Looks not too bad. Let's do a test.

library(tseries)
jarque.bera.test(fit_savings$residuals)

# Okay, we can clearly reject the null hypothesis. It is not normal.


# What about serial dependence?
Acf(fit_savings$residuals)

# Serial correlation _might_ still present in the residuals:

Box.test(fit_savings$residuals,type = "Ljung-Box",lag = 1)
Box.test(fit_savings$residuals,type = "Ljung-Box",lag = 2)
Box.test(fit_savings$residuals,type = "Ljung-Box",lag = 3)


### Alternateively, we can use the Arima package
## Method has to be set to CSS to get same results.
## Otherwise, the results slightly differ due to different
## numerical approximations.
arima_fit_savings <- Arima(savings , order = c(1, 0, 0), method = "CSS")
summary(arima_fit_savings)



# Now we finally do some forecasting:

forecast(arima_fit_savings, h = 36)
plot(forecast(arima_fit_savings, h = 36))
lines(fitted(arima_fit_savings), col = "blue")


# Identify AR(p) process degree -------------------------------------------

# And now some AR(p) fitting:
arima_fit_savings_2 <- Arima(savings , order = c(2, 0, 0))
summary(arima_fit_savings_2)
coeftest(arima_fit_savings_2)

arima_fit_savings_3 <- Arima(savings , order = c(3, 0, 0))
summary(arima_fit_savings_3)
coeftest(arima_fit_savings_3)


arima_fit_savings_4 <- Arima(savings , order = c(4, 0, 0))
summary(arima_fit_savings_4)
coeftest(arima_fit_savings_4)


# Consider partial autocorrelation coefficient:

Pacf(savings)
# Seems to indicate AR(1)

# Let's compare the models using AIC and BIC
arima_fit_savings <- Arima(savings , order = c(1, 0, 0))
summary(arima_fit_savings)
summary(arima_fit_savings_2)
summary(arima_fit_savings_3)
summary(arima_fit_savings_4)

# AIC and BIC prefer different models!

# Have a look at Pacf of the residuals.
Pacf(resid(arima_fit_savings))
Pacf(resid(arima_fit_savings_2))
Pacf(resid(arima_fit_savings_3))
Pacf(resid(arima_fit_savings_4))

# Maybe an AR(2) process sufficient?
car::linearHypothesis(arima_fit_savings_4, c("ar4", "ar3"))
# Can not reject null hypothesis that the ar4 and ar3 term are equal to 0

Acf(resid(arima_fit_savings_2))
Box.test(arima_fit_savings_2$residuals,type = "Ljung-Box",lag = 5)
# Apparently also no more autocorrelation in residuals
