# Approximately slides 122 - 150

library(forecast)
library(lmtest)
library(car)



# Case Study US population ------------------------------------------------

# Read in data from EViews workfile
uspop <- hexView::readEViews("eviews/uspop.wf1")
uspop <- ts(uspop$Y, start = 1790, end = 1990, frequency = 0.1)
# That means we have data for each decade.

par(mfrow=c(3,1))

plot(uspop)

# model with quadratic trend
mod_uspop <- tslm(uspop ~ trend + I(trend^2))
lines(mod_uspop$fitted.values, col="red")



# Check residuals for autocorrelation
plot(residuals(mod_uspop))
abline(0,0, col="blue")
Acf(residuals(mod_uspop))
Box.test(residuals(mod_uspop), lag = 6)
# Apparently, the residuals still have autocorrelation.


# Create regressor matrix for Arima
X <- as.matrix(data.frame(time = 1:length(uspop), time2 = (1:length(uspop))^2))
head(X)

# Overfit model
mod_uspop_arima <- Arima(uspop, order = c(5, 0, 5), xreg = X, method = "ML")
summary(mod_uspop_arima)
coeftest(mod_uspop_arima)
# Tons of non-significant parameters

# Visual inspection:
plot(uspop)
lines(fitted(mod_uspop_arima), col="red")
plot(residuals(mod_uspop_arima))
abline(0,0, col="blue")
Acf(residuals(mod_uspop_arima))
Box.test(residuals(mod_uspop_arima), lag = 6, type = "Ljung-Box")
# But no more autocorrelation!

# Lots of insignificant parameters -> use overfitting and AIC/BIC to find best model
#
# This function brute forces all possible combinations of regressors and outputs AIC/BIC
# I wouldnt recommend using it above max.order = 4, as the model space grows exponentially
# If you want to gain some insight into how this function works, you can use the 
# browser() function to pause the execution and see whats going on
order_checker <- function(Y, max.order = 3, xreg = NULL, I = 0, include.drift = FALSE){

  params <- list()

  for (j in c("ar", "ma")){
    for (i in 1:max.order){
     params[[paste0(j, i)]] <- c(0, NA)
    }
  }

  if (!is.null(xreg)){
    for (j in colnames(xreg)){
      params[[j]] <- NA
    }
  }

  params$intercept <- NA

  combinations <- expand.grid(params)
  res <- data.frame(BIC = rep(NA, nrow(combinations)), AIC = NA)


  pb <- txtProgressBar(1, nrow(combinations), style = 3)
  for (i in 1:nrow(combinations)){
    curr_mod <- try(Arima(Y, order = c(max.order, I, max.order), fixed = c(combinations[i, ]),
                      xreg = xreg, method = "ML", include.drift = include.drift), silent = TRUE)

    if ("try-error" %in% attr(curr_mod, "class")){
      res$BIC[i] <- NA
      res$AIC[i] <- NA
    } else {
      res$BIC[i] <- BIC(curr_mod)
      res$AIC[i] <- AIC(curr_mod)
    }
    setTxtProgressBar(pb, i)
  }

  res <- merge(res, combinations, by = 0)
  res$Row.names <- NULL
  return(res)
}

res <- order_checker(uspop, 5, X)
res[which.min(res$BIC), ]
res[which.min(res$AIC), ]

## BIC chooses ARMA(2, 2), AIC chooses ARMA(2, 4) --> Go with ARMA(2, 2)

final_us_mod <- Arima(uspop, order = c(5, 0, 5), xreg = X, fixed = res[which.min(res$BIC), 3:ncol(res)])
summary(final_us_mod)
coeftest(final_us_mod)

# Check residuals
plot(residuals(final_us_mod))
abline(0,0, col="blue")
Acf(residuals(final_us_mod))
Box.test(residuals(final_us_mod), lag = 6, type = "Ljung-Box")
# Looking good

# Model fit looks nice too
plot(uspop)
lines(fitted(final_us_mod), col = "forestgreen")


dev.off()


# Case Study Airline Data -------------------------------------------------

airline <- read.csv("data/airline.csv")

# Transform with logs
airline <- log(airline)
airline <- ts(airline, start = c(1949, 1), frequency = 12)

# Model with linear trend and seasonality
airline_mod <- tslm(airline ~ trend + season)


# Check residuals for autocorrelation
par(mfrow=c(3,1))
plot(airline)
lines(fitted(airline_mod), col="red")
plot(residuals(airline_mod))
abline(0,0, col="blue")
Acf(residuals(airline_mod))
Box.test(residuals(airline_mod), lag = 6, type = "Ljung-Box")
# Apparently we have clear autocorrelation

# Overfit model
# Create seasonal dummies that result in deviations from the mean
p <- 12
month <- as.factor(rep(1:p, 12))
D <- model.matrix(~month-1)
S <- (D-D[  , p])[, 1:(p-1)]
X <- cbind(as.matrix(data.frame(time = 1:length(airline))), S)

# Fit model with trend, season and ARMA(4, 4) errors
airline_mod_arima <- Arima(airline, order = c(4, 0, 4), xreg = X, method = "ML")
summary(airline_mod_arima)
coeftest(airline_mod_arima)
# Again we have a couple insignificant parameters

plot(airline)
lines(fitted(airline_mod_arima), col="red")
plot(residuals(airline_mod_arima))
abline(0,0, col="blue")
Acf(residuals(airline_mod_arima))
Box.test(residuals(airline_mod_arima), lag = 6, type = "Ljung-Box")
# But no more autocorrelation!

# Use overfitting and AIC/BIC to find best model again
res <- order_checker(airline, 4, X)
res[which.min(res$BIC), ]
res[which.min(res$AIC), ]
## BIC chooses ARMA(1, 0), AIC chooses ARMA(4, 4)* --> Go with ARMA(1, 0)


airline_mod_final <- Arima(airline, order = c(4, 0, 4), xreg = X, fixed = res[which.min(res$BIC), 3:ncol(res)] )
summary(airline_mod_final)
coeftest(airline_mod_final)
# This looks good. The ARMA coefficients are all significant. 
# It is completely in order when there are several dummy variables accounting for 
# seasonality that are insignificant.

plot(airline)
lines(fitted(airline_mod_final), col="forestgreen")
plot(residuals(airline_mod_final))
abline(0,0, col="blue")
Acf(residuals(airline_mod_final))
Box.test(residuals(airline_mod_final), lag = 6, type = "Ljung-Box")
# Nice reduced model with no more autocorrelation in the errors!


dev.off()


# Case Study Consumer Price Index -----------------------------------------

cpi <- read.csv("data/cpi.csv")
cpi <- ts(cpi, start = c(1994, 3), end = c(2012, 3), frequency = 12)

# Model with just a linear trend
cpi_mod <- tslm(cpi ~ trend)

par(mfrow=c(3,1))
# Check residuals for autocorrelation
plot(cpi)
lines(fitted(cpi_mod), col="red")
plot(residuals(cpi_mod))
abline(0,0, col="blue")
Acf(residuals(cpi_mod))
Box.test(residuals(cpi_mod), lag = 6, type = "Ljung-Box")
# Apparently we have autocorrelation.

X <- as.matrix(data.frame(time = 1:length(cpi)))

# Same strategy again - overfit, then see what we can remove
cpi_mod_arima <- Arima(cpi, order = c(10, 0, 10), xreg = X)
plot(cpi)
lines(fitted(cpi_mod_arima), col="red")
plot(residuals(cpi_mod_arima))
abline(0,0, col="blue")
Acf(residuals(cpi_mod_arima))
Box.test(residuals(cpi_mod_arima), lag = 6, type = "Ljung-Box")

coeftest(cpi_mod_arima)
# However, again insignificant values.

res <- order_checker(cpi, 4, X)
res[which.min(res$BIC), ]
res[which.min(res$AIC), ]
## BIC chooses ARMA(2, 3)*, AIC chooses ARMA(4, 4)* --> Go with ARMA(2, 3)*

cpi_mod_final <- Arima(cpi, c(4, 0, 4), xreg = X, fixed = res[which.min(res$BIC), 3:ncol(res)], method = "ML")
summary(cpi_mod_final)
coeftest(cpi_mod_final)

plot(cpi)
lines(fitted(cpi_mod_final), col = "forestgreen")
plot(residuals(cpi_mod_final))
abline(0,0, col="blue")
Acf(residuals(cpi_mod_final))
Box.test(residuals(cpi_mod_final), lag = 6, type = "Ljung-Box")


dev.off()



# Case Study Stocks -------------------------------------------------------

stocks <- read.csv("data/atx.csv")
atx <- ts(stocks$index, start = c(1993, 1), frequency = 12)

par(mfrow=c(3,1))
plot(atx)
length(atx)
# Definitely NOT stationary

# Create differenced time series
diff_atx <- diff(atx)
length(diff_atx)
plot(diff_atx)
# Looks way more stationary
Acf(diff_atx)
Box.test(diff_atx, lag = 6, type = "Ljung-Box")
# But apparently not stationary enough
mean(diff_atx)

# Estimate a random walk with drift
drift_mod <- lm(diff_atx ~ 1)
summary(drift_mod)
# Do not reject null hypothesis --> no drift coefficient needed

# Can also be achieved with Arima()
drift_mod_ar <- Arima(atx, order = c(0, 1, 0), include.drift = TRUE, method = "CSS")
summary(drift_mod_ar)

res <- order_checker(atx, 4, xreg = NULL, I = 1, include.drift = TRUE)
res[which.min(res$BIC), ]
res[which.min(res$AIC), ]
# BIC suggests an ARMA(1,0) model


drift_model_final <-  Arima(atx, c(4, 1, 4), include.drift = TRUE, xreg = NULL, fixed = res[which.min(res$BIC), 3:ncol(res)], method = "ML")
summary(drift_model_final)
coeftest(drift_model_final)

plot(atx)
lines(fitted(drift_model_final), col="forestgreen")
plot(residuals(drift_model_final))
abline(0,0, col="blue")
Acf(residuals(drift_model_final))
Box.test(residuals(drift_model_final), lag = 6, type = "Ljung-Box")

dev.off()

par(mfrow=c(2,1))


# Case Study Dow Jones ----------------------------------------------------

dow <- read.csv("data/dowjones.csv")
ldow <- ts(log(dow$dowjones), start = c(1980, 1), frequency = 52)
plot(ldow)
plot(diff(ldow))

hist(diff(ldow))
mean(diff(ldow))
# This indicates that there is a drift term!

diff_mod_ldow <- Arima(ldow, order = c(0, 1, 0), include.drift = TRUE)
lines(fitted(diff_mod_ldow), col="red")
plot(residuals(diff_mod_ldow))
abline(0,0, col="blue")
Acf(residuals(diff_mod_ldow))
Box.test(residuals(diff_mod_ldow), lag = 6, type = "Ljung-Box")
summary(diff_mod_ldow)

coeftest(diff_mod_ldow)


res <- order_checker(ldow, 4, xreg = NULL, I = 1, include.drift = TRUE)
res[which.min(res$BIC), ]
res[which.min(res$AIC), ]
# BIC chooses ARIMA(0, 1, 0) model

final_ldow_mod <- Arima(ldow, c(0, 1, 0), include.drift = TRUE)

plot(ldow)
lines(fitted(final_ldow_mod), col = "forestgreen")


dev.off()

par(mfrow=c(2,1))



# Case Study Consumer Price Index -----------------------------------------

# Overfit model with difference
# Load old cpi data
cpi <- hexView::readEViews("data/vpi_old.wf1")
cpi <- ts(cpi$VPISA[cpi$VPISA > 0], start = c(1964, 1), end = c(1994, 4), frequency = 4)

train <- window(cpi, end = c(1992, 4))
test <- window(cpi, start = c(1993, 1))
plot(train)
plot(diff(train))

cpi_mod_diff <- Arima(train, c(5, 1, 5), include.drift = 1)
summary(cpi_mod_diff)
coeftest(cpi_mod_diff)

plot(residuals(cpi_mod_diff))
abline(0,0, col="blue")
Acf(residuals(cpi_mod_diff))
Box.test(residuals(cpi_mod_diff), lag = 6, type = "Ljung-Box")

res <- order_checker(train, 5, xreg = NULL, I = 1, include.drift = TRUE)
res[which.min(res$BIC), ]
res[which.min(res$AIC), ]
# BIC chooses ARIMA(4, 1, 0)*

final_cpi_mod <- Arima(train, c(5, 1, 5), fixed = res[which.min(res$BIC), 3:ncol(res)], include.drift = TRUE)
summary(final_cpi_mod)
coeftest(final_cpi_mod)

plot(residuals(final_cpi_mod))
abline(0,0, col="blue")
Acf(residuals(final_cpi_mod))
Box.test(residuals(final_cpi_mod), lag = 6, type = "Ljung-Box")

plot(train)
lines(fitted(final_cpi_mod), col = "forestgreen")

forc <- forecast(final_cpi_mod, h = 8)
plot(forc, ylim = c(350, 450), xlim = c(1980, 1995))
lines(test, col = "red", lwd = 2)

# We are doing not too badly :)
