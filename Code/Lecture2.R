# Approximately slides 34 - 62
library(forecast)
library(lmtest)


# Case Study Savings ------------------------------------------------------
savings <-read.csv("data/savings.csv")
savings <-ts(savings, start =c(1955, 1), end =c(1979, 4),frequency = 4)
plot(savings)

# Assessing the autocorrelation
N <- length(savings)

par(mfrow =c(2, 2))
# No lag
plot(savings[1:N],savings[1:N])
cor(savings[1:N],savings[1:N])

plot(savings[1:(N-1)], savings[2:N])
cor(savings[1:(N-1)], savings[2:N])

plot(savings[1:(N-2)], savings[3:N])
cor(savings[1:(N-2)], savings[3:N])

plot(savings[1:(N-3)], savings[4:N])
cor(savings[1:(N-3)], savings[4:N])

dev.off()

Acf(savings)

dev.off()

# Case Study Consumer Price Index -----------------------------------------
cpi <-read.csv("data/cpi.csv")
cpi <-ts(cpi, start =c(1994, 3), end =c(2012, 3), frequency = 12)
plot(cpi, ylab = "Index", main = "Consumer Price Index, Austria")

inf <-diff(cpi)/cpi[1:(length(cpi)-1)]*100
plot(inf, ylab = "Inflation in %", main = "Inflation rate, Austria")

# Simple regression
inf_mod <-tslm(inf ~ 1)
plot(inf) 
lines(fitted(inf_mod), col = "red")

# Check residuals for autocorrelation
Acf(resid(inf_mod))

dev.off()

# Case Study Airline Passengers  ------------------------------------------

# Let's consider a log-scale

ts_log_airline <- log(ts_airline)
plot(ts_log_airline, main = "International airline passengers",
     ylab = "Nr. of passengers, in log of thousands")

# Linear trend model
fit_log_airline_trend<-tslm(ts_log_airline ~ trend)
summary(fit_log_airline_trend)

lines(fit_log_airline_trend$fitted.values, col="red")

plot(forecast(fit_log_airline_trend), h=4)
lines(fit_log_airline_trend$fitted.values, col="red")

plot(fit_log_airline_trend$residuals)

Acf(fit_log_airline_trend$residuals)

dwtest(fit_log_airline_trend)

dev.off()

# Linear trend + seasonality
plot(ts_log_airline, main = "International airline passengers",
     ylab = "Nr. of passengers, in log of thousands")

fit_log_airline<-tslm(ts_log_airline ~ trend + season)
summary(fit_log_airline)

lines(fit_log_airline$fitted.values, col="red")

plot(forecast(fit_log_airline), h=4)
lines(fit_log_airline$fitted.values, col="red")

plot(fit_log_airline$residuals)

Acf(fit_log_airline$residuals)

dwtest(fit_log_airline)



# Case Study Savings ------------------------------------------------------
mod_savings <- tslm(savings ~ trend+season)
pred_mod_savings <-forecast(mod_savings, h = 4)
plot(pred_mod_savings)
lines(fitted(pred_mod_savings), col = "red")

par(mfrow =c(1, 2))
plot(resid(mod_savings))
Acf(resid(mod_savings))

dwtest(mod_savings)

dev.off()

# ACF/Box-Ljung-Statistics demonstration ----------------------------------
demo_acf <- read.delim("eviews/demo_acf", header = FALSE, sep = "")
par(mfrow =c(1, 2))
plot(demo_acf[,1], type = "l")
Acf(demo_acf[,1])

# This function runs the Box-Ljung Test from 1 up to the specified degree (parameter h)
run_box_tests <-function(data, h = 6){
  results <- data.frame(h = NA, statistic = NA, p = NA)
  for (i in 1:h){
    box <-Box.test(data, lag = i, type = "Ljung-Box")
    results[i, ] <- c(i, box$statistic, box$p.val)
  }
  knitr::kable(results)
}
run_box_tests(demo_acf[,1])


plot(demo_acf[,2], type = "l")
Acf(demo_acf[,2])

run_box_tests(demo_acf[,2])


plot(demo_acf[,3], type = "l")
Acf(demo_acf[,3])

run_box_tests(demo_acf[,3])


plot(demo_acf[,4], type = "l")
Acf(demo_acf[,4])
run_box_tests(demo_acf[,4])

# Reset all par() settings
dev.off()


run_box_tests(savings)



uspop <- hexView::readEViews("eviews/uspop.wf1")
uspop <- ts(uspop$Y, start = 1790, end = 1990, frequency = 0.1)
plot(uspop)

mod_uspop <- tslm(uspop ~ trend + I(trend^2))
plot(uspop)
lines(fitted(mod_uspop), col = "red")

Acf(resid(mod_uspop))
run_box_tests(resid(mod_uspop))

