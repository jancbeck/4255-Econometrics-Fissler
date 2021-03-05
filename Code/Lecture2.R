# Approximately slides 34 - 62
library(forecast)
library(lmtest)


# Case Study Savings ------------------------------------------------------
savings <-read.csv("../data/savings.csv")
savings <-ts(savings, start =c(1955, 1), end =c(1979, 4),frequency = 4)
plot(savings)

Acf(savings)

# Case Study Consumer Price Index -----------------------------------------
cpi <-read.csv("../data/cpi.csv")
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


# Case Study Airline Passengers  ------------------------------------------
# Linear trend model
data_airline <- read.csv("../data/airline.csv")
ts_airline<- ts(data_airline, frequency = 12, start = c(1949, 1))
fit_airline<-tslm(ts_airline ~ trend)
summary(fit_airline)

plot(ts_airline)
lines(fit_airline$fitted.values, col="blue")

# Quadratic trend model

fit_airline_2<-tslm(ts_airline ~ trend +  I(trend^2))
summary(fit_airline_2)
#plot(ts_airline)
lines(fit_airline_2$fitted.values, col="red")

# Cubic trend model

fit_airline_3<-tslm(ts_airline ~ trend + I(trend^2) + I(trend^3))
summary(fit_airline_3)
lines(fit_airline_3$fitted.values, col="green")

#Model comparison

c(AIC(fit_airline), AIC(fit_airline_2), AIC(fit_airline_3))
c(BIC(fit_airline), BIC(fit_airline_2), BIC(fit_airline_3))

# Look at seasonal patterns

fit_airline_4<-tslm(ts_airline ~ season)
summary(fit_airline_4)
plot(ts_airline)
lines(fit_airline_4$fitted.values, col="blue")

fit_airline_5<-tslm(ts_airline ~ trend + season)
summary(fit_airline_5)
#plot(ts_airline)
lines(fit_airline_5$fitted.values, col="red")

# Forecasting

forecast(fit_airline_2, h = 10) # h is for 'horizon'


# You might also try to consider log-transformed data
ts_log_airline <- log(ts_airline)
plot(ts_log_airline)

# In what respect might they be more appropriate for our purposes?
# Of course, you can now also fit similar models as above.

fit_log_airline<-tslm(ts_log_airline ~ trend + season)
summary(fit_log_airline)
#plot(ts_airline)
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



# ACF/Box-Ljung-Statistics demonstration ----------------------------------
demo_acf <- read.delim("../eviews/demo_acf", header = FALSE, sep = "")
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



uspop <- hexView::readEViews("../eviews/uspop.wf1")
uspop <- ts(uspop$Y, start = 1790, end = 1990, frequency = 0.1)
plot(uspop)

mod_uspop <- tslm(uspop ~ trend + I(trend^2))
plot(uspop)
lines(fitted(mod_uspop), col = "red")

Acf(resid(mod_uspop))
run_box_tests(resid(mod_uspop))
