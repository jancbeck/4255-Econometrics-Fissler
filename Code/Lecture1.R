# Approximately slides 1 - 33

library(forecast)
library(lmtest)


# Case Study Airline Passengers  ------------------------------------------
data_airline <- read.csv("data/airline.csv")
head(data_airline)
ts_airline<- ts(data_airline, frequency = 12, start = c(1949, 1))
plot(ts_airline, main = "International airline passengers",
     ylab = "Nr. of passengers, in thousands")



# Case Study Savings ------------------------------------------------------
data_savings <- read.csv("data/savings.csv")
head(data_savings)

ts_savings <- ts(data_savings, frequency = 4, start = c(1955, 1))
plot(ts_savings, main  = "Quarterly savings rate, USA",
     ylab = "Savings rate in %")


cpi <-read.csv("data/cpi.csv")
cpi_ts <-ts(cpi, start =c(1994, 3), end =c(2012, 3), frequency = 12)
plot(cpi_ts, ylab = "Index", main = "Consumer Price Index, Austria") 

inf <- diff(cpi_ts)/cpi_ts[1:(length(cpi_ts)-1)]*100
plot(inf, ylab = "Inflation in %", main = "Inflation rate, Austria")


# Case Study Exchange Rates -----------------------------------------------
data_exrates <- read.csv("data/exrates.csv")
head(data_exrates)

ts_exrates <- ts(data_exrates, frequency = 365, start = c(2000, 3))

plot(ts_exrates)




# Case Study Consumer Price Index -----------------------------------------
lin_mod <- tslm(cpi_ts ~ trend)
summary(lin_mod)

plot(cpi_ts, ylab = "Index")
lines(fitted(lin_mod), col = "red")

quad_mod <- tslm(cpi_ts ~ trend + I(trend^2))
summary(quad_mod)
lines(fitted(quad_mod), col = "green")

# Model comparison
cat(c(AIC(lin_mod), AIC(quad_mod)))
cat(c(BIC(lin_mod), BIC(quad_mod)))


