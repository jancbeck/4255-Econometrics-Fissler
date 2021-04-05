
#############EXAMPLE 1 (PURELY RANDOM PROCESS)####################################
u1<- rnorm(1000, 0, sqrt(0.16))
y1=2 + u1
plot(y1, type="l")
abline(h=2, col="red")
abline(h=2 + 1.96*sqrt(0.16), col="blue", lty=2)
abline(h=2 - 1.96*sqrt(0.16), col="blue", lty=2)


##############EXAMPLE 2 (AR1 PROCESS)####################################
u2<- rnorm(500, 0, sqrt(1))
y2<-rep(NA, 500)
y2[1]<-10
for(i in 2:500)
y2[i]<-0.8*y2[i-1]  + 2 + u2[i]

plot(y2, type="l")
abline(h=10, col="red")
abline(h=10 + 1.96*sqrt(1/(1-(0.8^2))), col="blue", lty=2)
abline(h=10 - 1.96*sqrt(1/(1-(0.8^2))), col="blue", lty=2)


#################EXAMPLE 3 (TREND STATIONARY RANDOM PROCESS)#######################################################
u3<- rnorm(300, 0, sqrt(0.36))
t<- 1:300
y3=2 + 0.01*t + u3

plot(y3, type="l")
lines(2 + 0.01*t, col="red")
lines(2 + 0.01*t + 1.96*sqrt(0.36), col="blue", lty=2)
lines(2 + 0.01*t - 1.96*sqrt(0.36), col="blue", lty=2)

#################EXAMPLE 4 (RANDOM WALK PROCESS WITH DRIFT)######################################################
#set.seed=1
u4<- rnorm(500, 0, sqrt(1))
t<-1:500
y4<-rep(NA,500)
y4[1]<-0

for(i in 2:500)
  y4[i]<-y4[i-1]  + 0.2 + u4[i]

plot(y4, ylim=c(-20, 160), type="l")
lines(0 + 0.2*t, col="red")
lines(0 + 0.2*t + 1.96*sqrt(t), col="blue", lty=2)
lines(0 + 0.2*t - 1.96*sqrt(t), col="blue", lty=2)

##############EMPIRICAL AUTOCORRELATION FUNCTION##################################
par(mfrow=c(2,2))
acf(y1, main="purely random process")
acf(y2, main="AR(1) process")
acf(y3, main="trend stationary process")
acf(y4, main="random walk process with drift")



Box.test(y1, lag = 6, type = c("Ljung-Box"))
Box.test(y2, lag = 20, type = c("Ljung-Box"))
Box.test(y3, lag = 6, type = c("Ljung-Box"))
Box.test(y4, lag = 20, type = c("Ljung-Box"))


######################################
library(hexView)

savewf1<-readEViews("save.wf1")
head(savewf1)
View(savewf1)
savings<-savewf1$SAVE[1:100]
plot(savings, pch=20)
lines(savings)
acf(savings)



data(uspop)
help(uspop)
n<-length(uspop)

plot(uspop, pch=20)
time<-1:n
time2<-time^2
fit<-lm(uspop ~ time + time2)
summary(fit)
uspop - predict(fit)
res<-residuals(fit)
plot(res, pch=20)
lines(res)
acf(res)
Box.test(res, lag = 1, type = c("Ljung-Box"))


###############################################################################
####simulation from AR(1) model


nsim<-100
yleft1<-rep(NA, nsim)
yleft2<-rep(NA, nsim)
yleft1[1]<-2
yleft2[1]<--4
phi<-0.3
sig2<-0.36
for(i in 2:nsim)
{
  yleft1[i]<- phi*yleft1[i-1] + rnorm(1,0,sqrt(sig2))
  yleft2[i]<- phi*yleft2[i-1] + rnorm(1,0,sqrt(sig2))
  
}

plot(yleft1, type="l")
lines(yleft2, col="red")
abline(h=0, col="red")
abline(h=0 + 1.96*sqrt(sig2/(1-phi^2)), col="blue", lty=2)
abline(h=0 -1.96*sqrt(sig2/(1-phi^2)), col="blue", lty=2)






nsim<-100
yright1<-rep(NA, nsim)
yright2<-rep(NA, nsim)
yright1[1]<-2
yright2[1]<--4
phi<-0.8
sig2<-0.09
for(i in 2:nsim)
{
  yright1[i]<- phi*yleft1[i-1] + rnorm(1,0,sqrt(sig2))
  yright2[i]<- phi*yleft2[i-1] + rnorm(1,0,sqrt(sig2))
  
}

plot(yright1, type="l")
lines(yright2, col="red")
abline(h=0, col="red")
abline(h=0 + 1.96*sqrt(sig2/(1-phi^2)), col="blue", lty=2)
abline(h=0 -1.96*sqrt(sig2/(1-phi^2)), col="blue", lty=2)

#arima.sim(model=list(ar=c(.3)),n=100)



nsim<-1000
yrw<-rep(NA, nsim)
sig2<-1
yrw[1]<-0
for(i in 2:nsim)
{
  yrw[i]<- yrw[i-1] + rnorm(1,0,sqrt(sig2))
}

plot(yrw, type="l")

############################################
savings
library(TSA)
y<-savings[2:100]
yprev<-savings[1:99]
fit<-lm(y~yprev)

summary(fit)
mu<-(fit$coef[1]/(1-fit$coef[2]))
plot(fit$residuals)
acf(fit$residuals)


acf(savings)
pacf(savings)
help(acf)


wf1<-readEViews("processes.wf1", as.data.frame=F)

Y1<-wf1$Y1[1:1000]
Y2<-wf1$Y2[1:1000]
Y3<-wf1$Y3[1:1000]
Y4<-wf1$Y4[1:1000]


plot(Y1, type="l")
plot(Y2, type="l")
plot(Y3, type="l")
plot(Y4, type="l")

pacf(Y1)
pacf(Y2)
pacf(Y3)
pacf(Y4)

acf(Y1)
acf(Y2)
acf(Y3)
acf(Y4)
