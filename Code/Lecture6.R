# Approximately slides 151 - 180

library(lmtest)
library(MASS)


# Case Study Road Safety --------------------------------------------------

# Read in EViews workfile
deaths <- hexView::readEViews("../data/deaths.wf1", as.data.frame = FALSE)

head(deaths)

# Cut length of all variables to uniform size
deaths <- lapply(deaths, function(x){
  return(x[1:length(deaths$KID_EQ01)])
})

# Remove redundant/corrupt data
deaths$Date <- NULL
deaths$KID_EQ01 <- NULL
deaths$KID6T10F <- NULL

# Transform into data frame and take a look
deaths <- as.data.frame(deaths)

# deaths is a daily dataset on fatal accidents in Linz
# Linz is a small town near the Danube
#
# HOLIDAY: 1 if day is a holiday, 0 else
# KID14: Nr. of children under 14 killed in traffic
# KID6T10: Nr. of children from ages 6 to 10 killed in traffic
# LEG_CHANGE: 1 after legislation was introduced to kill less kids in traffic 
# LINZ: Total nr. of people killed in traffic
# SEN: Number of seniors killed in traffic
# WINTER: 1 if winter, 0 else

head(deaths)

# Take a look at all time series simultaneously 
plot(ts(deaths, start = c(1987, 1), frequency = 12))
# If we want to explain number of deaths of children between 6 and 10, we need non normally distributed errors.
# Data is count data, very obviously not able to be modelled with conditionally normally distributed errors.
table(deaths$KID6T10)
plot(table(deaths$KID6T10))


# Case Study Labor Force --------------------------------------------------

# Read in laborforce data  
laborforce <- hexView::readEViews("../data/laborforce.wf1", time.stamp = FALSE)

# laborforce is a dataset on employment in austria around 1997/1998
# AGE: Age in years
# ALOS97/98: Dummy variable indicating whether the person was unemployed in 1997/1998
# CHANGEEMPL: Number of times a person changed employer
# GENDER: Dummy variable indicating gender
# WCOLLAR: Dummy variable indicating whether a person is a white collar worker or not
head(laborforce)

# To predict if a person is unemployed in 97/98, need a model for binary data
plot(table(laborforce$ALOS97))

# To predict number of changes in employer, need a model for count data
plot(table(laborforce$CHANGEEMPL))


# Case Study Road Safety --------------------------------------------------

# Linear model for child deaths
lin_mod_deaths_1 <- lm(KID6T10 ~ HOLIDAY + LEG_CHANGE + TREND, deaths)
summary(lin_mod_deaths_1)

lin_mod_deaths_2 <- lm(KID6T10 ~ HOLIDAY + LEG_CHANGE, deaths)
summary(lin_mod_deaths_2)

c(AIC(lin_mod_deaths_1), BIC(lin_mod_deaths_1))
c(AIC(lin_mod_deaths_2), BIC(lin_mod_deaths_2))
# Restricted model has lowest AIC/BIC
# But what about the prediction/residuals?


plot(deaths$KID6T10, type = "l", ylim = c(0, 6))
lines(predict(lin_mod_deaths_1), col = "red", lwd = 2)
lines(predict(lin_mod_deaths_2), col = "forestgreen", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Full model", "Restricted model"), 
       col = c("red", "forestgreen"),
       lty = c(1, 2),
       lwd = 2,
       inset = 0.01,
       bty = "n")

#par(mfrow=c(1,1))
plot(residuals(lin_mod_deaths_2))
plot(table(residuals(lin_mod_deaths_2)))
# Only a few possible values for the residuals -> definitely not normally distributed


# Poisson Model for Deaths ------------------------------------------------

poi_mod_deaths_1 <- glm(KID6T10 ~ HOLIDAY + LEG_CHANGE + TREND, "poisson", deaths)
summary(poi_mod_deaths_1)

poi_mod_deaths_2 <- glm(KID6T10 ~ HOLIDAY + LEG_CHANGE, "poisson", deaths)
summary(poi_mod_deaths_2)

c(AIC(poi_mod_deaths_1), BIC(poi_mod_deaths_1))
c(AIC(poi_mod_deaths_2), BIC(poi_mod_deaths_2))
# Reduced model has smallest AIC/BIC. It is also smaller than the one from 
# quasi-normal model.

plot(deaths$KID6T10, type = "l", ylim = c(0, 6))
lines(predict(poi_mod_deaths_1, type = "response"), col = "red", lwd = 2)
lines(predict(poi_mod_deaths_2, type = "response"), col = "forestgreen", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Full model", "Restricted model"), 
       col = c("red", "forestgreen"),
       lty = c(1, 2),
       lwd = 2,
       inset = 0.01,
       bty = "n")

# Let's zoom in on a prediction for a single time point
time_point <- 100
mean_lin <- predict(lin_mod_deaths_2, deaths[time_point,])
sd_lin <- sigma(lin_mod_deaths_2)
mean_poi <- predict(poi_mod_deaths_2, deaths[time_point,], type = "response")

plot(y = dpois(0:8, mean_poi),
     x = 0:8, col = "forestgreen", 
     type = "h", lwd = 3, xlim = c(-4, 8), ylab = "",
     xlab = "Deaths", ylim = c(0, 0.35))

curve(dnorm(x, mean_lin, sd_lin), 
      from = -4, to = 8, col = "firebrick", lwd = 2,
      add = TRUE)

legend("topright", 
       legend = c("Normal model", "Poisson model"), 
       col = c("firebrick", "forestgreen"),
       lwd = 2,
       inset = 0.01,
       bty = "n")

abline(v = deaths$KID6T10[time_point])


#par(mfrow=c(1,1))
plot(residuals(poi_mod_deaths_2))
plot(table(residuals(poi_mod_deaths_2)))

## Slide 175
c(AIC(lin_mod_deaths_2), AIC(poi_mod_deaths_2))
c(BIC(lin_mod_deaths_2), BIC(poi_mod_deaths_2))
# Poisson model has smaller AIC/BIC

## Slide 176
coeftest(lin_mod_deaths_2)
coeftest(poi_mod_deaths_2)

# Rescale for interpretation
poi_mod_deaths_2$coefficients[3]
exp(poi_mod_deaths_2$coefficients[3])-1


## Slide 177
resids <- residuals(poi_mod_deaths_2, type = "pearson")
plot(resids, type="l")
abline(0,0, col= "blue")
# Look pretty homoscedastic



# Dealing with Overdispersion - the Negative Binomial Model ---------------

## Slide 179
# Try fitting to deaths data
nb_mod_deaths <- glm.nb(KID6T10 ~ HOLIDAY + LEG_CHANGE, deaths, control = list(epsilon = 1e-8, maxit = 40, trace = FALSE))
# Not properly converging

# eta^2 = 1/theta; has inverse relationship to eta^2 on the slides -> trying to go to infinity
nb_mod_deaths$theta
1/nb_mod_deaths$theta


fabrics <- hexView::readEViews("../data/fabric_faults.wf1")

# ERRORS: Number of faults in fabric
# LENGTH: Length of piece of fabric
head(fabrics)


# Estimate a negative binomial model
nb_mod_fab <- glm.nb(ERRORS ~ LENGHT, fabrics)
summary(nb_mod_fab)

# Calculate eta^2 (level of overdispersion)
1/nb_mod_fab$theta

# Compare to Poisson model
poi_mod_fab <- glm(ERRORS ~ LENGHT, family = "poisson", fabrics)
summary(poi_mod_fab)

c(AIC(nb_mod_fab), AIC(poi_mod_fab))
c(BIC(nb_mod_fab), BIC(poi_mod_fab))
# Both AIC and BIC prefer NB model
