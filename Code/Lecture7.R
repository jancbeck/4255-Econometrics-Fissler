# Approximately slides 181 - 


# Case Study Labor Force --------------------------------------------------

# Read in laborforce data
laborforce <- hexView::readEViews("eviews/laborforce.wf1", time.stamp = FALSE)

# laborforce is a dataset on employment in austria around 1997/1998
# AGE: Age in years
# ALOS97/98: Dummy variable indicating whether the person was unemployed in 1997/1998
# CHANGEEMPL: Number of times a person changed employer
# GENDER: Dummy variable indicating gender
# WCOLLAR: Dummy variable indicating whether a person is a white collar worker or not
head(laborforce)

#### Slide 186
# Calculate median
summary(laborforce$AGE)
median(laborforce$AGE)
# demedian age

laborforce$AGE <- laborforce$AGE - median(laborforce$AGE)

# Estimate linear probability model
lin_prob_mod <- lm(ALOS98 ~ GENDER + AGE + ALOS97 + CHANGEEMPL + WCOLLAR, data = laborforce)
summary(lin_prob_mod)

coefs <- lin_prob_mod$coefficients
coefs <- round(coefs, 3) # Of course, we wouldn't usually round the coefficients. 
  # We just do this such that we can more easily follow the computations
# Transform them to percentage
coefs <- 100*coefs

## Slide 187
# Expected change for female, aged 50 instead of baseline
coefs["GENDER"] + 7*coefs["AGE"]
# Expected change for white collar, aged 35 instead of baseline
coefs["WCOLLAR"] - 8*coefs["AGE"]


# Expected probability for male, aged 60, blue collar, out of labor the previous year
coefs["(Intercept)"] + 17*coefs["AGE"] + coefs["ALOS97"]

# Expected probability for male, aged 85, blue collar, out of labor the previous year
coefs["(Intercept)"] + 42*coefs["AGE"] + coefs["ALOS97"]

# Expected probability for male, aged 25, white collar, changed employer twice:
coefs["(Intercept)"] - 18*coefs["AGE"] + coefs["WCOLLAR"] + 2*coefs["CHANGEEMPL"]

# Expected probability for male, aged 25, white collar, changed employer twice:
coefs["(Intercept)"] - 25*coefs["AGE"] + coefs["WCOLLAR"] + 2*coefs["CHANGEEMPL"]


#### Slide 200
# Estimate a probit and logit model
probit_mod <- glm(ALOS98 ~ GENDER + AGE + ALOS97 + CHANGEEMPL + WCOLLAR, 
                  family = binomial(probit), data = laborforce) 
summary(probit_mod)

logit_mod <- glm(ALOS98 ~ GENDER + AGE + ALOS97 + CHANGEEMPL + WCOLLAR, 
                 family = binomial(logit), data = laborforce) 
summary(logit_mod)


#### Slide 202
# Compute the restricted models
b0_logit <- logit_mod$coefficients["(Intercept)"]
b0_probit <- probit_mod$coefficients["(Intercept)"]

# Different scale for probit and logit model
cat(round(b0_logit, 3), round(b0_probit,3))

# Calculate baseline probability of unemployment
plogis(b0_logit)
pnorm(b0_probit)
# Contrast with LPM
coef(lin_prob_mod)["(Intercept)"]

#### Slide 206/207
# Effect of being female in LPM
coef(lin_prob_mod)["GENDER"]

# Effect changes in non linear models, depending on other values
# Define "new" observations for prediction
non_female <- data.frame(GENDER = c(0, 0, 0),
                         AGE = c(0, 0, 10),
                         ALOS97 = c(0, 0, 0),
                         CHANGEEMPL = c(0, 0, 0),
                         WCOLLAR = c(0, 1, 1))

# This creates synthetic observations which we can hand to predict()
non_female

# Create second data set of synthetic observations to calculate differences
female <- non_female
# Change gender on all synthetic observations
female$GENDER <- 1

# Same observations as before, but now FEMALE = 1 for all
female

# predict() can be used to calculate probabilities for new observations

# Now calculate differences to see effect of being female (ceteris paribus)
# in different situations
# Note: R applies subtraction element-wise
logit_diff <- round(predict(logit_mod, female, type = "response"),3) - round(predict(logit_mod, non_female, type = "response"),3)
probit_diff <- round(predict(probit_mod, female, type = "response"),3) - round(predict(probit_mod, non_female, type = "response"),3)

data.frame(logit = logit_diff, probit = probit_diff)


#### Slide 212
summary(probit_mod)
summary(logit_mod)
coefficients(probit_mod)
coefficients(logit_mod)
1.6*coefficients(probit_mod)
coefficients(logit_mod)/coefficients(probit_mod)

# This function creates the average probability for various values of the specified predictor (and potential interactions)
# It's a good way to visualize the effect of a predictor for different values.
# Don't wory too much about how the code exactly works - there is some advanced stuff going on here. I left some comments
# for the interested reader. 
explore_params <- function(mod, param, grouping_param, n = 100, silent = FALSE){
  
  # Its just nicer for some things
  library(ggplot2)
  
  # Extract covariates from model object
  covariates <- mod$data[, names(mod$data) %in% attr(mod$terms, "term.labels")]
  
  # Differentiate between dummy and non-dummy variables
  # It essentially just checks whether all values are either 0 or 1
  is.dummy <- apply(covariates, 2, function(x){
    all(x %in% c(0, 1))
  })
  
  # The grouping variable cant be continuous
  if (missing(grouping_param) == FALSE && is.dummy[grouping_param] == FALSE){
    stop("grouping_param has to be a dummy variable!")
  }
  
  # Dummy variables and continuous variables produce different plots, have to differentiate
  if (is.dummy[param]){
    # Generate synthetic observations for dummy = 0 and dummy = 1
    neg_synth <- pos_synth <- covariates
    
    # Overwrite all dummy variables with 0 for negative synth 
    neg_synth[,param] <- 0
    
    # Overwrite all dummy variables with 1 for positive synth
    pos_synth[, param] <- 1
    
    # Predict average probability of unemployment over synthetic observations
    pos_pred <- mean(predict(mod, pos_synth, type = "response"))
    neg_pred <- mean(predict(mod, neg_synth, type = "response"))
    
    # This is just stuff that ggplot2 wants
    data <- data.frame(Probability = c(neg_pred, pos_pred), val = as.factor(c(0, 1)))
    names(data)[2] <- param
    
    p <- ggplot(data, aes_string(y = "Probability", x = 0, fill = param)) +
      geom_bar(stat = "identity", position="dodge") + 
      ylab("Average Probability") +
      theme_bw() + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      ylim(c(0, 1)) +
      annotate("text", label = paste0("Difference: ", round(100*(pos_pred - neg_pred), 2), "%"),  
               x = -0.25, y = 0.95, hjust = 0.5)
    
  } else {
    # Get range of predictor variable
    par_range <- range(covariates[, param])
    
    # Create n evenly spaced values in the range of the param
    seq <- seq(from = par_range[1], to = par_range[2], length.out = n)
    
    # Loop over all points in seq and create synthetic observations
    avg_probs <- rep(NA, length(seq))
    
    if (missing(grouping_param)){
      avg_probs2 <- NULL
    } else {
      avg_probs2 <- rep(NA, length(seq))
    }
    
    
    for (i in 1:length(seq)){
      curr_dat <- covariates
      
      # Replace all values of param with current value to create synthetic observations
      curr_dat[, param] <- seq[i]
      
      # Create two seperate sets of synthetic observations if grouping_param was provided
      # This lets the user look at the effect of covariates on different groups
      if (missing(grouping_param) == FALSE){
        curr_dat[, grouping_param] <- 0
        curr_dat2 <- curr_dat
        curr_dat2[, grouping_param] <- 1
        
        avg_probs2[i] <- mean(predict(mod, curr_dat2, type = "response"))
      }
      
      # Calculate average probability for all synthetic observations at current value of param
      avg_probs[i] <- mean(predict(mod, curr_dat, type = "response"))
    }
    
    # Create data frame for ggplot again
    probs <- c(avg_probs, avg_probs2)
    data <- data.frame(Probability = probs, 
                       val = rep(seq, length.out = length(probs)),
                       group = as.factor(rep(0:1, each = length(seq))[1:length(probs)]))
    
    names(data)[2] <- param
    if (missing(grouping_param) == FALSE){
      names(data)[3] <- grouping_param
    }
    
    
    p <- ggplot(data)
    if (missing(grouping_param)) {
      p <- p + geom_line(lwd = 1.2, mapping = aes_string(x = param, y = "Probability"))
    } else {
      p <- p + geom_line(lwd = 1.2, mapping = aes_string(x = param, y = "Probability", color = grouping_param)) 
    }
    
    coord_x <- min(min(data[, param]))
    coord_y <- 0.95
    if (missing(grouping_param)){
      annot <- paste0("Max difference: ", round(100*(max(avg_probs) - min(avg_probs)), 2), "%")
      
    } else {
      annot <- c(paste0("Max difference, ", grouping_param, " = 0: ", round(100*(max(avg_probs) - min(avg_probs)), 2), "%"),
                      paste0("Max difference, ", grouping_param, " = 1: ", round(100*(max(avg_probs2) - min(avg_probs2)), 2), "%"))
      
      coord_y <- c(coord_y, 0.95*coord_y)
      
    }
    
    p <- p + theme_bw() + 
      ylim(c(0, 1)) + 
      ylab("Average Probability") + 
      annotate("text", label = annot, 
               x = coord_x,
               y = coord_y, hjust = 0, vjust = 0.5)     
  }
  if (silent == FALSE){
    print(p)
  }
  return(p)
}

# Like this!
explore_params(probit_mod, "AGE")
explore_params(logit_mod, "AGE")
explore_params(logit_mod, "GENDER")

# Binary models can produce different effects for effects for different groups, even without interactions
# This is a consequence of the non-linearity of the model
explore_params(probit_mod, "AGE", "ALOS97")

# Compare the models
prob_gend <- explore_params(probit_mod, "GENDER", silent = TRUE) + ggtitle("Probit model")
logit_gend <- explore_params(logit_mod, "GENDER", silent = TRUE) + ggtitle("Logit model")
gridExtra::grid.arrange(prob_gend, logit_gend, ncol = 2)


prob_age <- explore_params(probit_mod, "AGE", silent = TRUE) + ggtitle("Probit model")
logit_age <- explore_params(logit_mod, "AGE", silent = TRUE) + ggtitle("Logit model")
gridExtra::grid.arrange(prob_age, logit_age, ncol = 2)


prob_alos <- explore_params(probit_mod, "ALOS97", silent = TRUE) + ggtitle("Probit model")
logit_alos <- explore_params(logit_mod, "ALOS97", silent = TRUE) + ggtitle("Logit model")
gridExtra::grid.arrange(prob_alos, logit_alos, ncol = 2)


prob_empl <- explore_params(probit_mod, "CHANGEEMPL", silent = TRUE) + ggtitle("Probit model")
logit_empl <- explore_params(logit_mod, "CHANGEEMPL", silent = TRUE) + ggtitle("Logit model")
gridExtra::grid.arrange(prob_empl, logit_empl, ncol = 2)


prob_wcol <- explore_params(probit_mod, "WCOLLAR", silent = TRUE) + ggtitle("Probit model")
logit_wcol <- explore_params(logit_mod, "WCOLLAR", silent = TRUE) + ggtitle("Logit model")
gridExtra::grid.arrange(prob_wcol, logit_wcol, ncol = 2)

# In general the effects between probit/logit are very similar

# Calculate g(z) to see overall effect of parameters for individual
dnorm(qnorm(predict(probit_mod, type = "response")))
dlogis(qlogis(predict(logit_mod, type = "response")))


#### Slide 217
summary(logit_mod)
summary(probit_mod)
# Confidence intervals
confint(logit_mod)
confint(probit_mod)

# z-stat 
lmtest::coeftest(logit_mod)
lmtest::coeftest(probit_mod)

# Wald Test
lmtest::waldtest(logit_mod, "CHANGEEMPL")
lmtest::waldtest(probit_mod, "CHANGEEMPL")

# Wald Test with joint null hypotheses
lmtest::waldtest(logit_mod, c("CHANGEEMPL", "WCOLLAR"))
lmtest::waldtest(probit_mod, c("CHANGEEMPL", "WCOLLAR"))

# LR test
lmtest::lrtest(logit_mod, c("CHANGEEMPL", "WCOLLAR"))
lmtest::lrtest(probit_mod, c("CHANGEEMPL", "WCOLLAR"))

# 

### Slide 221
# Restrict models to exclude CHANGEEMPL, then compare with AIC/BIC
rest_logit_mod <- glm(ALOS98 ~ GENDER + AGE + ALOS97 + WCOLLAR, 
                      family = binomial(logit), data = laborforce)

rest_probit_mod <- glm(ALOS98 ~ GENDER + AGE + ALOS97 + WCOLLAR, 
                      family = binomial(probit), data = laborforce)
#AIC
AIC_dat <- data.frame(probit = c(AIC(probit_mod), AIC(rest_probit_mod)), 
                      logit =  c(AIC(logit_mod), AIC(rest_logit_mod)))
rownames(AIC_dat) <- c("Full model", "Without change employer")
AIC_dat

# BIC
BIC_dat <- data.frame(probit = c(BIC(probit_mod), BIC(rest_probit_mod)), 
                      logit =  c(BIC(logit_mod), BIC(rest_logit_mod)))
rownames(BIC_dat) <- c("Full model", "Without change employer")
BIC_dat

# Both prefer logit model, BIC is (once again) more restrictive

