rm(list = ls())
install.packages('fpp2')
library(fpp2)

#############################################################
#                   EXPONENTIAL SMOOTHING                   #
#############################################################

## SIMPLE EXPONENTIAL SMOOTHING ##

oildata <- window(oil, start=1996)
oildata %>% autoplot() + ylab("Oil (million of tons") + xlab("Year")

# Naive - the next value (forecast) = last value
# IOW, most recent observation weighted = 1.0

# Average - we weight all observations equally

# Exp Smoothing - we weight average (most recent = biggest weight)

############ OPTIMZATION ###############

# Example of Simple Exponential Smoothing 

oildata <- window(oil, start=1996)

# Estimate parameters
fc <- ses(oildata, h=5)
summary(fc)
# returns parameter estimates a = 083 & l = 446.6
# by minimising the SSE

#Accuracy of one-step ahead training erros
round(accuracy(fc),2)

autoplot(fc) + autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

### HOLT'S EXPONENTIAL SMOOTHING: Example Air Passengers ###

air <- window(ausair, start=1990)
fc <- holt(air, h=5)
summary(fc)
#the smoothing paramters are estimated by minimising
#the SSE for the 1-step training errors
# Alpha = 0.8302, Beta = 0.0001

### DAMPED TREND METHODS ###
# addditional varaible to demapen trend which can lead to 
# over-forecasts with Holt's

fc <- holt(air, h=15)
fc2 <- holt(air, damped=T, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series = "Holt's method", PI=F) +
  autolayer(fc2, series="Damped Holt's method", PI=F) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (m)") +
  guides(colour=guide_legend(title = "Forecast"))

### Example: Sheep in Asia ###

autoplot(livestock) +
  xlab("Year") + ylab("Livestock in Asia")

# Time Series Cross Validation to comapre the 1-step forecast accuracy
# of the three methods

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=T, h=1)

mean(e1^2, na.rm=T) # 178.25
mean(e2^2, na.rm=T) # 173.37
mean(e3^2, na.rm=T) # 162.63

mean(abs(e1), na.rm=T) # 8.532
mean(abs(e2), na.rm=T) # 8.803
mean(abs(e3), na.rm=T) # 8.024

# We can see that Damped Holt's = lowest MAE and MSE
# so we select that

fc <- holt(livestock, damped = T)
summary(fc)
fc[["model"]] # similar to summary() above

# beta = 0 (almost) so trend is not changing over time

autoplot(fc) + xlab("Year") + ylab("Livestock in Asia (m)")

########################################################
##### Holt-Winter's Seasonal Smoothimg #################

# The forecast equation + three smoothing operations

# use the additive method when seasonal variation is constant
# use multiplicative when seasonal variation is proportionate to level 

aust <- window(austourists, start=2005)
fit1 <- hw(aust, seasonal="additive")
fit2 <- hw(aust, seasonal="multiplicative")
  
autoplot(aust) +
  autolayer(fit1, series = "HW Additive", PI=F) +
  autolayer(fit2, series="HW Multiplicative", PI=F) +
  ggtitle("Forecasts from Holt-Winter method") + xlab("Year") +
  ylab("Vistors to Australia (m)") +
  guides(colour=guide_legend(title = "Forecast"))

summary(fit1) # HW Additive
# alpha = 0.3063, beta = 0.0003, gamma = 0.426, RSME = 1.763
# AICc = 239.7112, BIC = 250.48

summary(fit2) # HW Multiplicative
# alpha = 0.4406, beta = 0.0134, gamma = 0.0023, RSME = 1.576
# AICc = 226.43, BIC = 237.189

# when models have the same naumber of parameters to estimate,
# we use the  traing RMSE from both models.
# in above example we select Multiplicative 

aust %>%  decompose(type="multiplicative") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")

aust %>%  decompose(type="additive") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition")

## Example: Holt-Winters Damped, Multiplicative

fc <- hw(subset(hyndsight, end=length(hyndsight)-35),
         damped=TRUE, seasonal="multiplicative", h=35)
autoplot(hyndsight) + 
  autolayer(fc, series="HW multi damped", PI=FALSE) +
  guides(colour=guide_legend(title="Daily Forecasts"))

#######################################################
###############  ESTIMATING ETS MODELS ################
#######################################################

# Alternative to minimisng the SSE to select model is to
# maximise the LIKELIHOOD.

# Likelihood = P(of data arising from specifed model)

# Large Likelihood = Good model

# for an additive model, maximising the likelihood (assuming)
# normal distribution of errors) = minimising SSE

# Using the ETS framework , we use Info Criteria for model
# selection

aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)

# the model selected is ETS(M, A, M)

# compare outputs of ETS to HW methods and notice why forecasts
# will differ. 

autoplot(fit) # shows the states over time

cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit, type='response')) %>% 
        autoplot(facet=T) + xlab("Year") + ylab("")

# ETS point forecasts are equal to the medians of the forecast distributions
fit %>% forecast(h=8) %>% 
  autoplot() +
  ylab("International Visitors to Australia")










