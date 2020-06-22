rm(list=ls())

library(fpp2)

ggplot(aes(x = Income, y = Consumption),
       data = as.data.frame(uschange)) + 
  geom_point() + 
  ggtitle("Quarterly Change in US consumption &
          personal income")

## We can see there is some relationship between Consumption and 
## Income

## We could build a simple linear model
uschange %>% head()
fit_lm <- lm(Consumption ~ Income, data = uschange)
summary(fit_lm)

## But we can perform time serie analysis - AR

## Two parts 
#   1. Regression - predictors
#   2. ARIMA - time series dynamics

# Check residuals of your model


###########################################
## REGRESSION WITH ARIMA ERRORS ###########

#ARIMA() fit regression with ARIMA error - xreg
# order arg. specifies order of ARIMA
# differencing applied to all variables

# auto.arima() will select best ARIMA model for the
# errors
# AICc is calculated - used to select best predictors

autoplot(uschange[,1:2], facets=T) +
  xlab("year") + ylab("") + 
  ggtitle("Quarterly Change in US Consumption 
          and Personal Income")

(fit <- auto.arima(uschange[,"Consumption"],
                   xreg = uschange[,"Income"], trace=T, approximation = F,
                   stepwise = F))

# The data are stationary - it is in % 
# Fitted model
#      y = 0.599 + 0.203x + nt
#     nt = ARIMA (0.6922 - 0.576 +0.198)

cbind("Regression Errors" = residuals(fit, type = "regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>% 
  autoplot(facets=T)

# the ARIMA errors should resemble a white noise series
checkresiduals(fit)

# p-value > 0.05 so residuals resemble white noise

#################################################################
#     FORECAST - Regression with ARIMA
#################################################################

fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8)) # next 8 quarters
autoplot(fcast) + xlab("Year") + 
  ylab("Percentage Change")


# we can compare this to an ARIMA on Consumption only
fcast_noXREG <- auto.arima(uschange[,"Consumption"], seasonal = F)
fcast_noXREG %>% forecast(h=8) %>% autoplot()
# Compare the prediction intervals


################################################
#### Example 2 #################################

qplot(Temperature, Demand, data = as.data.frame(elecdaily)) +
  ylab("Elec Demand (GW)") + xlab("Temp")

xreg <- cbind(MaxTemp = elecdaily[,"Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])

fit <- auto.arima(elecdaily[,"Demand"], xreg = xreg)
checkresiduals(fit)

# The model has some significant autocorrelation in the residuals
# Histogram shows large positive outlier which may impact Pred. Intervals

# Use estimated model to forceast 14-days ahead
# x = predictor variable = weather forecast
# we use 26 degrees for 14 days

fcast <- forecast(fit,
          xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),
                       Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electricity demand (GW)")

### Stochastic and Deterministic Trends ###
autoplot(austa) + xlab("Year") +  
  ylab("Millions of people") + 
  ggtitle("Total international visitors")

# Deterministic trend model
trend <- seq_along(austa)
(fit1 <- auto.arima(austa, d=0, xreg=trend))

#Stochastic trend model
(fit2 <- auto.arima(austa, d=1))

# The difference is apparent in the prediction intervals

fc1 <- forecast(fit1, xreg = length(austa) + 1:10)
fc2 <- forecast(fit2, h=10)
autoplot(austa) + 
  autolayer(fc2, series="Stochastic trend") + 
  autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from trend models") + 
  xlab("Year") + ylab("Visitors to Aust. (millions)") +
  guides(colour=guide_legend(title = "Forecast"))

# Assumption: Deterministic - slope of trend remains constant
# Stochastic trend - slope can change = avg. growth over historical period

##########################################################################
###################     LAGGED PREDICTORS ################################

# impact of a predictor may not be immediate.  
# Impact on sales of advertsing spend could last months

# US Insurance Example
autoplot(insurance, facets = TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")


# build a model that considers four months (lagged) ad spend

Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"], -1),
  AdLag2 = stats::lag(insurance[,"TV.advert"], -2),
  AdLag3 = stats::lag(insurance[,"TV.advert"], -3)) %>% 
  head((NROW(insurance)))

fit1 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1], stationary = TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1:2], stationary = TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1:3], stationary = TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg = Advert[4:40,1:4], stationary = TRUE)

# Now choose optimal lag length for advertising based on AICc

c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])


# the best model with the smallest AICc has two lagged predictors
# advertising in the current month and the prvious month

# NOw re-estimate the model using all the data
(fit <- auto.arima(insurance[,1], xreg=Advert[,1:2],
                   stationary = T))

# We can forecast future quoates based on future advertsing 

fc8 <- forecast(fit, h=20,
                xreg = cbind(AdLag0 = rep(8,20),
                             AdLag1 = c(Advert[40,1], rep(8,19))))
autoplot(fc8) + ylab("Quotes") +
  ggtitle("Forecast quotes with future advertising set to 8")













  
  
  
  
  
  
  










