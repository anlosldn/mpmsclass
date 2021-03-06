#############################################################

#############  Bitcoin Example using GARCH ##################

#############################################################

# this script is an example of forecasting volatility using 
# a GARCH model on a BTC-USD time series


install.packages(c("quantmod","rugarch","rmgarch"))
library(quantmod)
library(rugarch)
library(rmgarch)

# The quantmod package allows us to upload specific data

startDate = as.Date("2015-01-02")
endDate = as.Date("2020-04-28")

# we can then import an 'xts' object:   xts = Time Series in R
getSymbols("BTC-USD", from = startDate, to = endDate)

head(`BTC-USD`)

chartSeries(`BTC-USD`)

# In a GARCH model we are estimating volatility, so we need to work with returns
# we use the dailyReturn function

rBTC <- dailyReturn(`BTC-USD`)

# Stick that in a dataframe (to make it easier to work with later)
dfBTC <- as.data.frame(rBTC)

# We need to specify the GARCH model, but we start with the default specification

# We are using a univariate (only BTC) GARCH Model

ug_spec = ugarchspec() # this is a list with relavent model specs

ug_spec 
# sGARCH(1,1) = standard GARCH, 
# mean model = ARMA(1,1) the 'FI' in ARFIMA stands for 'fractional integration' but none in ARFIMA(1,0,1)
# Distribution = Epsilion (errors) are normally distributed

# We can change the mean model from an ARMA(1,1) to an ARMA (1,0), which is an AR(1)

ug_spec = ugarchspec(mean.model = list(armaOrder=c(1,0)))

### Model Estimation ###

# We have now specified a model, we need to estimate the best parameters
# we can call the ugarchfit() function

ugfit = ugarchfit(spec = ug_spec, data = rBTC)

# this ceates a list with resultas of the estimation

ugfit

# Notice the 'Optimal Parameters' - refer to the GARCH formula
# omega = constant term of the model
# alpha1 = Coeff. for the squared residual for past period
# beta1 = Coeff. for the conditional variance from the last period (lagged variance)

# ar1 is the AR(1) coefficient of the mean model

# In order to use these outputs we extract parameter estimates, standard errors & residuals
# the code below shows you a list of things you can extract

paste("Elements in the @model slot")
names(ugfit@model)
paste("Elements in the @fit slot")
names(ugfit@fit)

# So we say we want to extract and print the coefficients
ugfit@fit$coef  # this gives us our 5 estimated coefficients

# we can likewise pull the estimated conditional variances
ug_var <- ugfit@fit$var

# and save the estimated square residuals
ug_res2 <- (ugfit@fit$residuals)^2

# and plot the squared residuals and estimated conditionl variance
plot(ug_res2, type = "l")
lines(ug_var, col = "green")

# we have a typical GARCH (financial asset) volatility - clusters of high / low volatility

###################################
### Forecasting using our model ###
###################################

# we can use the model to forecast the conditional variance

ugfore <- ugarchforecast(ugfit, n.ahead = 10)
ugfore

# This produces forecasts of the expected returns ('Series') and Conditional Variance ('Sigma')

# the 'ugfore' object contains two slots (@model and @forecast)
# we use names(ugfore@forecast) to see under what names the elements are saved

names(ugfore@forecast)

# for instance, we extract conditional volatility forecast

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l", main = "BTC Conditional Volatility Forecast")

# we can see from the plot we are forecasting an increase in volatility

# if we wanted to plot this against the last 20 observations

ug_var_t <- c(tail(ug_var, 20), rep(NA,10)) # gets last 20 observations
ug_res2_t <- c(tail(ug_res2, 20), rep(NA, 10)) # gets last 20 observations
ug_f <- c(rep(NA,20), (ug_f)^2)

plot(ug_res2_t, type="l")
lines(ug_f, col="red")
lines(ug_var_t, col="blue")

# black line is the 20 last squared residuals
# the blue line is the 20 last conditional variances - notice how it adjusts after a spike in variance (conditional)

# the red line is the conditional volatilty forecast


#################################################
#### BTC - USD Price action last 30 days ########
#################################################

BTC_Close = `BTC-USD`$'BTC-USD.Close'

BTC_Close_t <- tail(BTC_Close, 30)
plot(BTC_Close_t, type="l", main="BTC-USD Closing Price")

# Using GARCH model to predict BTC/USD 10-days ahead

BTC_forecast <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")
BTCGarch1 <- ugarchfit(spec = BTC_forecast, data = BTC_Close)
BTCGarch1

BTCPredict <- ugarchboot(BTCGarch1, n.ahead = 10, method = c("Partial","Full")[1])

# this plots the series - the forecast price
plot(BTCPredict, which=2)

# this plots the forecasted conditional volatility 
plot(BTCPredict, which=3)















































