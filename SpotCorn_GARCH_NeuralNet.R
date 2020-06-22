############################################################################
###### GARCH and Neural Net Example - US Corn Spot Price per Bushel ########
############################################################################

rm(list=ls())

# select directory in windows, copy PATH, then run before pasting...(takes care of Windows backslash)
x <- gsub("\\\\", "/",readClipboard())
setwd(x) #set working directory 
getwd()


install.packages(c("quantmod","rugarch","rmgarch",
                   "tidyverse","zoo","forecast"))
library(quantmod) # this allows us to download stock data directly into R
library(rugarch)
library(rmgarch)
library(tidyverse)
library(zoo)
library(forecast)
library(ggplot2)

#Import Corn Prices
df <- read.csv("corn_prices.csv")

# Convert df into a time series object
df1 <- xts(df$Spot, as.Date(df$ï..date, format = "%d/%m/%Y"))
class(df1)
plot(df1, type = "l", main = "Corn Price per Bushel")

# From the plot we can see the time series is heteroskedastic - non-constant variance
# even though it is long term, even in the short term, we can see non-stationarity

df5 <- df1["2020-01-01/2020-05-18"]
plot(df5, main="Corn Price per Bushel in 2020")

# So clearly we need to transform our data (first differencing)
rCorn <- dailyReturn(df1)

# we typicaly build several GARCH models and then cross validate them
# we use the ugarchspec() 
ug_spec = ugarchspec()

ug_spec

ugfit = ugarchfit(spec = ug_spec, data = rCorn)

ugfit@fit$coef

ug_var <- ugfit@fit$var

# and save the estimated square residuals
ug_res2 <- (ugfit@fit$residuals)^2

# and plot the squared residuals and estimated conditionl variance
plot(ug_res2, type = "l")
lines(ug_var, col = "red", lwd=3)


ugfore <- ugarchforecast(ugfit, n.ahead = 10)
ugfore

# This produces forecasts of the expected returns ('Series') and Conditional Variance ('Sigma')

# the 'ugfore' object contains two slots (@model and @forecast)
# we use names(ugfore@forecast) to see under what names the elements are saved

# names(ugfore@forecast)

# for instance, we extract conditional volatility forecast

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l", main = "Corn Price Conditional Volatility Forecast")

Corn_Close = df$Spot
Corn_Close_t <- tail(Corn_Close, 90) # last 90 days


plot(Corn_Close_t, type="l", main="Corn Futures - Underlying Spot Price per Bushel")

# Using GARCH model to predict Corn Spot Price 10 days ahead

Corn_forecast <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model = "std")
Corn_Garch1 <- ugarchfit(spec = Corn_forecast, data = Corn_Close)
Corn_Garch1

Corn_Predict <- ugarchboot(Corn_Garch1, n.ahead = 10, method = c("Partial","Full")[1])

# this plots the series - the forecast price
plot(Corn_Predict, which=2)

# this plots the forecasted conditional volatility 
plot(Corn_Predict, which=3)

########################################################################

ts_nn <- df1["2016-05-18/2020-05-18"] 

plot(ts_nn)

set.seed(1) # there is a random element in NNAR model, set seed to get same result

# Creates a neural net model - automatically select parameters
fit <- nnetar(ts_nn, p=2)

nnetforecast <- forecast(fit, h=270, PI=F) # takes too long to run the prediction intervals
library(ggplot2)
autoplot(nnetforecast)


