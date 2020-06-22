
rm(list=ls())

# select directory in windows, copy PATH, then run before pasting...(takes care of Windows backslash)
x <- gsub("\\\\", "/",readClipboard())
setwd(x) #set working directory 
getwd()


install.packages(c("quantmod","rugarch","rmgarch",
                   "tidyverse","zoo","forecast","Quandl"))
library(quantmod) # this allows us to download stock data directly into R
library(rugarch)
library(rmgarch)
library(tidyverse)
library(zoo)
library(forecast)
library(ggplot2)
library(Quandl)

#Import Corn Prices
df <- Quandl("CHRIS/CME_C1", api_key="nQW1ynmsFtb42YM2sf4Z")

# Convert df into a time series object
df1 <- xts(df$Last, as.Date(df$Date, format = "%d/%m/%Y"))

df1 = na.approx(df1) # fills in any missing data points
class(df1)
plot(df1, type = "l", main = "Continuous Near Dated Corn Futures")

# From the plot we can see the time series is heteroskedastic - non-constant variance
# even though it is long term, even in the short term, we can see non-stationarity

df5 <- df1["2020-01-01/2020-05-18"]
plot(df5, main="Corn Price per Bushel in 2020")

ndiffs(df5)

# So clearly we need to transform our data (first differencing)
rCorn <- dailyReturn(df1["2017-05-18/2020-05-18"])
plot(rCorn, main="Corn Futures Daily Price Moves since May 2017")

# we typicaly build several GARCH models and then cross validate them
# we use the ugarchspec() 
ug_spec = ugarchspec()
ug_spec # gives us the model defaults

# Base Model 
GARCHModel_1 = ugarchfit(spec = ug_spec, data = rCorn) # this is a default sGARCH s=standard

# Model 2
ug_spec2 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model = "std")
GARCHModel_2 = ugarchfit(spec = ug_spec2, data = rCorn)

# now we need to comapre the AIC of each model - we select the lowest AIC
# GARCHModel_1 = -5.8911
# GARCHModel_2 = -5.9902

# Model 3
ug_spec3 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),mean.model = list(armaOrder=c(2,2)),distribution.model = "std")
GARCHModel_3 = ugarchfit(spec = ug_spec3, data = rCorn)
GARCHModel_3 # AIC = -5.9878

## We settle on GARCHModel_2 as our best model
CornFuturesPredict <- ugarchboot(GARCHModel_2, n.ahead = 10,
                                 method = c("Partial","Full")[1]) # 10 days ahead 

plot(CornFuturesPredict, which = 2) # forecats prices
plot(CornFuturesPredict, which = 3) # forecast volatility

# the key information is contained in the prediction intervals

#####  Neural Net Forecast - Near Dated Corn Futures Prices #######

myts <- df1["2015-05-18/2020-05-18"]
autoplot(myts, main="Near Dated Corn Futures Prices - Last 5 Years")

#myts = na.approx(myts)
#tail(myts)

set.seed(250) # there is a random element in NNAR model, set seed to get same result

# Creates a neural net model - automatically select parameters
fit <- nnetar(myts, p=20)

nnetforecast <- forecast(fit, h=90, PI=F) # takes too long to run the prediction intervals
library(ggplot2)
autoplot(nnetforecast)

# for comparison
fit1 <- nnetar(myts, p=3)
nnetforecast1 <- forecast(fit1, h=90, PI=F)
autoplot(nnetforecast1)

# for comparison
fit2 <- nnetar(myts)
nnetforecast2 <- forecast(fit2, h=90, PI=F)
autoplot(nnetforecast2)









