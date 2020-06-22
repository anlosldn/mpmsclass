########## SIMPLE NEURAL NET EXAMPLE IN R #############

rm(list=ls())

# select directory in windows, copy PATH, then run before pasting...(takes care of Windows backslash)
x <- gsub("\\\\", "/",readClipboard())
setwd(x) #set working directory 
getwd()

install.packages(c('forecast','zoo','quantmod'))
library(forecast) # neural net function is in here
library(zoo)
library(quantmod)
library(ggplot2)

# we can scrap the latest Google Share Price data into R
GOOG = getSymbols("GOOG", auto.assign = F)
chart_Series(GOOG)

myts <- GOOG$GOOG.Close
myts %>% head()

plot(myts, main="Google Closing Share Price")

set.seed(15)
fit <- nnetar(myts) # this should take a while to run

nn_forecast <- forecast(fit, h=180, PI=F) # takes too long to run the prediction intervals
autoplot(nn_forecast)

# we can see the model takes an AR33, with 17 layers

nn_forecast %>% tail()


