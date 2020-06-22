install.packages("Quandl")
library(Quandl)
library(ggplot2)
library(forecast)

data <- Quandl("CHRIS/CME_C1", api_key="nQW1ynmsFtb42YM2sf4Z")

data %>% head()

ts <- xts(data$Last, as.Date(data$Date, format = "%d/%m/%Y"))
plot(ts)

myts <- ts["2016-05-18/2020-05-18"]
autoplot(myts, main="Near Dated Corn Futures Prices")

myts = na.approx(myts)
tail(myts)

set.seed(10) # there is a random element in NNAR model, set seed to get same result

# Creates a neural net model - automatically select parameters
fit <- nnetar(myts, p=3)

nnetforecast <- forecast(fit, h=180, PI=F) # takes too long to run the prediction intervals
library(ggplot2)
autoplot(nnetforecast)


