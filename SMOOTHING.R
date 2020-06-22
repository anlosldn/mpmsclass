rm(list = ls())

# we can use a smoothing function such as a moving average
# to remove the variabilty and identify a trend

plot(AirPassengers)

plot(ma(AirPassengers, 6))

# there's a 6 month moving average
autoplot(AirPassengers, series = "Data", lwd = 3) +
  autolayer(ma(AirPassengers, 6), series = "6-MA", lwd = 3) + 
  ggtitle("AirPassengers vs. 6-MA")
# so each point on the 6-MA line is an average of the
# previous 6 data points

# there's a 6 month moving average
autoplot(AirPassengers, series = "Data", lwd=3) +
  autolayer(ma(AirPassengers, 12), series = "12-MA", lwd=3) + 
  ggtitle("AirPassengers vs. 12-MA")
# we are basically left with the TREND line 
# which we would have got from running the Decompose function

model1 <- decompose(AirPassengers, type = 'multiplicative')

plot(model1)

# plot the trend only against the 12-MA
autoplot(model1$trend, series = "Trend", lwd=2) +
  autolayer(ma(AirPassengers, 12), series = "12-MA", lwd = 2) + 
  ggtitle("Trend vs. 12-MA")

# to illustrate the point...
autoplot(model1$trend, series = "TREND", lwd = 3) +
  autolayer(ma(AirPassengers, 11), series = "11-MA", lwd = 2) +
  autolayer(ma(AirPassengers, 6), series= "6-MA", lwd = 2)
  ggtitle("Trend vs. 11-MA")
  
  
  ##################################################################
#### EXPONENTIAL SMOOTHING #### weight the most recent observations
  
  # ses() for datasets with no trend or seasonality
  
  # holt() for data with trend but NO seasonality
  #     we can dampen the trend using 'damped' argument
  
  # hw() for data with trend AND seasonality 
  
  ###################################################
  
  # or we could just get R to do this for us automatically
  # using the ets() function
  
  # R will look at the data, perform the decomposition amd select the
  # best fitting model using information criteria
  
  # Smoothing coefficients
  # closer to 1 = recent lags heaviliy weighted (like a 3-MA for example)
  # closer to 0 = smoother MA (more like 12-MA)
  
library(forecast)

etsmodel = ets(AirPassengers); etsmodel

# we can see we get an ETS(M,Ad,M)
# check out the smoothing coefficients - closer to 1, the more we rely on recent data
# alpha = close to 1


# Compare the actual data to our fitted model

plot(AirPassengers, lwd = 3)
lines(etsmodel$fitted, col = 'red', lwd=2)
# not a bad fit 
  
# lets do what we came for...FORECAST
plot(forecast(etsmodel, h=10))
# lets take a closer look
plot(forecast(etsmodel, h = 10), xlim=c(1958,1963))



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  








