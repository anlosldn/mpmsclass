install.packages("fpp2")
library(fpp2)

# CREATE TIME SERIeS OBJECT
a <- ts(ausbeer, start = 1956, frequency = 4)

# AVERAGE
meanf(a, h=1)

# NAIVE (simply the last observation)
naive(a, h=1)
rwf(a, h=1) # naive is optimal when data follow a random walk
# which is the same as random walk forest

# SEASONAL NAIVE
# we set each forecast to the last observed value from the 
# same season of the year
snaive(a, h=4)
c <- snaive(a, h=4)
autoplot(c)

# ADD DRIFT TO THE VARIATION OVER TIME
# here we allow the fOrecasts to change over time by 
# the average change seen in the historical data
c <- rwf(a, h=4, drift=T)
autoplot(c)

##################
##   Examples ####
##################

beer2 <- window(ausbeer, start=1992, end=c(2007,4))

autoplot(beer2) + 
  autolayer(meanf(beer2, h=11),
            series = "mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series = "naive", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series = "Seasonal Naive", PI=FALSE) +
  ggtitle("Forecasts for Q beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title = "Forecast"))

## GOOGLE STOCK PRICE ##
autoplot(goog200) + 
  autolayer(meanf(goog200, h=40),
            series="mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naive", PI=FALSE) +
  autolayer(rwf(goog200, drift = TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily)") + 
  xlab("day") + ylab("Closing Price $")+
  guides(colour=guide_legend(title="Forecast"))


### TRANSFORMING DATA ################

# Calender Adjustments
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE)+
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

# Population Adjustments
# Use per cap

########################################
##### Mathematical Transformations #####
########################################

# when data shows variations that change with the level of the 
# series, we need to transform the data - logarithmic or power

# BOX-COX TRANSFORMATIONS #

(lambda <- BoxCox.lambda(elec)) # this generates optimum lambda
# we are looking for a lambda that makes the seasonal variation
# roughly the same across the whole series

# Compare the variation of the same data set
autoplot(elec) # before we transform the data - variation increases with level
autoplot(BoxCox(elec,lambda)) # after we transform the data

# BIAS-ADJUSTED Transformation #

fc <- rwf(eggs, drift=TRUE, lambda = 0, h=50, level = 50)
fc2 <- rwf(eggs, drift=TRUE, lambda = 0, h=50, level = 50, biasadj = TRUE)
autoplot(eggs) + 
  autolayer(fc, series = "Simple back transformation") +
  autolayer(fc2, series = "Bias Adjusted", PI=FALSE) +
  guides(colour=guide_legend(title = "Forecast"))

# the blue line shows the forecast medians (no adjsutment)
# while the red line shows the forecast means

##################################################
########## Residual Diagnostics ##################
##################################################

# residuals are what is left after fitting a model
# which are the differences between observations and 
# corrrepsonding fitted values (the unexplained variation)

# Good models 
#       1. uncorrelated residuals - otherwsie we are not capturing all info
#       2. Residuals have a mean of zero - otherwise bias preset
# 
# any forecast that does not satisfy these properties can be improved
# 
#       3. constant variance
#       4. normally distributed

### Forecasting Google Share Price ###
# stock preditions best forecast -> naive

autoplot(goog200) + 
  xlab("Day") + ylab("Closing Price $") +
  ggtitle("Google Stock Daily Price to Dec 2013")

res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from Naive Method")

# large spike corresponds with sharp move in share price, otherwise
# uncorrelated, mean zeroed, constant variance

gghistogram(res) + ggtitle("Histogram of residuals")
# normally distributed?  Perhaps this is an issue?

# ACF of residuals - to test for correlation between residuals

ggAcf(res) + ggtitle("ACF of residuals")
# all wihin band of insignificance

##############################################
#### PORTMANTEAU TEST for AUTOCORRELATION ####
##############################################

# test whether the first h autocorrelations are 
# significantly different from E(white noise)

# Large values of Q (test statistic) suggest the autocorrelaions
# do not come from white noise in the series

Box.test(res, lag = 10, fitdf = 0)
# the p-value = 0.4, so the result is not significant
# therefore autocorrelations not distuiguishable from
# white noise (Residuals NOT correlated)

Box.test(res, lag = 10, fitdf = 0, type = "Lj")
# p-value on Box-Ljung Test?

###############################################

checkresiduals(naive(goog200))

# Does all of the above in one simple function!

###############################################
######### Evaluating Forecast Accuracy ########
###############################################

# Training / Test Data
# Beware over-fitting - in ML it's a sign of incompetence or deceit

# CREATE THE TRAINING / TEST DATASETS

window(ausbeer, start=1995)

# or subset - last 5 years of the dataset
subset(ausbeer, start = length(ausbeer)-4*5)

# other indexing options
head(ausbeer, 4*5) # first 5 years of quarterly data
tail(ausbeer, 4*5) # last 5 years of quarterly data

# Forecast Error = difference between observed value and its forecast
# different from residuals: residuals -> training set, errors -> test set
# measure forecast accuracy by summarising forecast errors

# SCALE-DEPENDENT ERRORS: Mean Absolute Error & Root Mean Squared Error
# Minimising the RMSE - forecasting the mean; widely used

# PERCENTAGE ERRORS & SCALED ERRORS
# Comparable across different units

### example ###

beer2 <- window(ausbeer, start=1992,end=c(2007, 4))
beerfit1 <- meanf(beer2, h=10)
beerfit2 <- rwf(beer2, h=10)
beerfit3 <- snaive(beer2, h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=F) +
  autolayer(beerfit2, series="Naive", PI=F) +
  autolayer(beerfit3, series="Seasonal Naive", PI=F) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecast for Q beer production") + 
  guides(colour=guide_legend(title = "Forecast"))

beer3 <- window(ausbeer, start=2008)

# With beer2 set up as our training data 
# and beer3 as our test data, we can check our models 
# for accuracy

accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

#           RMSE    MAE     MAPE    MASE
# Mean      38.45   34.83   8.28    2.44
# Naive     62.29   57.40   14.18   4.01
# Snaive    14.31   13.40   3.17    0.94

# clearly the seasonal naive method is best in this case

################################
##### GOOGLE EXAMPLE ###########

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240))+
  autolayer(googfc1, PI=FALSE, series="Mean")+
  autolayer(googfc2, PI=FALSE, series="Naive")+
  autolayer(googfc3, PI=FALSE, series="Drift")+
  xlab("Day") + ylab("Closing Price $") +
  ggtitle("Google Stock Price") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)  
accuracy(googfc1, googtest)  
accuracy(googfc2, googtest)  
accuracy(googfc3, googtest)

#           RMSE    MAE     MAPE    MASE
# Mean      114.21  113.27  20.32   30.28
# Naive     28.43   24.59   4.36    6.57
# Snaive    14.08   11.67   2.07    3.12

###############################################
######### TIME SERIES CROSS VALIDATION ########
###############################################

# we test accuracy on one data point, using all the
# prior data points - we then compare the RMSE of the
# time series cross validation with the residual RMSE

e <- tsCV(goog200, rwf, drift=TRUE, h=1) # only one point ahead
sqrt(mean(e^2, na.rm = T)) # = 6.233
sqrt(mean(residuals(rwf(goog200, 
                        drift=T))^2, na.rm = T))
# Residual RMSE = 6.169

# Generally we select model with smallest RMSE computed
# using time series cross-validation

#############################
##  Using Pipe OPerators ####

#             %>%           #

# when nesting functions inside functions, one has to 
# read the code 'inside out'

# TIP: Use CTRL + SHIFT + M to insert pipes

goog200 %>% tsCV(forecastfunction = rwf, drift=T, h=1) -> e
e^2 %>% mean(na.rm=T) %>% sqrt() # = 6.233

goog200 %>% rwf(drift=T) %>% residuals() -> res
res^2 %>% mean(na.rm=T) %>% sqrt() # = 6.169

# the left side of each pipe is passed as the first argument
# of the function on the right

# Take 'goog200' series; pass it to rwf() with 'drift'
# compute residuals and store them as 'res'


#################################################

# in this example we take Google closing price for 200 
# consecutive trading days.  Compare a naive forecast for 
# 1 to 8 steps ahead and evaluate using MSE as forecast error measure

e <- tsCV(goog200, forecastfunction = naive, h=8)

# Compute MSE and remove missing values
mse <- colMeans(e^2, na.rm = T)

data.frame(h=1:8, MSE = mse) %>% 
  ggplot(aes(x=h, y=MSE)) + geom_point()


##############################################
##############################################
#########  PREDICTION INTERVALS #############
#############################################

#  is a range in which we expect y to lie
# +/- 1.96 std deviations of the h-step forecast

# intervals express the uncertainty in the forecasts

# for a 1-step forecast (naive) - no parameter estimates
# then std dev of forecast distirbution = std dev of residuals

# To produce a prediction interval, we must estimate the std dev
# we can derive forecast std dev from residual std dev

naive(goog200)
autoplot(naive(goog200))
goog200 %>% naive() %>% autoplot() # same as above using pipes

# when we cannot assume the forecast errors are normally distributed
# we can apply bootstrapping

naive(goog200, bootstrap=TRUE)
autoplot(naive(goog200, bootstrap=TRUE))

# General Forecast Function

forecast(ausbeer, h=4)

# Exercise
beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)
res <- residuals(fc)
autoplot(res)
checkresiduals(res)











