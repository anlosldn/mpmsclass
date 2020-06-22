library(fpp2)

###################################################
########### SIMPLE LINEAR REGRESSION ##############
###################################################

# Intercept = predcited value of y when x=0
# Slope = rise over run

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

# Scatterplot of Consumption ~ Income
# in R the symbol ~ (tilde) means 'as a function of'

uschange %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quartherly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Estimate the regression equation

tslm(Consumption ~ Income, data = uschange)
# equation of the blue line

##################################################
######### MULTPLE LINEAR REGRESSION ##############
##################################################

# two or more predictor variables
# coefficients measure the margin effects of the dependent variables

# In the example we are forecasting CONSUMPTION
# we can consider several factors:
#   Industrial Production, 
#   Savings Rates 
#   & Change in Unemployment Rate 

uschange %>% 
  as.data.frame() %>%
  GGally::ggpairs()

######### REGRESSION ASSUMPTIONS ########

# 1. The model reasonaly represents reality
# 2. Errors:
#         mean zero (no systematic bias)
#         No autocorrelation 
#         Independent of predoctor variables

# 3. Errors should be normal dist. with constant variance

####### LEAST SQUARES ESTIMATION ##########

# Minimise the sum of the squared errors
# tslm() fits a model to a time series

fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment +
    Savings, data=uschange)
summary(fit.consMR)

# Plot a fitted line to actual values

autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") + 
  ggtitle("Percent change in US consumption") +
  guides(colour=guide_legend(title = " "))

# Plot actual Consumption against Predicted
cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Data, y=Fitted)) +
    geom_point()+
    ylab("Fitted (predicted)") +
    xlab("Data (Actual)") +
    ggtitle("Percent change in US Consumption") +
    geom_abline(intercept = 0, slope=1)

## Goodness of Fit - Coefficent of Determination - R-Square

# square of the correl. between observed values of y
# and the predicted values of y

# represents the proportion of the variation in y,
# that is explained by the model

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Std Error of the Regression #
# a.k.a. RSE - Residual Std Error

# related to the size of the average error that the model produces

# residuals are the unpredictable component of the observation
# the average of residuals = 0 and
# correl between residuals and observations of
# predictor variable = 0

####################################################

# After fitting a model, you must plot the residuals
# to ensure the assumptions of the model have been satisfied

# ACF Plot of residuals
# we do not want the residuals to be autocorrelated
# (otherwise there is still information in the time series)

# Breusch-Godfrey Test - for autocorrelation in residuals
# Histogram - check distribution of residuals

checkresiduals(fit.consMR)
# p-value of Breusch-Godfrey > 0.05 so ACF not significant

## Random Residuals against Predictors ##
df <- as.data.frame(uschange)
df[,"Residuals"] <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

## Residual Plots against Fitted Values
cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

# if there is a pattern in the plot of residuals to 
# fitted values = heteroscedasticity = variance of the
# residuals is not constant, requiring transformation

## Example of Spurious Correlation 
# Non-stationary time series can lead to spurious regressions

aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)
checkresiduals(fit)

########################################################
###############  USEFUL PREDICTORS #####################
########################################################

# TREND

# DUMMY VARIABLES

# SEASONAL DUMMY VARIABLES
beer2 <- window(ausbeer, start=1992)
autoplot(beer2)+xlab("Year")+ylab("Megalitres")

# clearly strong quarterly seasonality in ts
# to forecast..

fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)

# average downward trend of -0.34 megalitres per quarter
# Q2 is -34.7 megalitres lower than Q1 (on average)
# Q3 is -17.2 megalitres lower that Q1 (on average)
# Q4 is 72.8 megalitres higher than q1 (on average)

autoplot(beer2, series = "Data") +
  autolayer(fitted(fit.beer), series = "Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

cbind(Data=beer2, Fitted=fitted(fit.beer)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Data, y=Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette = "Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

# Using a Fourier Series as an alternative to seasonal dummy variables

fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)

# using fourier(m, K) - m = seasonal period, where
# K = how many pairs of sin and cosine terms to include


###########################################################
################# SELECTING PREDICTORS ####################
###########################################################

# We need a measure of predictive accuracy when selecting 
# the predictor variables for our models

CV(fit.consMR)

# AdjR2 - How well the model fits the historical data
# adding any variable increases R2, so we need to adjust

# CV - Cross Validation - the smallest CV is the best model

# AIC - Akaike's Information Criterion - minimising AIC = best model

# AICc - Corrected AIC - we seek to minimise

# BIC - Bayesian Information Criterion - as above

# you then slect your model 2^n, where n = number of predictors
# and run the CV() function on all of them and select with
# the lowest AICc, BIC and CV


###########################################################
################# FORECASTING WITH REGRESSION ####################
###########################################################

# to forecast, we estimate the coefficients and ignore the error

## EX-ANTE
#  forecasts made in advance, using info available at the time
# the model requires forecasts of the predictors

## Ex-POST
# we use actual observations of the predictors

# Two sources of potential error arise here
#   1. Poor forecasts of the predictor
#   2. Poor forecasting model

beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend +season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("Megalitres")

### SCENARIO-BASED FORECASTING ###

fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)

h <- 4

newdata <- data.frame(
  Income = c(1,1,1,1),
  Savings = c(0.5,0.5,0.5,0.5),
  Unemployment = c(0,0,0,0))

fcast.up <- forecast(fit.consBest, newdata = newdata)

newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(0.5, h),
  Unemployment = rep(0, h))

fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[,1]) +
  ylab("% change in US Consumption") +
  autolayer(fcast.up, PI=T, series = "increase") +
  autolayer(fcast.down, PI=T, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))

#####################################################
#### BUILDING A PREDICTIVE REGRESSION MODEL #########
#####################################################

# US Consumption:  y = 0.55 + 0.28x

# if we assume Persona Income will increase by historical mean = 0.72%
# then we forecast consumption to increase by 0.75%

fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
        newdata = data.frame(
          Income = rep(mean(uschange[,"Income"]), h)))
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(
                       Income = rep(5, h)))

autoplot(uschange[,"Consumption"]) +
  ylab("% change in US Consumption") +
  autolayer(fcast.ave, PI=T, series = "Average increase") +
  autolayer(fcast.up, PI=T, series = "Extreme increase") +
  guides(colour = guide_legend(title = "Scenario"))

# this graph presents a historical increase of 0.72% 
# and what happens if there is an extreme increase of 5%


##############################################################
################# NONLINEAR REGRESSION ######################
##############################################################

# for non-linear modelling -  we transform the forecast variable 
# and/or the predictor variable before estimating the model

# log-log = logy = int + coeff. logx + E

# Coeff. = average % change in y from a 1% change in x 

# log-linear: only transform the forecasy variable
# Linear-log: only transform the predictor

# Log-transformation: all x > 0 elif x = 0: log(x + 1)

# Non-Linear: y = f(x) + e
# in Linear Model: f(x) = int + coeff.x

# in non-linear we want to adjust the slope of f
# Piecewise = nonlinear trend constructed out of linear pieces

h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h=0)

t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>% 
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(marathon) +
  autolayer(fitted(fit.lin), series="Linear") +
  autolayer(fitted(fit.exp), series="Exponential") +
  autolayer(fitted(fit.pw), series="Piecewise") +
  autolayer(fitted(fit.spline), series="Cubic Spline") +
  autolayer(fcasts.pw , series="Piecewise") +
  autolayer(fcasts.lin , series="Linear", PI=F) +
  autolayer(fcasts.exp , series="Exponential", PI=F) +
  autolayer(fcasts.spl , series="Cubic Spline", PI=F) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))

# Comment
# Piecewise looks like the best forecast model
# Cubic Spline - fits best, but poor forecasts (overfitting)

# we can adjust the Cubic Splines with constraints, using
# Natural Cubic Smoothing Spline (add linear model at end)

marathon %>% 
  splinef(lambda = 0) %>% # log transformation to handle heteroskedasticity
  autoplot()

# still a wide prediction interval (volatility in the ts)

# Check Residuals
marathon %>% 
  splinef(lambda = 0) %>%
  checkresiduals()













