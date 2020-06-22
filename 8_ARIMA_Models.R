## Exponential Smoothing & ARIMA Models used for forecasting

## Exp Smoothing - fit the data (trend and seasonality)
## ARIMA - autocorrelation

rm(list=ls())  # clear environment

install.packages('fpp2')
library(fpp2)


plot(goog200)

plot(diff(goog200))


Box.test(goog200, lag=10, type = "Ljung-Box")

library(tseries)
adf.test(goog200)


Box.test(diff(goog200), lag=10, type = "Ljung-Box")
# p-value = 0.3551 on differenced dataset 
# daily change is not correlated with previous observation

adf.test(diff(goog200))


## Example of secondary differencing - transforming to log scale, then seasonal differencing
cbind("Sales ($mil)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>% 
        autoplot(facets=TRUE)+
          xlab("Year")+ylab(" ")+
          ggtitle("Antidiabetic drug sales")


## Second Example
cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" = diff(log(usmelec),12),
        "Doubly\n differenced logs" = diff(diff(log(usmelec),12),1)) %>% 
        autoplot(facets=TRUE) + 
        xlab("Year")+ylab(" ")+
        ggtitle("Monthly US Elec Generation")



## Unit Root Test ##
library(urca)
goog %>% ur.kpss() %>% summary()
# the test stat is much larger than the 1% critical stat
# therefore in the zone of rejection - Data NOT stationary

adf.test(goog)


## now run the test on the differenced data
goog %>% diff() %>% ur.kpss() %>% summary()

# test stat smaller than crit stat - therefore p-value > alpha
# can't reject null

adf.test(diff(goog))

## How many differences do we need to do?
plot(diff(goog))

ndiffs(goog)

##Seasonal differencing required?
usmelec %>% log() %>% nsdiffs()

####################################################
############### ARIMA ##############################
####################################################

autoplot(uschange[,"Consumption"]) + 
  xlab("Year") + ylab("Quarterly % change")


ndiffs(uschange)


fit <- auto.arima(uschange[,"Consumption"], seasonal=F, stepwise = F,
                  approximation = F, trace = T)

fit
# c = 0.745 x (1 - 0.589) = 0.307
# e = white noise with std dev = sqrt(0.3499)


fit %>% forecast(h=10) %>% autoplot(include=80)

autoplot(forecast(fit, h=10), xlim = c(2010, 2020))

checkresiduals(fit)

######## ACF and PACF to select ARIMA(p,d,q) ########

ggAcf(uschange[,"Consumption"])

ggPacf(uschange[,"Consumption"])

(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))

# Compare the AICc with the auto.arima() model

# To improve the auto.arima() model paramters 
(fit3 <- auto.arima(uschange[,"Consumption"], seasonal = FALSE,
                    stepwise=FALSE, approximation = F, trace = T))

autoplot(forecast(fit3), xlim = c(2010, 2018))

#####################################################################
#### worked example - seasonally adjusted electrical equipment orders

elecequip %>% stl(s.window = 'periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

# Some sudden changes, nothing stands out
# no evidence of changing variance, so no Box-Cox transformation
# Data NON-stationary - series moves up and down for long periods
# therefore data must be differenced

eeadj %>% diff() %>% ggtsdisplay(main="")
ggAcf(diff(eeadj))
ggPacf(diff(eeadj))

# this looks like a AR(3) model so first candidate ARIMA(3,1,0)

# trial and error fit (4,1,0), (2,1,0), (3,1,1) etc. and find smallest AICc

(fit <- Arima(eeadj, order=c(4,1,0)))

fit4 <- auto.arima(eeadj, trace = T)

checkresiduals(fit4)
# Partmanteau test - large p-value so residials: white noise

autoplot((forecast(fit)))

autoplot((forecast(fit)), xlim = c(2010,2014))

####################################################
######### SEASONAL ARIMA Example ###################

autoplot(euretail) + ylab("Retail Index") + xlab("Year")

# Clearly non-stationary - use diff()


# Take a seasonal differencing
euretail %>% diff(lag=4) %>% ggtsdisplay()

# Additional First differencing
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()

#Select a model
euretail %>% Arima(order = c(0,1,1), seasonal=c(0,1,1)) %>% 
  residuals() %>% ggtsdisplay()
# We iterate with different model orders to get AICc down

fit5 <- auto.arima(euretail, trace = T)
fit5

fit3 <- Arima(euretail, order=c(0,1,3), seasonal = c(0,1,1))
fit3
checkresiduals(fit3)
# all spikes are within the significance limits so the 
# residuals appear to be white noise
# Ljung-Box Test -- high p-value: residuals white noise


# We now have a seasonal ARIMA that passes the required checks
# and is ready for forecasting

fit3 %>% forecast(h=12) %>% autoplot()


####################################################
######### SEASONAL ARIMA Example ###################

### Using auto.arima() ###

auto.arima(euretail)

###################################################
# Example - Forecast MOnthly Corticosteroid Sales #

lh02 <- log(h02)
cbind("HO2 Sales (million scripts)" = h02,
      "Log HO2 Sales"=lh02) %>% 
  autoplot(facets=TRUE) + xlab("Year")+ylab("")

# log to stabilze the variance
# data strongly seasonal - diff(lag=12)

lh02 %>% diff(lag=12) %>% 
  ggtsdisplay(xlab="Year",
              main="Seasonally Differenced H02 Scripts")

fit5 <- lh02 %>% diff(lag=12) %>% auto.arima(stepwise = FALSE)

(fit <- Arima(h02, order = c(3,0,1), seasonal = c(0,1,2),
              lambda = 0))
checkresiduals(fit, lag=36)

checkresiduals(fit5, lag=36)

h02 %>% 
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda = 0) %>% 
  forecast() %>% 
  autoplot()+
  ylab("HO2 sales (million scripts)") + xlab("Year")

### Time Series Cross Validation of ARIMA vs ETS ###

fets <- function(x, h){
  forecast(ets(x), h = h)}

farima <- function(x, h){
  forecast(auto.arima(x), h=h)}

# returned objects are then passed into tsCV()

air = window(ausair, start=1990)

# Compute CV errors for ETS
e1 <- tsCV(air, fets, h=1)

# Compute CV erros for ARIMA as e2
e2 <- tsCV(air, farima, h=1)

# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm = T)

air %>% ets() %>% forecast() %>% autoplot()

## Making the Comparison on Seasonal Data ####

cement <- window(qcement, start=1988)

# Use 20 years of data for training
train <- window(cement, end=c(2007,4))

(fit.arima <- auto.arima(train))
checkresiduals(fit.arima)

(fit.ets <- ets(train))
checkresiduals(fit.ets)

# Generate forecast and compare accuracy over test set

a1 <- fit.arima %>% forecast(h= 4*(2013-2007)+1) %>% 
  accuracy(qcement)
a1[,c("RMSE","MAE","MAPE","MASE")]

a2 <- fit.ets %>% forecast(h= 4*(2013-2007)+1) %>% 
  accuracy(qcement)
a2[,c("RMSE","MAE","MAPE","MASE")]

# Generate forecast for next three years
cement %>% ets() %>% forecast(h=12) %>% autoplot()



















