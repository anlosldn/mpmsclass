install.packages("neuralnet")
library(neuralnet)


# select directory in windows, copy PATH, then run before pasting...(takes care of Windows backslash)
x <- gsub("\\\\", "/",readClipboard())
setwd(x) #set working directory 
getwd()


data <- Quandl("CHRIS/CME_C1", api_key="nQW1ynmsFtb42YM2sf4Z")

data = na.approx(data)

ts <- xts(data$Last, as.Date(data$Date, format = "%d/%m/%Y"))
plot(ts)

class(ts)

ts = ts["2018-05-18/2020-05-18"]

training_data <- window(ts, start = "2018-05-01", end = "2020-05-01")
test_data <- window(ts, start = "2020-05-01", end = "2020-05-18")


class(training_data)  
class(test_data)  # ts[index==1,] 
# test_data <- ts[index==2,]

tail(training_data)
tail(test_data)
training_data = na.approx(training_data)
test_data = na.approx(test_data)
autoplot(training_data, main="Near Dated Corn Futures Prices - Last 3 Years")


ndiffs(training_data)

(fit.arima <- auto.arima(training_data, seasonal=F, stepwise = F,
                  approximation = F, trace = T))

(fit2 <- arima(training_data, order = c(2,1,2)))

#(fit.arima <- auto.arima(train))
checkresiduals(fit.arima)
checkresiduals(fit2)

# Generate forecast and compare accuracy over test set

a1 <- fit.arima %>% forecast(h= 90) %>% 
  accuracy(test_data)
a1[,c("RMSE","MAE","MAPE","MASE")]

a2 <- fit2 %>% forecast(h= 90) %>% 
  accuracy(test_data)
a2[,c("RMSE","MAE","MAPE","MASE")]

# Generate forecast for next two weeks
training_data %>% auto.arima() %>% forecast(h=12) %>% autoplot()



     