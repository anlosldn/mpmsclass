library(fpp2)

###### TIME SERIES DECOMPOSITION ##########

#   TREND, SEASONALITY & CYCLES     #

# y = S + T + R   - additive decomposition
# y = S X T X R   - multiplicative

# S = Season, T = Trend and Cycle & R = Remainder

# when trend-cycle (T) appears proportional to level of time series
# then use the multiplicative (usually economic ts data)

# y = S X T X R == logy = logS + logT + logR

# it is important to remove the seasonality esp. in economic ts data
# Seasonally adj series contains T + R

############ CLASSICAL DECOMPOSITION ##############

###### Moving Average Smoothing #######

# removes some of the randomness in the data

autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("annual electricity sales")

ma(elecsales, 5)

autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, 5), series = "5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("annual electricity sales") +
  scale_color_manual(values = c("Data"="grey50","5-MA"="red"),
                     breaks = c("Data","5-MA"))


###### Moving Averages of Moving Averages #######

beer2 <- window(ausbeer, start=1992)
ma4 <- ma(beer2, order = 4, centre = F)
ma2x4 <- ma(beer2, order = 4, centre = F)

# ma4 = average of last 4 observations
# ma2x4 = average of last 2 ma4

# We use ma() to estimate the trend cycles in seasonal data

autoplot(elecequip, series = "Data") +
  autolayer(ma(elecequip, 12), series = "12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manfacturing") + 
  scale_color_manual(values = c("Data"="grey", "12-MA"="red"),
                     breaks=c("Data","12-MA"))

## Multiplicative Decomposition
elecequip %>%  decompose(type="multiplicative") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")

########### 11 Decomposition ##############

install.packages("seasonal")
library(seasonal)

elecequip %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of elec equip index")

autoplot(elecequip, series = "Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New Orders") +
  ggtitle("Electrical equipment manufacturing") +
  scale_colour_manual(values=c("black","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

########### SEATS Decomposition #############

# SEATS - "Seasonal Extraction in ARIMA Time Series

library(seasonal)
elecequip %>% seas() %>% autoplot() +
  ggtitle("SEATS decomposition of elec equipment index")

########### STL Decomposition ################

# STL - Seasonal and Trend Decomposition using Loess

elecequip %>% 
  stl(t.window = 13, s.window = "periodic", robust=T) %>%
  autoplot()

# t.window - trend-cycle window
# s.window - seasonal window

##################################################
#### Forecasting with decomposition #############
#################################################

# to forecast a decomposed time series, we forecast the seasonal
# component S and the seasonally adjusted component A seperately

fit <- stl(elecequip, t.window = 13, s.window = "periodic",
           robust=T)
fit %>% seasadj() %>% naive() %>% 
  autoplot() + ylab("New Orders Index") +
  ggtitle("Niave Forecasts of Seasonally Adjusted Data")

autoplot(forecast(fit, method = "naive")) +
  ylab("New Orders Index")





  






















