# Clear environment 
rm(list=ls(all.names = TRUE))


# Install the fpp2 package with all the datasets
# and specific graphing functions

install.packages("fpp2")
library(fpp2)

# Create a simple ts object
y <- ts(c(123,39,78,52,110), start=2014)


# Autoplot times series
autoplot(a10)+
  ggtitle("Antidiabetic Drug Sales (ADS)")+
  ylab("$ million")+
  xlab("Year")

# Plot seasonal time series
ggseasonplot(AirPassengers, col=rainbow(12), year.labels=TRUE)

ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: Antidiabtes meds")

# Plot seasonal subseries plot
ggsubseriesplot(a10) + ylab("$ million") +
  ggtitle("Seasonal Subseries: ADS meds")

# plot two times series next to each other
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE)+
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half Hourly elec demand")

s <- as.data.frame(elecdemand)

# Create scatterplot
qplot(Temperature, Demand, data = s) + 
  ylab("Demand in GW") + xlab("Temperature (Celsius")

# Considering multiple variables
autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter")

#To explore the relationships between multiple variables
# we call the ggpairs function (after installing GGAlly package)
install.packages("GGally")
library(GGally)
GGally::ggpairs(as.data.frame(visnights[,1:5]))

# Lagplots
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

# in the lagplots, the colours indicate the
# quarter of the variable on the y-axis

# Autocorrelation Coefficent Function
ggAcf(beer2) # notice the seasonality
autoplot(beer2) # check seasonality
# the dashed lines = 95% confidence interval
# so therefore significantly different from zero

#Trend and seasonality
aelec <- window(elec, start=1980)
plot(aelec) + xlab("Year") + yab("GWh")
ggAcf(aelec,lag=48)
# the slow decay on the ACF lags is due to the trend,
# while the dips are seasonality

#White NOise
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White Noise")
ggAcf(y)
# because our ACF are within 95% confidence interval
# (+- 2/ T^0.5), then we confirm it is white noise

head(dj)
a <- as.data.frame(dj)
library(dplyr)
getwd()
write.csv(a, "dj.csv")

