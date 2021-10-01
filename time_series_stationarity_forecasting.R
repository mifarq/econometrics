## Time Series Analysis -- Stationarity and Forecasting

# Loading packages 

library(tseries)
library(vars)
library(forecast)

# Simulating a white noise process and performing a Dickey-Fuller test to confirm the simulated series are stationary

TT <- 100
wn <- rnorm(TT)
plot.ts(wn)
adf.test(wn)

### P-value is less than 0.05, so we reject the null hypothesis of non-stationarity.

# Simulating a random walk process and performing a Dickey-Fuller test to confirm the simulated series are non-stationary.

rw <- cumsum(rnorm(TT))
plot.ts(rw)
adf.test(rw)

### P-value is much larger than 0.05, so, so we fail to reject the null hypothesis of non-stationarity.

# Visualizing Air Passengers data

air <- AirPassengers
plot.ts(air)

# Deciding whether the series needs to be made stationary

adf.test(air, k=12)

### P-value of the augmented dickey fuller test is much larger than 0.05, so we fail to reject the null hypothesis of non-stationarity. The series needs to be made stationary. 

# Plotting the ACF/PACF charts and decide on the best model to employ 

air %>% diff(lag=12) %>% ggtsdisplay()
air %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
auto.arima(air, seasonal = TRUE)

### The auto.arima function suggests that the best model to run is an ARIMA(2,1,1) model with 2 lags, 1 difference, and a moving average of order 1. The seasonal part of the model indicates a first difference with a 12-month seasonal lag. 

# (d) Estimating the model

airfit <- auto.arima(air, seasonal = TRUE)
checkresiduals(airfit)

# (e) Using the model to forecast the number of passengers in the next 24 months

airforecast <- airfit %>% forecast(h=24) 
autoplot(airforecast)
airforecast
