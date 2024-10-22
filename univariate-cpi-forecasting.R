library(tidyverse)
library(forecast)
library(tseries)
library(fredr)
library(seasonal)

#Loading the Data

fredr_set_key("1d1d3ef23de16acfda3ba1b4c2d29018")
x <- na.omit(fredr(series_id="CPIAUCSL", units="pc1", observation_end = as.Date("2021-06-30")))
cpi <- ts(data = x$value, start = c(1948,1), frequency = 12)
head(cpi)
tail(cpi)

#ACF, PACF, Stationarity, and Seasonality

autoplot(cpi) + ggtitle("Year Over Year Percent Change in US Infaltion") + labs(x = "Time", y = " YOY Percent Change in CPI")
autoplot(mstl(cpi)) + ggtitle("STL Decomposition of YOY Percent Change in US Inflation")
ggAcf(cpi) + ggtitle("YOY Percent Change in US Inflation ACF")
ggPacf(cpi) + ggtitle("YOY Percent Change in US Inflation PACF")

adf.test(cpi)
ndiffs(cpi, test = "adf")
kpss.test(cpi)
ndiffs(cpi, test = "kpss")

autoplot(diff(cpi)) + ggtitle("1st Diff:Year Over Year Percent Change in US Infaltion") + labs(x = "Time", y = "1st Diff:YOY Percent Change")
autoplot(mstl(diff(cpi))) + ggtitle("STL Decomposition of 1st Diff:YOY Percent Change in US Inflation")
ggAcf(diff(cpi)) + ggtitle("1st Diff:YOY Percent Change in US Inflation ACF")
ggPacf(diff(cpi)) + ggtitle("1st Diff:YOY Percent Change in US Inflation PACF")

adf.test(diff(cpi))
kpss.test(diff(cpi))

#SARIMA Model

model <- auto.arima(cpi)
checkresiduals(model)

#In Sample Forecast

training <- subset(cpi, end = length(cpi)-25)
test <- subset(cpi, start = length(cpi)-24)
training.forecast <- forecast(training, model = model, h = 25)
autoplot(training.forecast) + autolayer(test) + xlim(2015,NA)
accuracy(training.forecast, cpi)

#Out of Sample Forecast

out.forecast <- forecast(cpi, model = model, h = 12)
autoplot(out.forecast) + xlim(2015, NA)
out.forecast