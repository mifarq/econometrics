library(tseries)
library(vars)
library(forecast)
library(fredr)
library(tidyverse)

#Loading the Data

fredr_set_key("1d1d3ef23de16acfda3ba1b4c2d29018")
x <- fredr(series_id="CPIAUCNS", units="pc1", observation_start = as.Date("1964-01-01"), observation_end = as.Date("2021-06-01"))
y <- fredr(series_id="DSPIC96", units="pc1", observation_start = as.Date("1964-01-01"), observation_end = as.Date("2021-06-01")) ## Year over year change in real disposable income
z <- fredr(series_id="PCE", units="pc1", observation_start = as.Date("1964-01-01"), observation_end = as.Date("2021-06-01")) ## Year over year change in Personal Consumption Expenditures (PCE)
z1 <- fredr(series_id="PPIACO", units="pc1", observation_start = as.Date("1964-01-01"), observation_end = as.Date("2021-06-01")) ## Year over year change in PPI
cpi <- ts(x$value,
          start = c(1964, 1),
          end = c(2021, 6),
          frequency = 12)
rdpi <- ts(y$value,
           start = c(1964, 1),
           end = c(2021, 6),
           frequency = 12)
pce <- ts(z$value,
          start = c(1964, 1),
          end = c(2021, 6),
          frequency = 12)
ppi <- ts(z1$value,
          start = c(1964, 1),
          end = c(2021, 6),
          frequency = 12)

#CPI Stationarity, Seasonality, ACF and PACF

autoplot(cpi) + ggtitle("YOY Percent Change in CPI") + labs(x = "Time", y = "YOY Percent Change in CPI")
autoplot(mstl(cpi)) + ggtitle("STL Decomposition of YOY Percent Change in CPI")
ggAcf(cpi) + ggtitle("YOY Percent Change in CPI ACF")
ggPacf(cpi) + ggtitle("YOY Percent Change in CPI PACF")

adf.test(cpi)
ndiffs(cpi, test = "adf")
kpss.test(cpi)
ndiffs(cpi, test = "kpss")

autoplot(diff(cpi)) + ggtitle("1st Diff:YOY Percent Change in CPI") + labs(x = "Time", y = "YOY Percent Change in CPI")
autoplot(mstl(diff(cpi))) + ggtitle("STL Decomposition of 1st Diff:YOY Percent Change in CPI")
ggAcf(diff(cpi)) + ggtitle("1st Diff:YOY Percent Change in CPI ACF")
ggPacf(diff(cpi)) + ggtitle("1st Diff:YOY Percent Change in CPI PACF")

adf.test(diff(cpi))
kpss.test(diff(cpi))

#PCE Stationarity, Seasonality, ACF and PACF

autoplot(pce) + ggtitle("YOY Percent Change in PCE") + labs(x = "Time", y = "YOY Percent Change in PCE")
autoplot(mstl(pce)) + ggtitle("STL Decomposition of YOY Percent Change in PCE")
ggAcf(pce) + ggtitle("YOY Percent Change in PCE ACF")
ggPacf(pce) + ggtitle("YOY Percent Change in PCE PACF")

adf.test(pce)
ndiffs(pce, test = "adf")
kpss.test(pce)
ndiffs(pce, test = "kpss")

autoplot(diff(pce)) + ggtitle("1st Diff:YOY Percent Change in PCE") + labs(x = "Time", y = "YOY Percent Change in PCE")
autoplot(mstl(diff(pce))) + ggtitle("STL Decomposition of 1st Diff:YOY Percent Change in PCE")
ggAcf(diff(pce)) + ggtitle("1st Diff:YOY Percent Change in PCE ACF")
ggPacf(diff(pce)) + ggtitle("1st Diff:YOY Percent Change in PCE PACF")

adf.test(diff(pce))
kpss.test(diff(pce))

#PPI Stationarity, Seasonality, ACF and PACF

autoplot(ppi) + ggtitle("YOY Percent Change in PPI") + labs(x = "Time", y = "YOY Percent Change in PPI")
autoplot(mstl(ppi)) + ggtitle("STL Decomposition of YOY Percent Change in PPI")
ggAcf(ppi) + ggtitle("YOY Percent Change in PPI ACF")
ggPacf(ppi) + ggtitle("YOY Percent Change in PPI PACF")

adf.test(ppi)
ndiffs(ppi, test = "adf")
kpss.test(ppi)
ndiffs(ppi, test = "kpss")

autoplot(diff(ppi)) + ggtitle("1st Diff:YOY Percent Change in PPI") + labs(x = "Time", y = "YOY Percent Change in PPI")
autoplot(mstl(diff(ppi))) + ggtitle("STL Decomposition of 1st Diff:YOY Percent Change in PPI")
ggAcf(diff(ppi)) + ggtitle("1st Diff:YOY Percent Change in PPI ACF")
ggPacf(diff(ppi)) + ggtitle("1st Diff:YOY Percent Change in PPI PACF")

adf.test(diff(ppi))
kpss.test(diff(ppi))

#PPI Stationarity, Seasonality, ACF and PACF

autoplot(rdpi) + ggtitle("YOY Percent Change in RDPI") + labs(x = "Time", y = "YOY Percent Change in RDPI")
autoplot(mstl(rdpi)) + ggtitle("STL Decomposition of YOY Percent Change in RDPI")
ggAcf(rdpi) + ggtitle("YOY Percent Change in RDPI ACF")
ggPacf(rdpi) + ggtitle("YOY Percent Change in RDPI PACF")

adf.test(rdpi)
ndiffs(rdpi, test = "adf")
kpss.test(rdpi)
ndiffs(rdpi, test = "kpss")

autoplot(diff(rdpi)) + ggtitle("1st Diff:YOY Percent Change in RDPI") + labs(x = "Time", y = "YOY Percent Change in RDPI")
autoplot(mstl(diff(rdpi))) + ggtitle("STL Decomposition of 1st Diff:YOY Percent Change in RDPI")
ggAcf(diff(rdpi)) + ggtitle("1st Diff:YOY Percent Change in RDPI ACF")
ggPacf(diff(rdpi)) + ggtitle("1st Diff:YOY Percent Change in RDPI PACF")

adf.test(diff(rdpi))
kpss.test(diff(ppi))

#Combining Datasets

Data <- cbind(cpi, rdpi, pce, ppi)
autoplot(Data) + ggtitle("Year Over Year Change in Inflation, PCE, PPI, and Real Disposable Income") + labs(x = "Time", y = "Percent Change")

#Model Selection and Estimation

VARselect(Data, lag.max = 8, type = "const", season = 12)
model <- VAR(Data, p = 2, season = 12, type = "const")
serial.test(model)

#In Sample Forecasting

training_cpi <- subset(cpi, end = length(cpi)-25)
test_cpi <- subset(cpi, start = length(cpi)-24)
training_pce <- subset(pce, end = length(pce)-25)
test_pce <- subset(pce, start = length(pce)-24)
training_ppi <- subset(ppi, end = length(ppi)-25)
test_ppi <- subset(ppi, start = length(ppi)-24)
training_rdpi <- subset(rdpi, end = length(rdpi)-25)
test_rdpi <- subset(rdpi, start = length(rdpi)-24)

training.Data <- cbind(training_cpi, training_rdpi, training_pce, training_ppi)
test.Data <- cbind(test_cpi, test_pce, test_ppi, test_rdpi)

training.model <- VAR(training.Data, p = 2, season = 12, type = "const")
training.forecast <- forecast(training.model, h = 25)

autoplot(training.forecast$forecast$training_cpi) + autolayer(test_cpi) + xlim(2015,NA)
accuracy(training.forecast$forecast$training_cpi, test_cpi)

#Out of Sample Forecasting

out.forecast <- forecast(model, h = 12)
autoplot(out.forecast$forecast$cpi) + xlim(2015,NA)
out.forecast$forecast$cpi

#1st Diff: Combining Datasets

Data.diff <- cbind(diff(cpi), diff(rdpi), diff(pce), diff(ppi))
autoplot(Data.diff) + ggtitle("1st Diff: Over Year Change in Inflation, PCE, PPI, and Real Disposable Income") + labs(x = "Time", y = "Percent Change")

#1st Diff: Model Selection and Estimation

VARselect(Data.diff, lag.max = 8, type = "none", season = 12)
model.diff <- VAR(Data.diff, p = 2, season = 12, type = "none")
serial.test(model.diff)

#1st Diff: In Sample Forecasting

training_dcpi <- subset(diff(cpi), end = length(diff(cpi))-25)
test_dcpi <- subset(diff(cpi), start = length(diff(cpi))-24)
training_dpce <- subset(diff(pce), end = length(diff(pce))-25)
test_dpce <- subset(diff(pce), start = length(diff(pce))-24)
training_dppi <- subset(diff(ppi), end = length(diff(ppi))-25)
test_dppi <- subset(diff(ppi), start = length(diff(ppi))-24)
training_drdpi <- subset(diff(rdpi), end = length(diff(rdpi))-25)
test_drdpi <- subset(diff(rdpi), start = length(diff(rdpi))-24)

training.dData <- cbind(training_dcpi, training_drdpi, training_dpce, training_dppi)
test.dData <- cbind(test_dcpi, test_dpce, test_dppi, test_drdpi)

training.model.diff <- VAR(training.dData, p = 2, season = 12, type = "none")
training.forecast.diff <- forecast(training.model.diff, h = 25)

autoplot(training.forecast.diff$forecast$training_dcpi) + autolayer(test_dcpi) + xlim(2015,NA)
accuracy(training.forecast$forecast$training_cpi, test_cpi)

#1st Diff: Out of Sample Forecasting

out.forecast.diff <- forecast(model.diff, h = 12)
autoplot(out.forecast.diff$forecast$diff.cpi) + xlim(2015,NA)
out.forecast.diff$forecast$diff.cpi.
tail(cpi)
