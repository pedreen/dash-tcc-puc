##### ARIMA

library(forecast)
library(lmtest)

data_arima <- data_normalized[-9] %>% 
    mutate(date = as.Date(date))

library(tsbox)
a <- ts_ts(ts_long(data_arima))

data_arima <- ts(data_arima,
                 frequency = 365,
                 start = year(data_normalized$date)) 

ts.plot(data_arima)

seasonplot(a,
           col = rainbow(12),
           year.labels = T,
           type = 'o',
           pch = 16)

ggtsdisplay(a)


# Plot multivariate ts
plot(data_arima[,1:7])

arima_fit = auto.arima(data_arima[,2])

# Forecast for the next 10 time units
arima_forecast = forecast(arima_fit, h = 365)

# Plot forecasts
plot(arima_forecast)

lines(fitted(arima_fit),col="blue")




