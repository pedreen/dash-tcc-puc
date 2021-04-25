library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)



library(xts)

set.seed(100)

# Gerando valores aleatórios da base para posteriormente separar em treino e teste
linhas <- sample(1:length(data_normalized$ibov), length(data_normalized$ibov)*0.7)

# Dados de treino 70%
train <- data_normalized[linhas,]

# Dados de teste 30%
test <- data_normalized[-linhas,]



dat_ts <- xts(train[,-1], order.by=as.Date(train[,1], "%m/%d/%Y"))

mape <- function(actual,pred){
    mape <- mean(abs((actual - pred)/actual))*100
    return (mape)
}


arima_model <- auto.arima(dat_ts$ibov)
summary(arima_model)

fore_arima = forecast::forecast(arima_model, h=nrow(test))
df_arima = as.data.frame(fore_arima)
test$arima = df_arima$`Point Forecast`
mape(test$ibov, test$arima)  


# Projeção 
forec = forecast(fore_arima)
# Impressão de gráfico com série temporal e projeção com intervalos de confiança
plot(forec)
# Incluir no gráfico os valores do modelo para dados observados em azul
lines(fitted(fore_arima),col="blue")


plot.ts(fore_arima$residuals)

myforecast <- forecast(fore_arima,  h=10*12)

plot(myforecast)

Box.test(fore_arima$resid, lag=5, type="Ljung-Box")
Box.test(fore_arima$resid, lag=10, type="Ljung-Box")
Box.test(fore_arima$resid, lag=15, type="Ljung-Box")

checkresiduals(forec)
autoplot(forecast(fore_arima))
