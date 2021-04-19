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





####### Random Forest
library(randomForest)

set.seed(100)
forest <- randomForest(ibov ~ ., data = data_normalized, localImp = T)

library(randomForestExplainer)

# min_depth_distribution
min_depth_frame <- min_depth_distribution(forest)
plot_min_depth_distribution(min_depth_frame)


importance_frame <- measure_importance(forest)
plot_multi_way_importance(importance_frame, size_measure = 'no_of_nodes')


plot_multi_way_importance(importance_frame, x_measure = 'mse_increase' , y_measure = 'node_purity_increase', size_measure = 'p_value',
                          no_of_labels = 8)

plot_importance_ggpairs(importance_frame)


plot_predict_interaction(forest, data_normalized, 'selic', 'ibov')


data_predict <- data_normalized %>% 
    mutate(prdict_forest = forest[["predicted"]])




model_num <- 1

plot_ly(type = 'scatter', mode = 'lines') %>% 
    
    add_trace(
        x = data_predict$date, y = data_predict$ibov,
        name = 'Hist.',
        line = list(color = "rgb(47, 73, 139)"),
        hoverinfo = 'text',
        text = ~paste("<b>Modelo:</b>", model_num,
                      "\n<b>Data:</b>", data_predict$date,
                      "\n<b>Ibovespa (hist.):</b>", paste(formatC(data_predict$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
    ) %>%
    
    
    
    add_trace(
        x = data_predict$date, y = data_predict$prdict_forest,
        name = 'Fit',
        line = list(color = "rgb(250, 0, 0)", dash = 'dot'), 
        hoverinfo = 'text',
        text = ~paste("<b>Modelo:</b>", model_num,
                      "\n<b>Data:</b>", data_predict$date,
                      "\n<b>Ibovespa (fit):</b>", paste(formatC(data_predict$prdict_forest, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
        
    ) %>% 
    layout(
        title = '<b>Ibovespa (Hist. vs Fit) </b>',
        legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
        autosize = T, 
        xaxis = list(title = '', showgrid = FALSE),
        yaxis = list(title = '')
    ) 



library(caret)
library(randomForest)
# Setando o número randômico 
set.seed(100)

data_normalized_rf <- data_normalized
rownames(data_normalized_rf) <- data_normalized_rf$date
data_normalized_rf <- data_normalized_rf[-1]


# Gerando valores aleatórios da base para posteriormente separar em treino e teste
linhas <- createDataPartition(data_normalized_rf$ibov, p = 0.7, times = 1, list = F)

# Dados de treino 70%
train <- data_normalized_rf[linhas,]

# Dados de teste 30%
test <- data_normalized_rf[-linhas,]

prop.table(table(train$dummy_date))

rf <- randomForest(x = train[,-2],
                   y = train$ibov,
                   xtest = train[,-2],
                   ytest = train$ibov,
                   ntree = 1000,
                   mtry = 6,
                   replace = T,
                   nodesize = 10,
                   maxnodes = 15,
                   keep.forest = T)

varImpPlot(rf)

rf

plot(rf)



library(randomForestExplainer)

# min_depth_distribution
min_depth_frame <- min_depth_distribution(rf)
plot_min_depth_distribution(min_depth_frame)


importance_frame <- measure_importance(rf)
plot_multi_way_importance(importance_frame, size_measure = 'no_of_nodes')


plot_multi_way_importance(importance_frame, x_measure = 'mse_increase' , y_measure = 'node_purity_increase', size_measure = 'p_value',
                          no_of_labels = 8)

plot_importance_ggpairs(importance_frame)


plot_predict_interaction(rf, data_normalized, 'selic', 'ibov')


data_predict <- data_normalized %>% 
    mutate(prdict_forest = rf[["predicted"]])




model_num <- 1

plot_ly(type = 'scatter', mode = 'lines') %>% 
    
    add_trace(
        x = data_predict$date, y = data_predict$ibov,
        name = 'Hist.',
        line = list(color = "rgb(47, 73, 139)"),
        hoverinfo = 'text',
        text = ~paste("<b>Modelo:</b>", model_num,
                      "\n<b>Data:</b>", data_predict$date,
                      "\n<b>Ibovespa (hist.):</b>", paste(formatC(data_predict$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
    ) %>%
    
    
    
    add_trace(
        x = data_predict$date, y = data_predict$prdict_forest,
        name = 'Fit',
        line = list(color = "rgb(250, 0, 0)", dash = 'dot'), 
        hoverinfo = 'text',
        text = ~paste("<b>Modelo:</b>", model_num,
                      "\n<b>Data:</b>", data_predict$date,
                      "\n<b>Ibovespa (fit):</b>", paste(formatC(data_predict$prdict_forest, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
        
    ) %>% 
    layout(
        title = '<b>Ibovespa (Hist. vs Fit) </b>',
        legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
        autosize = T, 
        xaxis = list(title = '', showgrid = FALSE),
        yaxis = list(title = '')
    ) 



