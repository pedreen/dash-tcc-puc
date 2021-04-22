
####### Random Forest
library(randomForest)

# Setando o número randômico 
set.seed(100)

# Gerando valores aleatórios da base para posteriormente separar em treino e teste
linhas <- sample(1:length(data_normalized$ibov), length(data_normalized$ibov)*0.7)

# Dados de treino 70%
train <- data_normalized[linhas,]

# Dados de teste 30%
test <- data_normalized[-linhas,]

forest <- randomForest(ibov ~ ., data = test[-1], localImp = T, mtry = 6, do.trace=T, importance = T, ntree=500)
forest2 <- randomForest(ibov ~ cambio + risco + selic, data = test[-1], localImp = T, mtry = 6, do.trace=T, importance = T, ntree=500)
forest3 <- randomForest(ibov ~ selic + vix + risco + cambio, data = test[-1], localImp = T, mtry = 3, do.trace=T, importance = T, ntree=500)



# cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>%
    mutate(predict = forest$predicted) %>% 
    select(date, predict) %>% nest()
model_num <- 1
model_type <- 'Random Forest'
mse_rf = mean((test$ibov - forest$predicted[[1]])^2)
mae_rf = caret::MAE(test$ibov, forest$predicted[[1]])
rmse_rf = caret::RMSE(test$ibov, forest$predicted[[1]])

modelo1 <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino, 'model_fit' = teste, 'mse' = mse_rf, 'mae' = mae_rf, 'rmse' = rmse_rf) 

treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>%
    mutate(predict = forest2$predicted) %>% 
    select(date, predict) %>% nest()
model_num <- 2
model_type <- 'Random Forest'
mse_rf = mean((test$ibov - forest2$predicted[[1]])^2)
mae_rf = caret::MAE(test$ibov, forest2$predicted[[1]])
rmse_rf = caret::RMSE(test$ibov, forest2$predicted[[1]])

modelo2 <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino, 'model_fit' = teste, 'mse' = mse_rf, 'mae' = mae_rf, 'rmse' = rmse_rf) 

treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>%
    mutate(predict = forest3$predicted) %>% 
    select(date, predict) %>% nest()
model_num <- 3
model_type <- 'Random Forest'
mse_rf = mean((test$ibov - forest3$predicted[[1]])^2)
mae_rf = caret::MAE(test$ibov, forest3$predicted[[1]])
rmse_rf = caret::RMSE(test$ibov, forest3$predicted[[1]])

modelo3 <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino, 'model_fit' = teste, 'mse' = mse_rf, 'mae' = mae_rf, 'rmse' = rmse_rf) 

forecast_rf <- bind_rows(modelo1, modelo2, modelo3)
colnames(forecast_rf) <- c('tipo_modelo', 'numero_modelo', 'model_hist', 'model_fit', 'mse', 'mae', 'rmse')

saveRDS(forecast_rf, 'bases/arvore/forecast_rf')


library(randomForestExplainer)

# min_depth_distribution
min_depth_frame <- min_depth_distribution(forest)
min_depth_frame2 <- min_depth_distribution(forest2)
min_depth_frame3 <- min_depth_distribution(forest3)
saveRDS(min_depth_frame, 'www/min_depth_frame1')
saveRDS(min_depth_frame2, 'www/min_depth_frame2')
saveRDS(min_depth_frame3, 'www/min_depth_frame3')

# explicando o modelo
explain_forest(forest, interactions = TRUE, data = data_normalized)

plot_min_depth_distribution(min_depth_frame)


importance_frame <- measure_importance(forest)
plot_multi_way_importance(importance_frame, size_measure = 'no_of_nodes')
saveRDS(importance_frame, 'www/importance_frame')

importance_frame2 <- measure_importance(forest2)
plot_multi_way_importance(importance_frame, size_measure = 'no_of_nodes')
saveRDS(importance_frame2, 'www/importance_frame2')


importance_frame3 <- measure_importance(forest3)
plot_multi_way_importance(importance_frame, size_measure = 'no_of_nodes')
saveRDS(importance_frame3, 'www/importance_frame3')



plot_multi_way_importance(importance_frame, x_measure = 'mse_increase' , y_measure = 'node_purity_increase', size_measure = 'p_value',
                          no_of_labels = 8)

plot_importance_ggpairs(importance_frame)


plot_predict_interaction(forest, data_normalized, 'selic', 'ibov')


data_predict <- data_normalized
# %>% 
#     mutate(predict_forest = forest[["predicted"]])




model_num <- 1

plot_ly(type = 'scatter', mode = 'lines') %>% 
    
    # add_trace(
    #     x = data_predict$date, y = data_predict$ibov,
    #     name = 'Hist.',
    #     line = list(color = "rgb(47, 73, 139)"),
    #     hoverinfo = 'text',
    #     text = ~paste("<b>Modelo:</b>", model_num,
    #                   "\n<b>Data:</b>", data_predict$date,
    #                   "\n<b>Ibovespa (hist.):</b>", paste(formatC(data_predict$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
    # ) %>%
    
    
    
    add_trace(x = test$date,y = forest[["predicted"]],
        name = 'Fit',
        line = list(color = "rgb(250, 0, 0)", dash = 'dot'), 
        hoverinfo = 'text',
        text = ~paste("<b>Modelo:</b>", model_num,
                      "\n<b>Data:</b>", test$date,
                      "\n<b>Ibovespa (fit):</b>", paste(formatC(forest[["predicted"]], digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
        
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



