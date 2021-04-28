##### Visualizando os dados

# plot_ly(data_orig, x = ~date, y = ~ibov, type = 'scatter', mode = 'line', name = 'Ibovespa') %>% 
#     add_trace(y = ~cambio, name = 'Câmbio')

data_orig_normalizada <- readRDS('www/data_normalized')

correlacao <- data_orig_normalizada %>% select(-date, -dummy_date)
correlacao <- cor(correlacao)

corrplot(correlacao, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

ggcorrplot(correlacao, method = 'circle', 
           lab = T, lab_size = 3)


# Setando o número randômico 
set.seed(100)

# Gerando valores aleatórios da base para posteriormente separar em treino e teste
linhas <- sample(1:length(data_normalized$ibov), length(data_normalized$ibov)*0.7)

# Dados de treino 70%
train <- data_normalized[linhas,]

# Dados de teste 30%
test <- data_normalized[-linhas,]

# Arvode de Decisão

library(rpart)
modelo <- rpart(ibov ~ .,data = train, control = rpart.control(cp=0))
modelo2 <- rpart(ibov ~ cambio + risco + selic,data = train, control = rpart.control(cp=0))
modelo3 <- rpart(ibov ~ selic + vix + risco + cambio,data = train, control = rpart.control(cp=0))

# Realizando previsões
test$predict <- stats::predict(modelo, test)

# Analisando resultados
test$ibov_predict <- abs(round(test$predict/test$ibov, 4) -1)
erros_predict <- summary(test$ibov_predict) 
erros_predict

# Model 1
treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>% select(date, predict) %>% nest()
model_num <- 1
model_type <- 'Árvore de Decisão'
error_model_1 <- erros_predict
saveRDS(error_model_1, 'bases/arvore/error_model_1')
mse_tree = mean((test$ibov - test$ibov_predict)^2)
mae_tree = caret::MAE(test$ibov, test$ibov_predict)
rmse_tree = caret::RMSE(test$ibov, test$ibov_predict)

modelo1 <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino,
                      'model_fit' = teste, 'mse' = mse_tree, 'mae' = mae_tree, 'rmse' = rmse_tree) 

# Model 2

# Realizando previsões
test$predict <- stats::predict(modelo2, test)

# Analisando resultados
test$ibov_predict <- abs(round(test$predict/test$ibov, 4) -1)
erros_predict <- summary(test$ibov_predict) 
erros_predict


treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>% select(date, predict) %>% nest()
model_num <- 2
model_type <- 'Árvore de Decisão'
error_model_2 <- erros_predict
saveRDS(error_model_2, 'bases/arvore/error_model_2')
mse_tree = mean((test$ibov - test$ibov_predict)^2)
mae_tree = caret::MAE(test$ibov, test$ibov_predict)
rmse_tree = caret::RMSE(test$ibov, test$ibov_predict)

modelo2 <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino,
                      'model_fit' = teste, 'mse' = mse_tree, 'mae' = mae_tree, 'rmse' = rmse_tree) 

# Model 3
# Realizando previsões
test$predict <- stats::predict(modelo3, test)

# Analisando resultados
test$ibov_predict <- abs(round(test$predict/test$ibov, 4) -1)
erros_predict <- summary(test$ibov_predict) 
erros_predict

treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>% select(date, predict) %>% nest()
model_num <- 3
model_type <- 'Árvore de Decisão'
error_model_3 <- erros_predict
saveRDS(error_model_3, 'bases/arvore/error_model_3')
mse_tree = mean((test$ibov - test$ibov_predict)^2)
mae_tree = caret::MAE(test$ibov, test$ibov_predict)
rmse_tree = caret::RMSE(test$ibov, test$ibov_predict)

modelo3 <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino,
                      'model_fit' = teste, 'mse' = mse_tree, 'mae' = mae_tree, 'rmse' = rmse_tree) 

forecast_tree <- bind_rows(modelo1, modelo2, modelo3)
colnames(forecast_tree) <- c('tipo_modelo', 'numero_modelo', 'model_hist', 'model_fit', 'mse', 'mae', 'rmse')

saveRDS(forecast_tree, 'bases/arvore/forecast_tree')

