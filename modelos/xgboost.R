
library(xgboost)
set.seed(100)

# Gerando valores aleatórios da base para posteriormente separar em treino e teste
linhas <- sample(1:length(data_normalized$ibov), length(data_normalized$ibov)*0.7)

# Dados de treino 70%
train <- data_normalized[linhas,]

# Dados de teste 30%
test <- data_normalized[-linhas,]

train_x = data.matrix(train[, -2])
train_y = train[,2]

test_x = data.matrix(test[, -2])
test_y = test[, 2]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

xgbc = xgboost(data = xgb_train, max.depth = 2, nrounds = 50)
saveRDS(xgbc, 'bases/xgboost/xgbc')
print(xgbc)


pred_y = predict(xgbc, xgb_test)


mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

treino <- train %>% select(date, ibov) %>% nest()
teste <- test %>% select(date, ibov) %>% mutate(predict = pred_y) %>% nest()
model_num <- 1
model_type <- 'XGBOOST'

xgboost_model <- data.frame('tipo_modelo' = model_type, 'numero_modelo' = model_num, 'model_hist' = treino, 'model_fit' = teste, 'mse' = mse, 'mae' = mae, 'rmse' = rmse)
colnames(xgboost_model) <- c('tipo_modelo', 'numero_modelo', 'model_hist', 'model_fit', 'mse', 'mae', 'rmse')

saveRDS(xgboost_model, 'bases/xgboost_model')

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(test_y)
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

