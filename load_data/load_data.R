
# Carregando indicadores
# files.sources_y <-  list.files('www/indicadores/', full.names = TRUE)
# sapply(c(files.sources_y), source)
data_normalized <- readRDS('www/data_normalized')

data_coleta <- last(data_normalized$date)

# forecast tree
forecast_tree <- readRDS('bases/arvore/forecast_tree')

# forecast random forest
forecast_rf <- readRDS('bases/randomForest/forecast_rf')

model1 <- readRDS('bases/randomForest/model_rf1')
model2 <- readRDS('bases/randomForest/model_rf2')
model3 <- readRDS('bases/randomForest/model_rf3')

# Disp data rf
disp_model1 <- readRDS('www/min_depth_frame1')
disp_model2 <- readRDS('www/min_depth_frame2')
disp_model3 <- readRDS('www/min_depth_frame3')

imp_mode1 <- readRDS('www/importance_frame')
imp_mode2 <- readRDS('www/importance_frame2')
imp_mode3 <- readRDS('www/importance_frame3')

erro_tree1 <- readRDS('bases/arvore/error_model_1')
erro_tree2 <- readRDS('bases/arvore/error_model_2')
erro_tree3 <- readRDS('bases/arvore/error_model_3')

# XGBOOST
forecast_xgboost <- readRDS('bases/xgboost/xgboost_model')
model_xgboost <- readRDS('bases/xgboost/xgbc')

indicadores_list <- c('Ibovespa' = "ibov", 'Taxa de Câmbio' = "cambio", 'Vix' = "vix", 'Ouro' = "gold_usd", 'Taxa Selic' = "selic", 'Risco País' = "risco", 'Bitcoin' = "bitcoin_usd")

# Carregando módulos
files.sources_modules <-  list.files('modulos/', full.names = TRUE)
sapply(c(files.sources_modules), source)

# Lista de Y's
lista_y <- readRDS('www/lista_indicadores')
