
# Carregando indicadores
# files.sources_y <-  list.files('www/indicadores/', full.names = TRUE)
# sapply(c(files.sources_y), source)
data_normalized <- readRDS('www/data_normalized')

indicadores_list <- c('Ibovespa' = "ibov", 'Taxa de Câmbio' = "cambio", 'Vix' = "vix", 'Ouro' = "gold_usd", 'Taxa Selic' = "selic", 'Risco País' = "risco", 'Bitcoin' = "bitcoin_usd")

# Carregando módulos
files.sources_modules <-  list.files('modulos/', full.names = TRUE)
sapply(c(files.sources_modules), source)

# Lista de Y's
lista_y <- readRDS('www/lista_indicadores')
