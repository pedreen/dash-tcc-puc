
# Carregando indicadores
# files.sources_y <-  list.files('www/indicadores/', full.names = TRUE)
# sapply(c(files.sources_y), source)
data_normalized <- readRDS('www/data_normalized')

# Carregando módulos
files.sources_modules <-  list.files('modulos/', full.names = TRUE)
sapply(c(files.sources_modules), source)

# Lista de Y's
lista_y <- readRDS('www/lista_indicadores')
