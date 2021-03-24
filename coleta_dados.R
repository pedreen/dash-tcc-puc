############### Coleta de dados financeiros ############### 


library(BETS)
library(quantmod)
library(Quandl)
library(tidyverse)
library(ecoseries)   # Para instalar essa lib é preciso seguir esses passos: require(devtools), install_github('cran/ecoseries')
library(ggcorrplot)
library(timetk)
library(scales)
library(plotly)
library(janitor)
library(dplyr)
library(corrplot)
library(xts)
library(zoo)

#### Coletando dados do Yahoo Finance

# Bovespa
getSymbols('^BVSP')
ibov_series <- tk_tbl(BVSP, preserve_index = T, rename_index = 'date')

# plot_ly(ibov_series, x = ~date, y = ~BVSP.Adjusted, type = 'scatter', mode = 'line') %>% 
#     layout(
#         title = "Índice BOVESPA - SP Ajustado",
#         xaxis = list(title = "Data" ,showgrid = FALSE ),
#         yaxis = list(title = "BVSP Ajustado")
#     )

# Taxa de câmbio
getSymbols('BRL=X', src = 'yahoo')
cambio_yahoo = tk_tbl(`BRL=X`, preserve_index = T, rename_index = 'date') 


#### Coleta de dados direto do FRED (possui sérios macro e micro economicas globais) 'https://fred.stlouisfed.org'
getSymbols('VIXCLS', src = 'FRED') # Índice de Volatilidade (Índice do medo)
VIXCLS <- tk_tbl(VIXCLS, preserve_index = T, rename_index = 'date')

#### 2008 foi a crise causada pela crise imobiliária dos EUA

# plot_ly(VIXCLS, x = ~date, y = ~VIXCLS, type = 'scatter', mode = 'line') %>% 
#     layout(
#         title = "Índice de Volatilidade - FRED",
#         xaxis = list(title = "Data" ,showgrid = FALSE ),
#         yaxis = list(title = "Índice de Volatilidade (%)")
#     )


#### Pegando dados do site https://quandl.com
api_key = '9txRuYjAikVs9s8wTX2n' ## Foi necessário criar uma conta para usar os dados do site como estudante e usar a API para o site não te bloquear
# devido a várias requisições que possam ser feitas
Quandl.api_key(api_key)

# Pegando dados do Ouro
gold = Quandl('LBMA/GOLD', type = 'xts', order = 'asc')
gold <- tk_tbl(gold, preserve_index = T, rename_index = 'date')

# Pegando dados do Bitcoin
bitcoin_usd <- Quandl('BITFINEX/BTCUSD',  type ='xts', order = 'asc')
bitcoin_usd <- tk_tbl(bitcoin_usd, preserve_index = T, rename_index = 'date')
# Cambio via Quandl
cambio_quandl <- Quandl('BCB/1', order = 'asc')

## Pegando dados do Banco Central do Brasil https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries

# O número 1 representa o código do Câmbio no site do Banco Central
cambio_bcb <- BETSget(1, from = '1999-06-01')

# O número 1178 representa o código da taxa Selic no site do Banco Central
selic_bcb <- BETSget(1178, from = '1999-06-01')

## Pegando dados do IPEADATA

## Para pega a série usando esse pacote é um pouco mais complicado, é necessário entrar no site http://www.ipeadata.gov.br/Default.aspx
# e quando selecionar o índice EMBI utiliza-se o inspect do navegador pra pegar o número do índice de EMBI no HTML da página

risco_pais <- series_ipeadata(40940, periodicity = 'D')$serie_40940 # Importa como uma lista, precisa-se tratar o dado
colnames(risco_pais) <- c('date', 'risco')


### Pegando a inflação do dolar
inflacao_dolar <- Quandl('RATEINF/INFLATION_USA',  type ='xts', order = 'asc')
inflacao_dolar <- tk_tbl(inflacao_dolar, preserve_index = T, rename_index = 'date')
colnames(inflacao_dolar) <- c('date', 'inflation_yoy_usa')


# # pegando o PIB per capta do brazil
# pib_br <-  Quandl('BCB/14640',  type ='xts', order = 'asc')
# pib_br <- tk_tbl(pib_br, preserve_index = T, rename_index = 'date')
# ##### Juntando séries em um único Data Frame

data_orig <- inner_join(ibov_series, cambio_yahoo, by = 'date') %>% 
    inner_join(VIXCLS, by = 'date') %>% 
    inner_join(gold, by = 'date') %>% 
    inner_join(bitcoin_usd, by = 'date') %>% 
    inner_join(selic_bcb, by = 'date') %>% 
    inner_join(risco_pais, by = 'date') %>% 
    #inner_join(inflacao_dolar, by = 'date') %>% 
    select(date, BVSP.Adjusted, `BRL=X.Adjusted`, VIXCLS, `USD (AM)`, value, risco, Last) %>% 
    clean_names()

colnames(data_orig) <- c('date', 'ibov', 'cambio', 'vix', 'gold_usd', 'selic', 'risco', 'bitcoin_usd')

saveRDS(data_orig, 'www/data_orig')

data_orig <- readRDS('www/data_orig') #%>% drop_na()

### Normalizando dados 

data_normal <- data.frame(date = seq(ymd("2014-04-15"), ymd("2021-03-22"), by = "days"))

data_orig_normalizada <- left_join(data_normal, data_orig, by = 'date') %>% 
    mutate(dummy_date = ifelse(is.na(ibov), 1 , 0))

data_orig_normalizada <- data_orig_normalizada %>% fill(c('ibov', 'cambio', 'vix', 'gold_usd', 'selic', 'risco', 'bitcoin_usd'),.direction = 'down')

saveRDS(data_orig_normalizada, 'www/data_normalized')

##### Visualizando os dados

# plot_ly(data_orig, x = ~date, y = ~ibov, type = 'scatter', mode = 'line', name = 'Ibovespa') %>% 
#     add_trace(y = ~cambio, name = 'Câmbio')

correlacao <- data_orig %>% select(-date)
correlacao <- cor(correlacao)

corrplot(correlacao, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

ggcorrplot(correlacao, method = 'circle', 
           lab = T, lab_size = 3)



set.seed(100)

linhas <- sample(1:length(data_orig$ibov), length(data_orig$ibov)*0.7)

# Dados de treino 70%
train <- data_orig[linhas,]

# Dados de teste 30%
test <- data_orig[-linhas,]

library(rpart)
modelo <- rpart(cambio ~ .,data = train, control = rpart.control(cp=0))

# Realizando previsões
test$predict <- predict(modelo, test)


modelo1 <- lm(data_orig$bitcoin_usd~data_orig$cambio,data_orig) # ajuste do modelo de regressão no R

summary(modelo1)
plot(modelo1)
