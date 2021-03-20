##### Carregando bases

library(dplyr)
library(janitor)
library(plotly)
library(sf)
library(xlsx)

## Dados do IBGE
ibge <- read.csv("bases/mapas/estadosibge.csv", header = T) %>% 
    select(codigo.uf = Código.UF, UF) 
## Centroides
centroids <- readRDS("bases/mapas/centroids") %>% 
    rename(UF = uf)

## Shape Ufs Brasil
shp <- shape_uf_br <- st_read('bases/mapas/shape_ufs.geojson')


## Base PIB (que será usada para as análises)
# base_pib <- xlsx::read.xlsx('bases/PIB dos Municípios - base de dados 2010-2018.xls', sheetIndex = 1)
# 
# saveRDS(base_pib, 'www/base_pib_mun_2010_2018')

# a <- readxl::read_excel("bases/PIB dos Municípios - base de dados 2010-2018.xls", range = "A1:AQ1")
# names_col_pib <- colnames(a)
# saveRDS(names_col_pib, 'www/names_col_pib')


base_pib <- readRDS('www/base_pib_mun_2010_2018') #%>% 
    #select(-)#%>% clean_names() 
