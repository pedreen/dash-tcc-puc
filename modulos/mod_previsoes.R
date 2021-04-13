
# Manual Adjustment

mod_previsoes_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        # uiOutput(ns('model_type')), # tipo de modelo
        # uiOutput(ns('visualizacoes')), # gráfico com histórico + projeção (original e ajustada)
        # uiOutput(ns('ajustes')) # tabela original e ajustável + botões 
        
    )
    
}

mod_previsoes_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
    
    
}
