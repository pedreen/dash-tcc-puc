# Detalhes modelos

mod_detalhes_modelos_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('correlacao')), # tipo de modelo
        reactableOutput(ns('table_indicadores'))
    )
    
}


mod_detalhes_modelos_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
    output$table_indicadores <- renderReactable({
        
        
        forecast_total <- bind_rows(forecast_tree, forecast_rf, forecast_xgboost) %>% 
            mutate(y = 'IBOV')
        
        data_orig <- forecast_total %>%
            select(y, numero_modelo, tipo_modelo, mse, mae,rmse) %>% 
            mutate(mse = mse %>% round(digits = 2) %>% format(big.mark = '.', small.mark = ',')) %>% 
            mutate(mae = mae %>% round(digits = 2) %>% format(big.mark = '.', small.mark = ',')) %>% 
            mutate(rmse = rmse %>% round(digits = 2) %>% format(big.mark = '.', small.mark = ','))
        
        colnames(data_orig) <- c('Y', 'N° Modelo', 'Tipo Modelo', 'MSE', 'MAE', 'RMSE')
        
        
        
        
        reactable::reactable(data_orig,
                             pagination = FALSE,
                             highlight = TRUE,
                             height = 400,
                             sortable = TRUE,
                             borderless = TRUE,
                             defaultPageSize = nrow(data_orig),
                             defaultColDef = colDef(
                                 align = 'center'
                             ),
                             bordered = TRUE
                             
                             
        )
        
    })
    
    
    output$correlacao <- renderUI({
        
        ### Correlações
        output$correlacao1 <- renderPlot({
            
            correlacao <- data_normalized %>% select(-date, -dummy_date)
            correlacao <- cor(correlacao)
            
            corrplot(correlacao, type = "upper", order = "hclust", 
                     tl.col = "black", tl.srt = 45, addCoef.col = T)
            
            
            
        })
        
        output$correlacao2 <- renderPlot({
            
            correlacao <- data_normalized %>% select(-date, -dummy_date)
            correlacao <- cor(correlacao)
            
            ggcorrplot(correlacao, method = 'circle', 
                       lab = T, lab_size = 3)
            
            
        })
        
        ### UI 
        
        fluidRow(
            tabBox(
                title = 'Correlação',
                width  = 12,
                tabPanel(
                    title = "Função cor()",
                    #div(style = 'overflow-x: scroll',
                    plotOutput(ns('correlacao1'))
                    
                ),
                
                tabPanel(
                    title = "Função ggcorrplot()",
                    #div(style = 'overflow-x: scroll',
                    plotOutput(ns('correlacao2'))
                    
                )
            )
        )
        
    })
    
    
}