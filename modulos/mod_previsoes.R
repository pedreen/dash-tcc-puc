
# Manual Adjustment

mod_previsoes_arvore_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('decision_tree')) # tipo de modelo
        # uiOutput(ns('visualizacoes')), # gráfico com histórico + projeção (original e ajustada)
        # uiOutput(ns('ajustes')) # tabela original e ajustável + botões 
        
    )
    
}

mod_previsoes_arvore_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
    output$decision_tree <- renderUI({
        
    ### Correlações
    output$correlacao1 <- renderPlot({
        
        correlacao <- data_orig_normalizada %>% select(-date, -dummy_date)
        correlacao <- cor(correlacao)
        
        corrplot(correlacao, type = "upper", order = "hclust", 
                 tl.col = "black", tl.srt = 45)
        
        
        
    })
    
    output$correlacao2 <- renderPlot({
        
        correlacao <- data_orig_normalizada %>% select(-date, -dummy_date)
        correlacao <- cor(correlacao)
        
        ggcorrplot(correlacao, method = 'circle', 
                   lab = T, lab_size = 3)
        
        
    })
    
    ## Erro
    
    output$erro <- renderPrint({
        
        model_num <- input$model
        
        if(model_num == 1){
            erro <- erro_tree1
        } else if(model_num == 2){
            erro <- erro_tree2
        } else {
            erro <- erro_tree3
        }
        
        erro
        
    })
    
    
    ## Gráficos
    
    output$comparacao_plot <- renderPlotly({
        
        model_num <- input$model
        
        print(model_num)
        
        dados <- forecast_tree %>% filter(numero_modelo == model_num)
        
        data_orig <- dados$model_hist
        data_orig <- data_orig[[1]]
        data_orig <- data_orig[order(data_orig$date),]
        data_fit <- dados$model_fit 
        data_fit <- data_fit[[1]]
        
        plot_ly(type = 'scatter', mode = 'lines') %>% 
            
            add_trace(
                x = data_orig$date, y = data_orig$ibov,
                name = 'Hist.',
                line = list(color = "rgb(47, 73, 139)"),
                hoverinfo = 'text',
                text = ~paste("<b>Modelo:</b>", model_num,
                              "\n<b>Data:</b>", data_orig$date,
                              "\n<b>Ibovespa (hist.):</b>", paste(formatC(data_orig$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
            
            
            
            add_trace(
                x = data_fit$date, y = data_fit$predict,
                name = 'Fit',
                line = list(color = "rgb(250, 0, 0)", dash = 'dot'), 
                hoverinfo = 'text',
                text = ~paste("<b>Modelo:</b>", model_num,
                              "\n<b>Data:</b>", data_fit$date,
                              "\n<b>Ibovespa (fit):</b>", paste(formatC(data_fit$predict, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
                
            ) %>% 
            layout(
                legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                autosize = T, 
                xaxis = list(title = '', showgrid = FALSE),
                yaxis = list(title = '<b>Ibovespa (Hist. vs Fit) </b>')
            ) 
        
    })
    
    ##### UI
    
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
    

    box(title = "",
        width = 12,
        solidHeader = TRUE,
        
        fluidRow(
        column(4,
               selectInput(
                   inputId = ns('model'),
                   label = 'Selecione o modelo:',
                   choices = c(1,2,3) %>% as.numeric(),
                   selected = 1
                   
               )
        ),
        
        column(8,
        h2("Error Summary"),
        verbatimTextOutput(ns('erro'))
        )
        ),
        
        fluidRow(),
        
        plotlyOutput(ns('comparacao_plot'))
        
    )
    
        
    })
    
}





# Model Arima

mod_previsoes_arima_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('arima')) # tipo de modelo
        # uiOutput(ns('visualizacoes')), # gráfico com histórico + projeção (original e ajustada)
        # uiOutput(ns('ajustes')) # tabela original e ajustável + botões 
        
    )
    
}


mod_previsoes_arima_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
}
