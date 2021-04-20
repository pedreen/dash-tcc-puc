
# Manual Adjustment

mod_previsoes_arvore_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('correlcao')), # correlação
       
        fluidRow( 
        box(title = "",
            width = 4,
            solidHeader = TRUE,
            column(4,
                   selectInput(
                       inputId = ns('model'),
                       label = 'Selecione o modelo:',
                       choices = c(1,2,3),
                       selected = 1
                       
                   )
            )
        ),
        
        box(title = "",
            width = 8,
            solidHeader = TRUE,
            
            h4("Error Summary"),
            verbatimTextOutput(ns('erro'))

        )),
        
        uiOutput(ns('anual_view')), # gráficos com dado histórico e forecast 
        uiOutput(ns('decision_tree')) # tipo de modelo
       

    )
    
}

mod_previsoes_arvore_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    output$correlcao <- renderUI({
        
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
    
    
    
    output$decision_tree <- renderUI({

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
        
        dados <- forecast_tree %>% filter(numero_modelo == model_num)
        
        data_orig <- dados$model_hist
        data_orig <- data_orig[[1]]
        data_orig <- data_orig[order(data_orig$date),]
        data_fit <- dados$model_fit 
        data_fit <- data_fit[[1]]
        
        plot_ly(type = 'scatter', mode = 'lines') %>% 
            
            add_trace(
                x = data_orig$date, y = data_orig$ibov,
                name = 'Dado Histórico (Hist.)',
                line = list(color = "rgb(47, 73, 139)"),
                hoverinfo = 'text',
                text = ~paste("<b>Modelo:</b>", model_num,
                              "\n<b>Data:</b>", data_orig$date,
                              "\n<b>Ibovespa (hist.):</b>", paste(formatC(data_orig$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
            
            
            
            add_trace(
                x = data_fit$date, y = data_fit$predict,
                name = 'Forecast (Fit)',
                line = list(color = "rgb(250, 0, 0)", dash = 'dot'), 
                hoverinfo = 'text',
                text = ~paste("<b>Modelo:</b>", model_num,
                              "\n<b>Data:</b>", data_fit$date,
                              "\n<b>Ibovespa (fit):</b>", paste(formatC(data_fit$predict, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
                
            ) %>% 
            layout(
                title = '<b>Ibovespa (Hist. vs Fit) </b>',
                legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                autosize = T, 
                xaxis = list(title = '', showgrid = FALSE),
                yaxis = list(title = '')
            ) 
        
    })
    
    
    ##### UI

    fluidRow(
    box(title = "",
        width = 12,
        solidHeader = TRUE,

        plotlyOutput(ns('comparacao_plot'))
        
    )
    )

    })
    
    
    output$anual_view <- renderUI({
        
        
        # Plot anual
        
        output$plot_anual_hist <- renderPlotly({
            
            
            model_num <- input$model
            
            dados <- forecast_tree %>% filter(forecast_tree$numero_modelo == model_num)
            
            data_orig <- dados$model_hist
            data_orig <- data_orig[[1]]
            data_orig <- data_orig[order(data_orig$date),]
            
            data_ano_orig <- data_orig %>% 
                group_by(year(date)) %>% 
                summarise(total_ano = sum(ibov))
            
            colnames(data_ano_orig) <- c('ano', 'total')
            
            plot_ly(data_ano_orig, x = ~ano, y = ~total,
                    type = 'bar', 
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Modelo:</b>", model_num,
                                  "\n<b>Data:</b>", data_ano_orig$ano,
                                  "\n<b>Ibovespa:</b>", paste(formatC(data_ano_orig$total, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
                
                layout(
                    title = '<b>Índice Ibovespa - YoY (Hist.)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
            
        })
        
        output$plot_anual_fit <- renderPlotly({
            
            
            model_num <- input$model
            
            dados <- forecast_tree %>% filter(forecast_tree$numero_modelo == model_num)
            
            data_fit <- dados$model_fit 
            data_fit <- data_fit[[1]]
            
            
            data_ano_fit <- data_fit %>% 
                group_by(year(date)) %>% 
                summarise(total_ano = sum(predict))
            
            colnames(data_ano_fit) <- c('ano', 'total')
            
            plot_ly(data_ano_fit, type = 'bar', 
                    x = data_ano_fit$ano, y = data_ano_fit$total,
    
                    hoverinfo = 'text',
                    text = ~paste("<b>Modelo:</b>", model_num,
                                  "\n<b>Data:</b>", data_ano_fit$ano,
                                  "\n<b>Ibovespa:</b>", paste(formatC(data_ano_fit$total, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
                layout(
                    title = '<b>Índice Ibovespa - YoY (Fit)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
        })
        
        output$pareto <- renderPlotly({
            
            model_num <- input$model
        
            forecast <- forecast_tree %>% filter(forecast_tree$numero_modelo == model_num)
            
            forecast <- forecast$model_hist
            forecast <- forecast[[1]]
            forecast <- forecast[order(forecast$date),]
            
            forecast_anual <- forecast %>% 
                mutate(ano = year(date)) %>% 
                group_by(ano) %>% 
                summarise(nivel_anual = sum(ibov, na.rm = TRUE)) %>% 
                ungroup() %>% 
                mutate(taxa_anual = nivel_anual/lag(nivel_anual) - 1) %>% 
                mutate_at(vars(taxa_anual), ~ round(.*100, 2)) %>% # passa p/ porcentagem
                filter(!is.na(taxa_anual))
            
            
            line <- list(width = "1.5", shape = "spline", smoothing = 0.50)
            
            plot_ly() %>% 
                
                add_trace(data = forecast_anual,
                          x = ~as.character(ano),
                          y = ~nivel_anual, 
                          type = 'bar', name = "Nível", 
                          text = forecast_anual$nivel_anual %>% round(digits = 2) %>% format(big.mark = ".", decimal.mark = ","), textposition = 'auto'
                ) %>% 
                
                add_trace(data = forecast_anual,
                          x = ~as.character(ano),
                          y = ~taxa_anual, 
                          type = 'scatter', mode = 'lines+markers', 
                          line = c(line, list(color = 'rgb(36,45,55)')),
                          marker = list(color = 'rgb(36,45,55)'), 
                          legendgroup = "yoy", name = paste("YoY", "(%)"), yaxis = 'y2') %>% 
                

                layout(title = '<b>Dados Históricos</b>',
                       legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.35), 
                       xaxis = list(title = ''),
                       yaxis = list(title = 'Nível YoY'),
                       yaxis2 = list(title = 'Variação YoY', ticksuffix = "%", side = 'right', overlaying = "y", zeroline = F, showgrid = F),
                       margin = list(l = 20, r = 60))
            
        })
        
        
        output$pareto_forecast <- renderPlotly({
            
            model_num <- input$model
            
            forecast <- forecast_tree %>% filter(forecast_tree$numero_modelo == model_num)
            
            forecast <- forecast$model_fit
            forecast <- forecast[[1]]
            
            forecast_anual <- forecast %>% 
                mutate(ano = year(date)) %>% 
                group_by(ano) %>% 
                summarise(nivel_anual = sum(predict, na.rm = TRUE)) %>% 
                ungroup() %>% 
                mutate(taxa_anual = nivel_anual/lag(nivel_anual) - 1) %>% 
                mutate_at(vars(taxa_anual), ~ round(.*100, 2)) %>% # passa p/ porcentagem
                filter(!is.na(taxa_anual))
            
            
            line <- list(width = "1.5", shape = "spline", smoothing = 0.50)
            
            plot_ly() %>% 
                
                add_trace(data = forecast_anual,
                          x = ~as.character(ano),
                          y = ~nivel_anual, 
                          type = 'bar', name = "Nível", 
                          text = forecast_anual$nivel_anual %>% round(digits = 2) %>% format(big.mark = ".", decimal.mark = ","), textposition = 'auto'
                ) %>% 
                
                add_trace(data = forecast_anual,
                          x = ~as.character(ano),
                          y = ~taxa_anual, 
                          type = 'scatter', mode = 'lines+markers', 
                          line = c(line, list(color = 'rgb(36,45,55)')),
                          marker = list(color = 'rgb(36,45,55)'), 
                          legendgroup = "yoy", name = paste("YoY", "(%)"), yaxis = 'y2') %>% 
                
                
                layout(title = '<b>Dados Forecast</b>',
                       legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.35), 
                       xaxis = list(title = ''),
                       yaxis = list(title = 'Nível YoY'),
                       yaxis2 = list(title = 'Variação YoY', ticksuffix = "%", side = 'right', overlaying = "y", zeroline = F, showgrid = F),
                       margin = list(l = 20, r = 60))
            
        })
        
        
        #### UI
        
        fluidRow(
        box(title = "",
            width = 6,
            solidHeader = TRUE,
            
            plotlyOutput(ns('plot_anual_hist'))
        ),
        
        box(title = "",
            width = 6,
            solidHeader = TRUE,
            
            plotlyOutput(ns('plot_anual_fit'))
        )
        )
        
        fluidRow(
            box(title = "",
                width = 6,
                solidHeader = TRUE,
                
                plotlyOutput(ns('pareto'))
            ),
            
            box(title = "",
                width = 6,
                solidHeader = TRUE,

                plotlyOutput(ns('pareto_forecast'))
            )
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



# Ramdom Forest

mod_previsoes_rf_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        fluidRow( 
            box(title = "",
                width = 4,
                solidHeader = TRUE,
                column(4,
                       selectInput(
                           inputId = ns('model'),
                           label = 'Selecione o modelo:',
                           choices = c(1,2,3),
                           selected = 1
                           
                       )
                )
            ),
            
            box(title = "",
                width = 8,
                solidHeader = TRUE,
                
                h4("Error Summary"),
                verbatimTextOutput(ns('erro'))
                
            )),
        
        uiOutput(ns('rf')) # tipo de modelo
        # uiOutput(ns('visualizacoes')), # gráfico com histórico + projeção (original e ajustada)
        # uiOutput(ns('ajustes')) # tabela original e ajustável + botões 
        
    )
    
}


mod_previsoes_rf_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
}