# XGBOOST

mod_xgboost_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('xgboost')), # visualizações do módulo ,
        
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
        
    )
    
}


mod_xgboost_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
    output$xgboost <- renderUI({
        
        output$erros <- renderPrint({
            
            data <- forecast_xgboost
            
            cat("MSE: ", data$mse, "MAE: ", data$mae, " RMSE: ", data$rmse)
            
        })
        
        output$importance <- renderPrint({
            
            xgb.importance(model = model_xgboost)
            
        })
        
        ## Gráficos
        
        output$comparacao_plot <- renderPlotly({
            
            model_num <- 1
            dados <- forecast_xgboost 
            
            data_orig <- dados$model_hist
            data_orig <- data_orig[[1]]
            data_orig <- data_orig[order(data_orig$date),]
            data_fit <- dados$model_fit 
            data_fit <- data_fit[[1]]
            
            plot_ly(type = 'scatter', mode = 'lines') %>% 
                
                add_trace(
                    x = data_orig$date, y = data_orig$ibov,
                    name = 'Dado Histórico (Hist.)',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Modelo:</b>", model_num,
                                  "\n<b>Data:</b>", data_orig$date,
                                  "\n<b>Ibovespa (hist.):</b>", paste(formatC(data_orig$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
                ) %>%
                
                
                
                add_trace(
                    x = data_fit$date, y = data_fit$predict,
                    name = 'Forecast (Fit)',
                    line = list(color = "rgb(178,34,34)", dash = 'dot'), 
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
        
        ## UI 
        
        fluidRow(
            
            # box(title = "",
            #     width = 6,
            #     solidHeader = TRUE,
            #     column(4,
            #            selectInput(
            #                inputId = ns('model'),
            #                label = 'Selecione o modelo:',
            #                choices = c(1),
            #                selected = 1
            #                
            #            )
            #     )
            # ),
            
            box(title = "Erros do modelo",
                width = 6,
                solidHeader = TRUE,
                verbatimTextOutput(ns('erros'))
            ),
            
            box(title = "Tabela de Importância",
                width = 6,
                solidHeader = TRUE,
                verbatimTextOutput(ns('importance'))
            ),
            
            box(title = "",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns('comparacao_plot'))
            )
        )
        
    })
    
    
    
    # Gráfis de Pare
    
    output$pareto <- renderPlotly({
        
        forecast <- forecast_rf 
        
        forecast <- forecast$model_hist
        forecast <- forecast[[1]]
        forecast <- forecast[order(forecast$date),]
        
        forecast_anual <- forecast %>% 
            mutate(ano = year(date)) %>% 
            group_by(ano) %>% 
            summarise(nivel_anual = mean(ibov, na.rm = TRUE)) %>% 
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
        
        forecast <- forecast_rf 
        
        forecast <- forecast$model_fit
        forecast <- forecast[[1]]
        
        forecast_anual <- forecast %>% 
            mutate(ano = year(date)) %>% 
            group_by(ano) %>% 
            summarise(nivel_anual = mean(predict, na.rm = TRUE)) %>% 
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
    
    
    
}

