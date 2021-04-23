

# Ramdom Forest


## Previsões

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
            
            box(title = "Erros do modelo", 
                width = 8, 
                solidHeader = T, 
                verbatimTextOutput(ns("info"))
            )
            
            
        ),
        
        fluidRow(
            
            box(title = "Importância das variáveis explicativas (Tabela)", 
                width = 12,
                solidHeader = T, 
                reactableOutput(ns("var_imp"))
            ), 
            
            box(title = "Importância das variáveis explicativas (Gráfico)", 
                width = 12, 
                solidHeader = T, 
                plotOutput(ns("var_imp_chart"))
            )
            
        ),
        
        # fluidRow(
        #     box(title = "Distribution of minimal depth",
        #         width = 12,
        #         solidHeader = TRUE,
        #         
        #         
        #         plotOutput(ns('disp_rf'))
        #     ),
        #     
        #     box(title = "Multi-way importance plot",
        #         width = 12,
        #         solidHeader = TRUE,
        #         
        #         
        #         plotOutput(ns('disp_rf2'))
        #     ),
        #     
        #     box(title = "Compare importance measures",
        #         width = 12,
        #         solidHeader = TRUE,
        #         
        #         
        #         plotOutput(ns('disp_rf3'))
        #     )
        #     
            
            # box(title = "",
            #     width = 12,
            #     solidHeader = TRUE,
            #     
            #     
            #     plotOutput(ns('disp_rf4'))
            # )
        #     
        #     
        # )    
        
        #uiOutput(ns('correlacao')),
        uiOutput(ns('info_model')),
        uiOutput(ns('prediction_rf')),
        uiOutput(ns('rf')) # tipo de modelo
        
    )
    
}


mod_previsoes_rf_server <- function(input, output, session){
    
    
    
    ns <- session$ns
    
    
    output$info <- renderPrint({
        
        model_num <- input$model
        
         data <- forecast_rf %>% 
             filter(numero_modelo == model_num)

        
        data <- data %>% 
            select(mse, mae, rmse)
        
        cat('MSE:', data$mse, 'MAE:', data$mae, 'RMSE:', data$rmse)
        
    })
    
    
    output$var_imp <- renderReactable({
        
        
        model_num <- input$model
        
        if(model_num == 1){
            
            data <- model1
            
        } else if(model_num == 2){
            
            data <- model2
            
        } else {
            
            data <- model3
            
        }
        
        data <- data$importance 
        
            reactable::reactable(data,
                                    pagination = FALSE,
                                    highlight = TRUE,
                                    height = 400,
                                    sortable = TRUE,
                                    borderless = TRUE,
                                    defaultPageSize = nrow(data),
                                    defaultColDef = colDef(
                                        align = 'center'
                                    ),
                                    bordered = TRUE
                                    
        )
        
    })
    
    
    
    output$var_imp_chart <- renderPlot({
        
        
        model_num <- input$model
        
        if(model_num == 1){
            
            data <- model1
            
        } else if(model_num == 2){
            
            data <- model2
            
        } else {
            
            data <- model3
            
        }
        
        varImpPlot(data, col="darkblue", pch=19)
        
    })
    
    
    output$prediction_rf <- renderUI({
        
        
        ## Gráficos
        
        output$comparacao_plot <- renderPlotly({
            
            model_num <- input$model
            
            dados <- forecast_rf %>% filter(numero_modelo == model_num)
            
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
        
        
        ##### UI
        
        fluidRow(
            box(title = "",
                width = 12,
                solidHeader = TRUE,
                
                plotlyOutput(ns('comparacao_plot'))
                
            )
        )
        
        
    })
    
    
    output$rf <- renderUI({
        
        
        # Plot anual
        
        output$plot_anual_hist <- renderPlotly({
            
            
            model_num <- input$model
            
            dados <- forecast_rf %>% filter(forecast_rf$numero_modelo == model_num)
            
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
            
            dados <- forecast_rf %>% filter(forecast_rf$numero_modelo == model_num)
            
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
            
            forecast <- forecast_rf %>% filter(forecast_rf$numero_modelo == model_num)
            
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
            
            forecast <- forecast_rf %>% filter(forecast_rf$numero_modelo == model_num)
            
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
    
    
    # Disp plot
    
    output$disp_rf <- renderPlot({
        
        model_num <- input$model
        
        if(model_num == 1){
            
            data_disp <- disp_model1
            
        } else if(model_num == 2){
            
            data_disp <- disp_model2
            
        } else {
            
            data_disp <- disp_model3
            
        }
        
        plot_min_depth_distribution(data_disp)
        
    })
    
    output$disp_rf2 <- renderPlot({
        
        model_num <- input$model
        
        if(model_num == 1){
            
            data_disp <- imp_mode1
            
        } else if(model_num == 2){
            
            data_disp <- imp_mode2
            
        } else {
            
            data_disp <- imp_mode3
            
        }
        
        plot_multi_way_importance(data_disp, x_measure = 'mse_increase' , y_measure = 'node_purity_increase', size_measure = 'p_value',
                                  no_of_labels = 8)
        
    })
    
    output$disp_rf3 <- renderPlot({
        
        model_num <- input$model
        
        if(model_num == 1){
            
            data_disp <- imp_mode1
            
        } else if(model_num == 2){
            
            data_disp <- imp_mode2
            
        } else {
            
            data_disp <- imp_mode3
            
        }
        
        plot_importance_ggpairs(data_disp)
        
    })
    
    
    # output$disp_rf4 <- renderPlot({
    #     
    #     model_num <- input$model
    #     
    #     forecast_rf <- forecast_rf %>% filter(forecast_rf$numero_modelo == model_num)
    #     
    #     forecast <- forecast_rf$model_fit
    #     forecast <- forecast[[1]] %>% as_tibble()
    # 
    #     plot_predict_interaction(forecast_rf, data_normalized, 'selic', 'ibov')
    #     
    # })
    
    
}