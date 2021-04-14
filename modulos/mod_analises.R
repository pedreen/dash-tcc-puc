
# Manual Adjustment

mod_analises_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('indicadores')),
        uiOutput(ns('table_y')), # Tabelas
        uiOutput(ns('chart_y')),# gráficos para análise
        uiOutput(ns('comparacoes')) # comparação de indicadores
        
    )
    
}

mod_analises_server <- function(input, output, session, y_selected){
    

    
    ns <- session$ns
    
    
    output$indicadores <- renderUI({
        
        # ValuesBox dos indicadores
        
        output$info1 <- renderValueBox({
            value1 <- data_normalized$ibov %>% last() %>% format(big.mark = ".")
            valueBox(
                "IBOV",
                color = 'green',
                value1,
                icon = icon("fas fa-chart-line")
            )
            
        })
        
        output$info2 <- renderValueBox({
            value2 <- data_normalized$cambio %>% round(2) %>% last() %>% format(big.mark = ".") 
            valueBox(
                "Câmbio",
                paste(value2, " %"),
                icon = icon("percentage")
            )
            
        })
        
        output$info3 <- renderValueBox({
            value3 <- data_normalized$vix %>% round(2) %>% last() %>% format(big.mark = ".") 
            valueBox(
                "VIX",
                color = 'green',
                paste(value3, ""),
                icon = icon("fas fa-chart-line")
            )
            
        })
        
        
        output$info4 <- renderValueBox({
            value4 <- data_normalized$selic %>% round(2)  %>% last() %>% format(big.mark = ".") 
            valueBox(
                "Selic",
                paste(value4, "%"),
                icon = icon("percentage")
            )
            
        })
        
        output$info5 <- renderValueBox({
            value5 <- data_normalized$risco  %>% last() %>% format(big.mark = ".") 
            valueBox(
                "Risco País",
                color = 'red',
                value5,
                icon = icon("users")
            )
            
        })
        
        output$info6 <- renderValueBox({
            value6 <- data_normalized$gold_usd  %>% last() %>% format(big.mark = ".") 
            valueBox(
                "Ouro",
                color = 'orange',
                value6,
                icon = icon("hand-holding-usd")
            )
            
        })
        
        
        output$info7 <- renderValueBox({
            value7 <- data_normalized$bitcoin_usd  %>% last() %>% format(big.mark = ".") 
            valueBox(
                "Bitcoin",
                color = 'orange',
                value7,
                icon = icon("hand-holding-usd")
            )
            
        })
        
        ### UI
        
        box(title = "Dados dos indicadores",
            width = 12,
            solidHeader = TRUE,
            
            # ValueBox dos dados de evoluçaõ do Covid 
            column(12,
                   valueBoxOutput(ns("info1"), width = 3),
                   valueBoxOutput(ns("info3"), width = 3), 
                   valueBoxOutput(ns("info2"), width = 3),
                   valueBoxOutput(ns("info4"), width = 3)
                   
            ), column(12,
                      valueBoxOutput(ns("info5"), width = 4),
                      valueBoxOutput(ns("info6"), width = 4),
                      valueBoxOutput(ns("info7"), width = 4)
            )
            
        )
        
        
        
    })
    
    
    
    output$chart_y <- renderUI({
        
        # Gráficos dos indicadores

        output$plot_ibov <- renderPlotly({

            data_orig <- data_normalized

            plot_ly(data_orig, x = ~date, y = ~ibov,
                    type = 'scatter', mode = 'lines',

                        hoverinfo = 'text',
                        text = ~paste("<b>Data:</b>", data_orig$date,
                                      "\n<b>Ibovespa:</b>", paste(formatC(data_orig$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
                    ) %>%
                        layout(
                            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                            autosize = T, 
                            xaxis = list(title = '', showgrid = FALSE),
                            yaxis = list(title = '<b>Índice Ibovespa</b>')
                        ) 

        })
        
        output$plot_cambio <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~cambio,
                    type = 'scatter', mode = 'lines',
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Taxa de Câmbio:</b>", paste(formatC(data_orig$cambio, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "R$"))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Taxa de Câmbio (R$)</b>')
                ) 
            
        })
        
        
        output$plot_vix <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~vix,
                    type = 'scatter', mode = 'lines',
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>VIX:</b>", paste(formatC(data_orig$vix, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Índice CBOE (Chicago Board Options Exchange)</b>')
                ) 
            
        })
        
        
        
        output$plot_selic <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~selic,
                    type = 'scatter', mode = 'lines',
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Taxa Selic:</b>", paste(formatC(data_orig$selic, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "%"))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Taxa Selic (%)</b>')
                ) 
            
        })
        
        
        output$plot_risco <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~risco,
                    type = 'scatter', mode = 'lines',
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Risco País:</b>", paste(formatC(data_orig$risco, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Risco País (Emerging Market Bond Index Brasil)</b>')
                ) 
            
        })
        
        output$plot_gold <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~gold_usd,
                    type = 'scatter', mode = 'lines',
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Preço do Ouro:</b>", paste(formatC(data_orig$gold_usd, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "USD"))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Preço do Ouro (USD)</b>')
                ) 
            
        })
        
        
        output$plot_bitcoin <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~bitcoin_usd,
                    type = 'scatter', mode = 'lines',
                    
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Preço do Bitcoin:</b>", paste(formatC(data_orig$bitcoin_usd, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "USD"))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Preço do Bitcoin (USD)</b>')
                ) 
            
        })

        ##### UI ####
        
        tabBox(
            width  = 12,
            tabPanel(
                title = "IBOV",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_ibov'))
                
            ),
            
            tabPanel(
                title = "VIX",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_vix'))
                
            ),
            
            
            tabPanel(
                title = "Câmbio",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_cambio'))
                
            ),
            

            tabPanel(
                title = "Selic",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_selic'))

            ),

            tabPanel(
                title = "Risco país",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_risco'))

            ),

            tabPanel(
                title = "Ouro",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_gold'))

            ),

            tabPanel(
                title = "Bitcoin",
                #div(style = 'overflow-x: scroll',
                plotlyOutput(ns('plot_bitcoin'))

            )

        )
        

        # box(
        #     title = "Ibovespa anual",
        #     solidHeader = TRUE,
        #     width = 12,
        # 
        #     plotlyOutput(ns('data_comp'))
        # )
    })
    

    output$table_y <- renderUI({
       
        data_orig <- data_normalized %>% 
            select(date, ibov, cambio, selic, risco, vix, gold_usd, bitcoin_usd) %>% 
            mutate(ibov = ibov %>% format(big.mark = ".", decimal.mark = ",")) %>% 
            mutate(cambio = cambio %>% round(digits = 2)) %>% 
            mutate(gold_usd = gold_usd %>% format(big.mark = ".", decimal.mark = ",")) %>% 
            mutate(bitcoin_usd = bitcoin_usd %>% round(digits = 2) %>% format(big.mark = ".", decimal.mark = ","))
            
            
        colnames(data_orig) <- c('Data', 'Ibovespa', 'Taxa de Câmbio', 'Taxa Selic', 'Risco País', 'VIX',
                                 'Ouro (USD)', 'Bitcoin (USD)')

       output$table_indicadores <- renderReactable({
            
           tbl <- reactable::reactable(data_orig,
                                       pagination = FALSE,
                                       highlight = TRUE,
                                       height = 400,
                                       sortable = TRUE,
                                       borderless = TRUE,
                                       defaultPageSize = nrow(data_orig)
                                       # defaultSortOrder = "desc",
                                       # defaultSorted = "Confirmed",
                                       
           )
           
        })
        
        
        ### UI
        
        box(title = "",
            width = 12,
            solidHeader = TRUE,

            reactableOutput(ns('table_indicadores'))

        )


    })
    
    
    #### Comparações
    
    output$comparacoes <- renderUI({


        output$comp_chart <- renderPlotly({

            indicador <- y_selected()

            data_orig <- data_normalized

            plot_ly(data_orig, x = ~date, y = ~bitcoin_usd,
                    type = 'scatter', mode = 'lines',

                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Preço do Bitcoin:</b>", paste(formatC(data_orig$bitcoin_usd, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "USD"))
            ) %>%
                layout(
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T,
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '<b>Preço do Bitcoin (USD)</b>')
                )

        })
        
        ### UI 
        
        box(title = "",
            width = 12,
            solidHeader = TRUE,
            
            column(4,
            pickerInput(
                inputId = "indicadores",
                label   =  "Selecione:",
                choices = indicadores_list,
                multiple = TRUE,
                options = list(`live-search` = TRUE, `actions-box` = TRUE,
                               `max-options` = 2, `none-selected-text`="Nenhum item selecionado",
                               `size` = 6)
            )
            )
        )


    })
    
}
