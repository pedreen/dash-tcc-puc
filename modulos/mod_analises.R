
# Manual Adjustment

mod_analises_ui <- function(id){
    
    ns <- NS(id)
    
    fluidPage(
        
        uiOutput(ns('indicadores')), # varBox com informações de indicadores
        uiOutput(ns('table_y')), # Tabelas
        uiOutput(ns('chart_y')),# gráficos para análise
        uiOutput(ns('comparacao')), # comparação de indicadores
        uiOutput(ns('candlestick')), # candlesticks charts
        
        tabBox(
            width  = 12,
            tabPanel(
                title = 'Ibov x Selic x Risco País',
                plotlyOutput(ns('ibov_selic_risco'))
            ),
            
            
            tabPanel(
                title = 'Ibov x Câmbio x VIX',
                plotlyOutput(ns('ibov_vix_cambio'))
            )
            
        )
        
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
                color = 'orange',
                paste("R$ ",value2),
                icon = icon("hand-holding-usd")
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
                paste("$ ",value6),
                icon = icon("hand-holding-usd")
            )
            
        })
        
        
        output$info7 <- renderValueBox({
            value7 <- data_normalized$bitcoin_usd  %>% last() %>% format(big.mark = ".") 
            valueBox(
                "Bitcoin",
                color = 'orange',
                paste("$ ",value7),
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
                   valueBoxOutput(ns("info5"), width = 3),
                   valueBoxOutput(ns("info4"), width = 3)
                   
            ), column(12,
                      valueBoxOutput(ns("info2"), width = 4),
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
                    line = list(color = "rgb(51, 122, 183)"),
                        hoverinfo = 'text',
                        text = ~paste("<b>Data:</b>", data_orig$date,
                                      "\n<b>Ibovespa:</b>", paste(formatC(data_orig$ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
                    ) %>%
                        layout(
                            title = '<b>Índice Ibovespa</b>',
                            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                            autosize = T, 
                            xaxis = list(title = '', showgrid = FALSE),
                            yaxis = list(title = '')
                        ) 

        })
        
        output$plot_cambio <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~cambio,
                    type = 'scatter', mode = 'lines',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Taxa de Câmbio:</b>", paste(formatC(data_orig$cambio, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "R$"))
            ) %>%
                layout(
                    title = '<b>Taxa de Câmbio (R$)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
        })
        
        
        output$plot_vix <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~vix,
                    type = 'scatter', mode = 'lines',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>VIX:</b>", paste(formatC(data_orig$vix, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
                layout(
                    title = '<b>Índice CBOE (Chicago Board Options Exchange)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
        })
        
        
        
        output$plot_selic <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~selic,
                    type = 'scatter', mode = 'lines',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Taxa Selic:</b>", paste(formatC(data_orig$selic, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "%"))
            ) %>%
                layout(
                    title = '<b>Taxa Selic (%)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
        })
        
        
        output$plot_risco <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~risco,
                    type = 'scatter', mode = 'lines',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Risco País:</b>", paste(formatC(data_orig$risco, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
            ) %>%
                layout(
                    title = '<b>Risco País (Emerging Market Bond Index Brasil)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
        })
        
        output$plot_gold <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~gold_usd,
                    type = 'scatter', mode = 'lines',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Preço do Ouro:</b>", paste(formatC(data_orig$gold_usd, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "USD"))
            ) %>%
                layout(
                    title = '<b>Preço do Ouro (USD)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
                ) 
            
        })
        
        
        output$plot_bitcoin <- renderPlotly({
            
            data_orig <- data_normalized
            
            plot_ly(data_orig, x = ~date, y = ~bitcoin_usd,
                    type = 'scatter', mode = 'lines',
                    line = list(color = "rgb(51, 122, 183)"),
                    hoverinfo = 'text',
                    text = ~paste("<b>Data:</b>", data_orig$date,
                                  "\n<b>Preço do Bitcoin:</b>", paste(formatC(data_orig$bitcoin_usd, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "USD"))
            ) %>%
                layout(
                    title = '<b>Preço do Bitcoin (USD)</b>',
                    legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                    autosize = T, 
                    xaxis = list(title = '', showgrid = FALSE),
                    yaxis = list(title = '')
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
    
    tabela_react <- reactive({
        
        data_orig <- data_normalized %>% 
            select(date, ibov, cambio, selic, risco, vix, gold_usd, bitcoin_usd) %>% 
            mutate(ibov = ibov %>% format(big.mark = ".", decimal.mark = ",")) %>% 
            mutate(cambio = cambio %>% round(digits = 2)) %>% 
            mutate(gold_usd = gold_usd %>% format(big.mark = ".", decimal.mark = ",")) %>% 
            mutate(bitcoin_usd = bitcoin_usd %>% round(digits = 2) %>% format(big.mark = ".", decimal.mark = ","))
        
        
        colnames(data_orig) <- c('Data', 'Ibovespa', 'Câmbio (R$)', 'Taxa Selic', 'Risco País', 'VIX',
                                 'Ouro (USD)', 'Bitcoin (USD)')
        
        data_orig
    })
    

    output$table_y <- renderUI({


       output$table_indicadores <- renderReactable({
           
           data_orig <- tabela_react()
           
            
           tbl <- reactable::reactable(data_orig,
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
                                       # defaultSortOrder = "desc",
                                       # defaultSorted = "Confirmed",
                                       
           )
           
        })
       
       # Download button
       
       output$download_table_indicadores <- downloadHandler(
           filename = function(){
               paste0(paste0("Tabela_indicadores"), ".xlsx")
           }, 
           content = function(fname){
               writexl::write_xlsx(tabela_react(), fname)
           }
       )
        
        
        ### UI
        
        box(title = "",
            width = 12,
            solidHeader = TRUE,

            reactableOutput(ns('table_indicadores')),
            
            footer = div(align = "right", downloadButton(ns("download_table_indicadores"), "Exportar para excel"))

        )


    })
    
    
    #### Comparações
    
    output$comparacao <- renderUI({

        #### Candle
        
        output$ibov_candle <- renderPlotly({
            
            getSymbols('^BVSP')
            ibov_series <- tk_tbl(BVSP, preserve_index = T, rename_index = 'date')
            
            plot_ly(ibov_series, x = ~date,
                    type="candlestick",
                    open = ~BVSP.Open, close = ~BVSP.Close,
                    high = ~BVSP.High, low = ~BVSP.Low 
            ) %>%
                layout(
                    title = '<b>Ibovespa (Candlestick)</b>',
                    xaxis = list(title = 'Data')
                )
            
        })
        
        
        ### UI 

        box(
            width = 12,
            solidHeader = TRUE,
            
            plotlyOutput(ns('ibov_candle'))   
        )


    })
    
    
    output$ibov_selic_risco <- renderPlotly({
        
        indicador <- y_selected()
        
        data_ibov <- data_normalized %>% 
            group_by(year(date)) %>% 
            summarise(media_ibov = mean(ibov))  
        
        data_selic <- data_normalized %>% 
            group_by(year(date)) %>% 
            summarise(media_selic = mean(selic)) 

        data_risco <- data_normalized %>%
            group_by(year(date)) %>%
            summarise(media_risco = mean(risco)) 
        
        data_orig <- left_join(data_ibov, data_selic, by = 'year(date)') %>% 
            mutate(media_risco = data_risco$media_risco)
        colnames(data_orig) <- c("date",  "media_ibov", "media_selic","media_risco")
        
        plot_ly(data_orig, x = data_orig$media_selic, y = data_orig$media_ibov ,
                type = 'scatter', 
                mode = "markers",
                color = data_orig$media_risco,
                # type = 'scatter', mode = 'lines',
                
                hoverinfo = 'text',
                text = ~paste("<b>Ibovespa:</b>",  paste(formatC(data_orig$media_ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""),
                              "\n<b>Taxa Selic:</b>", paste(formatC(data_orig$media_selic, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "%"),
                              "\n<b>Risco País:</b>", paste(formatC(data_orig$media_risco, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""))
        ) %>%
            layout(
                title = '<b>Ibovespa x Taxa Selic x Risco País</b>',
                #legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                autosize = T,
                xaxis = list(title = 'Taxa Selic (%)', showgrid = FALSE),
                yaxis = list(title = 'Ibovespa')
            )
        
    })
    
    
    output$ibov_vix_cambio <- renderPlotly({
        
        data_ibov <- data_normalized %>% 
            group_by(year(date)) %>% 
            summarise(media_ibov = mean(ibov))  
        
        data_vix <- data_normalized %>% 
            group_by(year(date)) %>% 
            summarise(media_vix = mean(vix)) 
        
        data_cambio <- data_normalized %>%
            group_by(year(date)) %>%
            summarise(media_cambio = mean(cambio)) 
        
        data_orig <- left_join(data_ibov, data_vix, by = 'year(date)') %>% 
            mutate(media_cambio = data_cambio$media_cambio)
        colnames(data_orig) <- c("date",  "media_ibov", "media_vix","media_cambio")
        
        plot_ly(data_orig, x = data_orig$media_cambio, y = data_orig$media_ibov,
                type = 'scatter', 
                mode = "markers",
                color = data_orig$media_vix,
                # type = 'scatter', mode = 'lines',
                
                hoverinfo = 'text',
                text = ~paste("<b>Ibovespa:</b>", paste(formatC(data_orig$media_ibov, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""),
                              "\n<b>VIX:</b>", paste(formatC(data_orig$media_vix, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), ""),
                              "\n<b>Câmbio:</b>", paste(formatC(data_orig$media_cambio, digits = 2, format = "f", big.mark = ".", decimal.mark = ","), "R$"))
        ) %>%
            layout(
                title = '<b>Ibovespa x Câmbio x VIX</b>',
                #legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
                autosize = T,
                xaxis = list(title = 'Câmbio (R$)', showgrid = FALSE),
                yaxis = list(title = 'Ibovespa')
            )
        
    })
    
}
