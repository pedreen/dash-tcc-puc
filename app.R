suppressMessages({
    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(tibble)
    library(purrr)
    library(lubridate)
    library(plotly)
    library(writexl)
    library(lmtest)
    library(broom)
    library(shinyjs)
    library(shinyBS)
    library(forecast)
    library(glmnet)
    library(caret)
    library(DT) 
    library(ForecastComb) 
    library(waiter)
    library(formattable)
    library(randomForest)
    library(shinycssloaders) # Lib para spinners
    library(reactable)

})

options(warn = -1, scipen = 999)

# Source files
source('load_data/load_data.R')
atualization_date_input <- '14-04-2021'
nome_projeto <- 'TCC-Puc'
logo = 'PUCMINAS.png'

# sidebar
sidebar <- dashboardSidebar(
    
    includeCSS("www/css/colors.css"), 
    includeCSS("www/css/elements.css"), 
    includeCSS("www/css/custom.css"), 
    includeCSS("www/css/extra.css"), 
    
    useShinyjs(),
    
    use_waiter(),
    
    show_waiter_on_load(
        logo = 'PUCMINAS.png', 
        tagList(
            spin_fading_circles(), 
            span("Loading...", style = "font-size: 18px;")
        )
    ),
    
    sidebarMenuOutput("renderSidebar")
    
)

# ui
ui <- dashboardPage(
    title = nome_projeto, 
    dashboardHeader(title = tags$a(tags$img(src = logo, width = "25%", style = "margin-bottom: 5px;"))), 
    sidebar,
    dashboardBody(
        tabItems(
            
            #tabItem(tabName = "tutorial", mod_tutorial_ui("tutorial")),

            tabItem(tabName = "analises", mod_analises_ui("analises")),

            tabItem(tabName = "previsao", mod_previsoes_ui("previsao"))
            
            
        )
    )
)

# server
server <- function(input, output, session) {
    
    # load functions 

    
    # Gerando todas as abas do sidebar
    output$renderSidebar <- renderMenu({
        
        
        sidebarMenu(
            
            id = "sidebar",
            
            #menuItem("Tutorial", tabName = "tutorial", icon = icon("home")),
            
            
            pickerInput(
                inputId = "indicadores",
                label   =  "Selecione:",
                choices = indicadores_list,
                options = list(`live-search` = TRUE, `actions-box` = TRUE)
            ),
            
            # Análises
            menuItem(
                "Análise Exploratória", icon = icon("chart-line"), #startExpanded = TRUE, 
                menuSubItem('Comparação de indicadores', tabName = "analises"),
                menuSubItem('Séries temporais', tabName = "ser_temp")
                
            ),
            
            # # Dropdown de Y's
            # hidden(
            #     pickerInput(
            #         inputId = "indicadores",
            #         label   =  "Selecione:",
            #         choices = lista_y,
            #         options = list(`live-search` = TRUE, `actions-box` = TRUE)
            #     )
            # ), 
            
            # Simulador + Ajuste Manual
            menuItem(
                'Explorando os modelos', icon = icon('fas fa-medal'),
                
                menuSubItem("ARIMA", tabName = "arima"), 
                menuSubItem("Árvore de Decisão", tabName = "arv_decisao")
            ),
            
            # # Botão para ajustes escondido
            # hidden(actionButton("btn_sidebar", "")),
            
            tags$style(".at_dash{position:absolute;bottom:0px; margin:5px}"),
            div(class='at_dash',
                span("Dados gerados em:", atualization_date_input)
            )
            
        )
        
    })
    
    # callModule(mod_tutorial,  'tutorial')
    callModule(mod_analises_server,  'analises', reactive(input$indicadores))
    callModule(mod_previsoes_server,  'analises1')
    

    # Loading... ----
    waiter_hide()
    
}

shinyApp(ui, server)

