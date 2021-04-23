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
    library(ggcorrplot)
    library(corrplot)
    library(xgboost)
    library(randomForestExplainer)
    library(quantmod)
    library(timetk)
    library(xgboost)
})

options(warn = -1, scipen = 999)

# Source files
source('load_data/load_data.R')
atualization_date_input <- data_coleta
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

            tabItem(tabName = "detales_model", mod_detalhes_modelos_ui("detales_model")),
            
            tabItem(tabName = "previsao_arvore", mod_previsoes_arvore_ui("previsao_arvore")),
            
            tabItem(tabName = "previsao_rf", mod_previsoes_rf_ui("previsao_rf")),
            
            tabItem(tabName = "previsao_xgboost", mod_xgboost_ui("previsao_xgboost"))
            
            
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
            
            
            # pickerInput(
            #     inputId = "indicadores",
            #     label   =  "Selecione:",
            #     choices = indicadores_list,
            #     options = list(`live-search` = TRUE, `actions-box` = TRUE)
            # ),
            
            # Análises
            menuItem(
                "Análise Exploratória", icon = icon("chart-line"), #startExpanded = TRUE, 
                menuSubItem('Comparação de indicadores', tabName = "analises")
                
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
                'Explorando os modelos', #icon = icon('fas fa-medal'),
                
                menuSubItem("Detalhes dos Modelos", tabName = "detales_model"), 
                menuSubItem("Árvore de Decisão", tabName = "previsao_arvore"),
                menuSubItem("Random Forest", tabName = "previsao_rf"),
                menuSubItem("XGBOOST", tabName = "previsao_xgboost")
                
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
    callModule(mod_previsoes_arvore_server, 'previsao_arvore')
    callModule(mod_detalhes_modelos_server, 'detales_model')
    callModule(mod_previsoes_rf_server, 'previsao_rf')
    callModule(mod_xgboost_server, 'previsao_xgboost')
    

    # Loading... ----
    waiter_hide()
    
}

shinyApp(ui, server)

