library(shiny)
library(plotly)
library(shinycssloaders)
ui <- fluidPage(
    titlePanel(' '),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Chart", 
          fluidPage(
            uiOutput("history"),
            uiOutput("back"),
            plotlyOutput("bars", height = "auto") %>% withSpinner(color="#0dc5c1"),
            tags$script(HTML('Shiny.addCustomMessageHandler("changetitle", function(x) {$(".container-fluid > h2").text(x)});'))
          )
        ),
        tabPanel("Data Table", DT::dataTableOutput("datatable") %>% withSpinner(color="#0dc5c1"))
      ) %>% withSpinner(color="#0dc5c1"),
      width = '100%'
    ),
    bootstrap = TRUE
)