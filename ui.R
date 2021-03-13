library(shiny)
library(plotly)
library(shinycssloaders)
ui <- fluidPage(
    titlePanel(' '),
    bootstrap = TRUE,
    uiOutput("history"),
    plotlyOutput("bars", height = "auto")  %>% withSpinner(color="#0dc5c1"),
    tags$script(HTML('Shiny.addCustomMessageHandler("changetitle", function(x) {$(".container-fluid > h2").text(x)});'))
)