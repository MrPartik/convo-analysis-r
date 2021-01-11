library(shiny)
library(plotly)
ui <- fluidPage(
    uiOutput("history"),
    plotlyOutput("bars", height = 200)
)