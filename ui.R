library(shiny)
library(plotly)
library(shinycssloaders)
ui <- fluidPage(
  titlePanel("Enrolment by Academic Year"),
  bootstrap = TRUE,
    uiOutput("history"),
    plotlyOutput("bars", height = "auto")  %>% withSpinner(color="#0dc5c1")
)