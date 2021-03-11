library(shiny)
port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = 'localhost',
  port = as.numeric(port)
)