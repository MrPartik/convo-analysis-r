library(dplyr)
library(readr)
library(jsonlite)
library(plotly)
library(DT)
defaultUrl <- if(Sys.getenv('DATA-SOURCE-URL') == "") "http://127.0.0.1:8000/get/data-source" else Sys.getenv('DATA-SOURCE-URL')
server <- function(input, output, session) {
    dataSource <- fromJSON(paste(defaultUrl, isolate(session$clientData$url_search), sep = ""))
    total <- dataSource$data_source
    chart <- dataSource$chart
    session$sendCustomMessage("changetitle", dataSource$title)
    output$datatable <- DT::renderDataTable({
        DT::datatable(total, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    categories <- unique(total$year)
    sub_categories <- unique(total$region)
    ids <- unique(total$hei)
    # These reactive values keep track of the drilldown state
    # (NULL means inactive)
    drills <- reactiveValues(
        year = NULL,
        region = NULL,
        hei = NULL,
        category = NULL,
        program = NULL
    )
    # filter the data based on active drill-downs
    # also create a column, value, which keeps track of which
    # variable we're interested in
    datasource <- reactive({
        if (!length(drills$year)) {
            return(mutate(total, value = year))
        }
        total <- filter(total, year %in% drills$year)
        if (!length(drills$region)) {
            return(mutate(total, value = region))
        }
        total <- filter(total, region %in% drills$region)
        if (!length(drills$hei)) {
            return(mutate(total, value = hei))
        }
        total <- filter(total, hei %in% drills$hei)
        if (!length(drills$category)) {
            return(mutate(total, value = category))
        }
        total <- filter(total, category %in% drills$category)
        mutate(total, value = program)
    })
    
    # bar chart of total by 'current level of year'
    output$bars <- renderPlotly({
        d <- count(datasource(), value, wt = total)
        
        p <- plot_ly(d, x = ~value, y = ~n, source = "bars") %>%
            layout(
                yaxis = list(title = "Total"),
                xaxis = list(title = "")
            )
        add_bars(p, color = ~value, showlegend = FALSE)
    })
    
    # control the state of the drilldown by clicking the bar graph
    observeEvent(event_data("plotly_click", source = "bars"), {
        x <- event_data("plotly_click", source = "bars")$x
        if (!length(x)) return()
        
        if (!length(drills$year)) {
            drills$year <- x
        } else if (!length(drills$region)) {
            drills$region <- x
        } else if (!length(drills$hei)) {
            drills$hei <- x
        } else if (!length(drills$category)) {
            drills$category <- x
        }
    })
    
    # populate a `selectInput()` for each active drilldown
    output$history <- renderUI({
        if (!length(drills$year))
            return("Click the bar chart to drilldown")
        
        yearInput <- selectInput(
            "year", "Academic Year",
            multiple = FALSE,
            selectize = FALSE,
            choices = categories, selected = drills$year
        )
        if (!length(drills$region)) return(yearInput)
        sd <- filter(total, year %in% drills$year)
        regionInput <- selectInput(
            "region", "Regions",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$region),
            selected = drills$region
        )
        if (!length(drills$hei)) {
            return(fluidRow(
                column(3, yearInput),
                column(3, regionInput)
            ))
        }
        sd <- filter(sd, region %in% drills$region)
        heiInput <- selectInput(
            "hei", "Institutions",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$hei),
            selected = drills$hei
        )
        if (!length(drills$category)) {
            return(fluidRow(
                column(3, yearInput),
                column(3, regionInput),
                column(3, heiInput)
            ))
        }
        sd <- filter(sd, hei %in% drills$hei)
        categoryInput <- selectInput(
            "category", "Program Categories",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$category),
            selected = drills$category
        )
        if (!length(drills$program)) {
            return(fluidRow(
                column(3, yearInput),
                column(3, regionInput),
                column(3, heiInput),
                column(3, categoryInput)
            ))
        }
        fluidRow(
            column(3, yearInput),
            column(3, regionInput),
            column(3, heiInput),
            column(3, categoryInput)
        )
    })
    
    output$back <- renderUI({
        if (!is.null(drills$year)) 
            actionButton("clear", "Back", icon("chevron-left"))
    })
    
    observeEvent(input$clear, {
        if(!is.null(drills$year) && is.null(drills$region)) {  
            drills$year <- NULL
        }else if(!is.null(drills$region) && is.null(drills$hei)) { 
            drills$region <- NULL
        } else if(!is.null(drills$hei) && is.null(drills$category)) {
            drills$hei <- NULL
        } else if(!is.null(drills$category) && is.null(drills$program)) {
            drills$category <- NULL
        } else if(!is.null(drills$program)) { 
            drills$program <- NULL
        }
    })
    
    # control the state of the drilldown via the `selectInput()`s
    observeEvent(input$year, {
        drills$year <- input$year
        drills$region <- NULL
        drills$hei <- NULL
        drills$category <- NULL
        drills$program <- NULL
    })
    observeEvent(input$region, {
        drills$region <- input$region
        drills$hei <- NULL
        drills$category <- NULL
        drills$program <- NULL
    })
    observeEvent(input$hei, {
        drills$hei <- input$hei
        drills$category <- NULL
        drills$program <- NULL
    })
    observeEvent(input$category, {
        drills$category <- input$category
        drills$program <- NULL
    })
    observeEvent(input$program, {
        drills$program <- input$program
    })
}
