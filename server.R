library(dplyr)
library(readr)
library(jsonlite)
library(plotly)
library(DT)
defaultUrl <- if(Sys.getenv('DATA-SOURCE-URL') == "") "http://brixbo.com/get/data-source" else Sys.getenv('DATA-SOURCE-URL')
server <- function(input, output, session) {
    dataSource <- fromJSON(paste(defaultUrl, isolate(session$clientData$url_search), sep = ""))
    total <- dataSource$data_source
    chart <- dataSource$chart
    intent <- dataSource$getType
    session$sendCustomMessage("changetitle", dataSource$title)
    output$datatable <- DT::renderDataTable({
        DT::datatable(total, options = list(lengthMenu = c(10, 20, 30, 40, 50, 100), pageLength = 20), selection = "none")
    })
    
    # These reactive values keep track of the drilldown state
    # (NULL means inactive)
    drills <- reactiveValues(
        year = NULL,
        type = NULL,
        region = NULL,
        hei = NULL,
        category = NULL,
        program = NULL,
        gender = NULL
    )
    # filter the data based on active drill-downs
    # also create a column, value, which keeps track of which
    # variable we're interested in
    datasource <- reactive({
        if (!length(drills$year)) {
            return(mutate(total, value = year))
        }
        total <- filter(total, year %in% drills$year)
        if (!length(drills$type)) {
            return(mutate(total, value = type))
        }
        total <- filter(total, type %in% drills$type)
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
        if (!length(drills$program)) {
            print(mutate(total, value = program))
            return(mutate(total, value = program))
        }
        total <- filter(total, program %in% drills$program)
        total <- c(sum(as.integer(total$male)), sum(as.integer(total$female)))
        value <- c("Male", "Female")
        gender <- c("Male", "Female")
        
        return (data.frame(gender, value, total))
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
        print(x)
        if (!length(drills$year)) {
            drills$year <- x
        } else if (!length(drills$type)) {
            drills$type <- x
        } else if (!length(drills$region)) {
            drills$region <- x
        } else if (!length(drills$hei)) {
            drills$hei <- x
        } else if (!length(drills$category)) {
            drills$category <- x
        } else if (!length(drills$program)) {
            drills$program <- x
        } else {
            drills$gender <- x
        }
    })
    
    # populate a `selectInput()` for each active drilldown
    output$history <- renderUI({
        if (!length(drills$year))
            return("Click the bar chart to drilldown")
        
        sd <- filter(total, year %in% drills$year)
        yearInput <- selectInput(
            "year", "Academic Year",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$year), selected = drills$year
        )
        if (!length(drills$type)) 
            return(yearInput)
        
        typeInput <- selectInput(
            "type", "From Data",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$type),
            selected = drills$type
        )
        if (!length(drills$region)) {
            return(fluidRow(
                column(3, yearInput),
                column(3, typeInput)
            ))
        }
        
        sd <- filter(sd, type %in% drills$type)
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
                column(3, typeInput),
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
                column(3, typeInput),
                column(3, regionInput),
                column(3, heiInput)
            ))
        }
        
        sd <- filter(sd, hei %in% drills$hei)
        categoryInput <- selectInput(
            "category", "Program Category",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$category),
            selected = drills$category
        )
        
        if (!length(drills$program)) {
            return(fluidRow(
                column(3, yearInput),
                column(3, typeInput),
                column(3, regionInput),
                column(3, heiInput),
                column(3, categoryInput)
            ))
        }
        
        sd <- filter(sd, category %in% drills$category)
        programInput <- selectInput(
            "program", "Programs",
            multiple = FALSE,
            selectize = FALSE,
            choices = unique(sd$program),
            selected = drills$program
        )
        fluidRow(
            column(3, yearInput),
            column(3, typeInput),
            column(3, regionInput),
            column(3, heiInput),
            column(3, categoryInput),
            column(3, programInput)
        )
    })
    
    output$back <- renderUI({
        if (!is.null(drills$year)) 
            actionButton("clear", "Back", icon("chevron-left"))
    })
    
    observeEvent(input$clear, {
        
        if(!is.null(drills$year) && is.null(drills$type)) {  
            drills$year <- NULL
        }else if(!is.null(drills$type) && is.null(drills$region)) {
            drills$type <- NULL
        } else if(!is.null(drills$region) && is.null(drills$hei)) { 
            drills$region <- NULL
        } else if(!is.null(drills$hei) && is.null(drills$category)) {
            drills$hei <- NULL
        } else if(!is.null(drills$category) && is.null(drills$program)) {
            drills$category <- NULL
        } else if(!is.null(drills$program) && is.null(drills$gender)) { 
            drills$program <- NULL
        }else if(!is.null(drills$gender)) { 
            drills$gender <- NULL
        }
    })
    
    # control the state of the drilldown via the `selectInput()`s
    observeEvent(input$year, {
        drills$year <- input$year
        drills$type <- NULL
        drills$region <- NULL
        drills$hei <- NULL
        drills$category <- NULL
        drills$program <- NULL
        drills$gender <- NULL
    })
    
    observeEvent(input$type, {
        drills$type <- input$type
        drills$region <- NULL
        drills$hei <- NULL
        drills$category <- NULL
        drills$program <- NULL
        drills$gender <- NULL
    })
    observeEvent(input$region, {
        drills$region <- input$region
        drills$hei <- NULL
        drills$category <- NULL
        drills$program <- NULL
        drills$gender <- NULL
    })
    observeEvent(input$hei, {
        drills$hei <- input$hei
        drills$category <- NULL
        drills$program <- NULL
        drills$gender <- NULL
    })
    observeEvent(input$category, {
        drills$category <- input$category
        drills$program <- NULL
        drills$gender <- NULL
    })
    observeEvent(input$program, {
        drills$program <- input$program
        drills$gender <- NULL
    })
    observeEvent(input$gender, {
        drills$gender <- input$gender
    })
}
