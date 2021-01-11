library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(jsonlite)

ui <- fluidPage(
    uiOutput("history"),
    plotlyOutput("bars", height = 200)
    #,plotlyOutput("lines", height = 300)
)

server <- function(input, output, session) {
total <- fromJSON(paste("http://127.0.0.1:8000/get/sample", isolate(session$clientData$url_search), sep = ""))$data_source
categories <- unique(total$category)
sub_categories <- unique(total$sub_category)
ids <- unique(total$id)
    # These reactive values keep track of the drilldown state
    # (NULL means inactive)
    drills <- reactiveValues(
        category = NULL,
        sub_category = NULL,
        id = NULL
    )
    # filter the data based on active drill-downs
    # also create a column, value, which keeps track of which
    # variable we're interested in
    datasource <- reactive({
        if (!length(drills$category)) {
            return(mutate(total, value = category))
        }
        total <- filter(total, category %in% drills$category)
        if (!length(drills$sub_category)) {
            return(mutate(total, value = sub_category))
        }
        total <- filter(total, sub_category %in% drills$sub_category)
        mutate(total, value = id)
    })

    # bar chart of total by 'current level of category'
    output$bars <- renderPlotly({
        d <- count(datasource(), value, wt = total)

        p <- plot_ly(d, x = ~value, y = ~n, source = "bars") %>%
            layout(
                yaxis = list(title = "Total"),
                xaxis = list(title = "")
            )

        if (!length(drills$sub_category)) {
            add_bars(p, color = ~value)
        } else if (!length(drills$id)) {
            add_bars(p) %>%
                layout(
                    hovermode = "x",
                    xaxis = list(showticklabels = FALSE)
                )
        } else {
            # add a visual cue of which ID is selected
            add_bars(p) %>%
                filter(value %in% drills$id) %>%
                add_bars(color = I("black")) %>%
                layout(
                    hovermode = "x", xaxis = list(showticklabels = FALSE),
                    showlegend = FALSE, barmode = "overlay"
                )
        }
    })

    # time-series chart of the total
    output$lines <- renderPlotly({
        p <- if (!length(drills$sub_category)) {
            datasource() %>%
                count(year, value, wt = total) %>%
                plot_ly(x = ~year, y = ~n) %>%
                add_lines(color = ~value)
        } else if (!length(drills$id)) {
            datasource() %>%
                count(year, wt = total) %>%
                plot_ly(x = ~year, y = ~n) %>%
                add_lines()
        } else {
            datasource() %>%
                filter(id %in% drills$id) %>%
                select(-value) %>%
                plot_ly() %>%
                add_table()
        }
        p %>%
            layout(
                yaxis = list(title = "Total"),
                xaxis = list(title = "")
            )
    })

    # control the state of the drilldown by clicking the bar graph
    observeEvent(event_data("plotly_click", source = "bars"), {
        x <- event_data("plotly_click", source = "bars")$x
        if (!length(x)) return()

        if (!length(drills$category)) {
            drills$category <- x
        } else if (!length(drills$sub_category)) {
            drills$sub_category <- x
        } else {
            drills$id <- x
        }
    })

    # populate a `selectInput()` for each active drilldown
    output$history <- renderUI({
        if (!length(drills$category))
            return("Click the bar chart to drilldown")

        categoryInput <- selectInput(
            "category", "Category",
            choices = categories, selected = drills$category
        )
        if (!length(drills$sub_category)) return(categoryInput)
        sd <- filter(total, category %in% drills$category)
        subCategoryInput <- selectInput(
            "sub_category", "Sub-category",
            choices = unique(sd$sub_category),
            selected = drills$sub_category
        )
        if (!length(drills$id)) {
            return(fluidRow(
                column(3, categoryInput),
                column(3, subCategoryInput)
            ))
        }
        sd <- filter(sd, sub_category %in% drills$sub_category)
        idInput <- selectInput(
            "id", "Program",
            choices = unique(sd$id), selected = drills$id
        )
        fluidRow(
            column(3, categoryInput),
            column(3, subCategoryInput),
            column(3, idInput)
        )
    })

    # control the state of the drilldown via the `selectInput()`s
    observeEvent(input$category, {
        drills$category <- input$category
        drills$sub_category <- NULL
        drills$id <- NULL
    })
    observeEvent(input$sub_category, {
        drills$sub_category <- input$sub_category
        drills$id <- NULL
    })
    observeEvent(input$id, {
        drills$id <- input$id
    })
}

shinyApp(ui, server)
