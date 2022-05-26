# execute the codes from global.R to load packages, set data path, etc. ---------------------------------------
source("global.R")

# ui part of the dashboard ---------------------------------------
ui <- dashboardBody(
  tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
  splitLayout(cellWidths = paste0(rep(100/3, 3), "%"),
              NULL,
              div(selectInput("ind", label = "", choices = ind_vec)),
              NULL
              ),
  splitLayout(cellWidths = c("70%", "30%"),
              list(
                conditionalPanel("input.ind != ''",sliderInput("year", label = "", min = 1960, max = 2022, value = 2020, step = 1)),
                conditionalPanel("input.ind != ''", highchartOutput("map", height = "400px"))
                ),
              conditionalPanel("input.ind != ''",
                               list(
                                 box(style='width:400px;overflow-x: scroll; height:400px;overflow-y: scroll;',
                                     div(DT::dataTableOutput("table"))
                                     ),
                                 hr(),
                                 downloadLink("download_wide", label = "Get data in wide format"),
                                 HTML("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"),
                                 downloadLink("download_long", label = "Get data in long format")
                                 )
                               )
              ),
  hr(),
  conditionalPanel("input.ind != ''", uiOutput("filter_by_country")),
  conditionalPanel("input.ind != ''", highchartOutput("barchart", height = "250px")),
  br()
)

# server part of the dashboard ---------------------------------------
server <- function(input, output, session) {
  
  data_wide <- reactive({
    if (input$ind != '') {
      dt <- readxl::read_excel(paste0(path, input$ind, ".xls"), skip = 3, sheet = "Data", guess_max = 1000) 
    }
  })
  
  data_long <- reactive({
    if (input$ind != '') {
      data_wide() %>% 
        pivot_longer(-c(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`), names_to = "year") %>% 
        filter(!is.na(value)) %>% 
        mutate(year = as.numeric(year)) 
    }
  })
  
  title <- reactive({
    if (input$ind != '') {
      readxl::read_excel(paste0(path, input$ind, ".xls"), sheet = "Metadata - Indicators") %>% pull(INDICATOR_NAME) 
    }
  })
  
  caption <- reactive({
    if (input$ind != '') {
      readxl::read_excel(paste0(path, input$ind, ".xls"), sheet = "Metadata - Indicators") %>% pull(SOURCE_NOTE) 
    }
  })
  
  map_data <- reactive({
    if (input$ind != '') {
      left_join(
        iso3166,
        data_long() %>% filter(year == input$year),
        by = c("a3" = "Country Code")
      ) %>% 
        rename("iso-a3" = "a3") 
    }
  })
  
  output$map <- renderHighchart({
    if (input$ind != '') {
      hcmap(
        map = "custom/world-highres3",
        data = map_data(),
        joinBy = "iso-a3",
        value = "value",
        colorKey = "value",
        name = "Value",
        dataLabels = list(enabled = TRUE, format = "{point.iso-a3}"),
        borderColor = "#FAFAFA",
        borderWidth = 0.1,
        tooltip = list(
          valueDecimals = 1,
          pointFormat = "{point.name}:{point.value}"
        ),
        showInLegend = TRUE,
        nullColor = "gray",
        download_map_data = FALSE
      ) %>%
        hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = TRUE,
          enableDoubleClickZoom = TRUE
        ) %>% 
        hc_legend(
          align = "center",
          verticalAlign = "bottom",
          layout = "horizontal"
        ) %>%
        hc_title(text = title()) %>% 
        hc_caption(text = caption())
    }
  })
  
  output$filter_by_country <- renderUI({
    if (input$ind != '') {
      selectInput("country", label = "", choices = unique(data_long()$`Country Name`), selected = "World")
    }
  })
  
  output$barchart <- renderHighchart({
    if (input$ind != '') {
      data_long() %>% 
        filter(`Country Name` == input$country) %>% 
        filter(!is.na(value)) %>% 
        hchart('column', hcaes(x = year, y = value)) 
    }
  })
  
  output$table <- DT::renderDataTable(
    # if (input$ind != '') {
      data_long() %>%
        select(-c(`Country Code`, `Indicator Name`, `Indicator Code`)) %>% 
        rename(country = `Country Name`) %>% 
        mutate(value = round(value, 2)),
      options = list(pageLength = 100, lengthChange = FALSE, autoWidth = TRUE),
      rownames= FALSE

    # }
  )
  
  output$download_wide <- downloadHandler(
    filename = function() {
      paste(input$ind,"wide format", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(data_wide(), file, row.names = F)
    }
  )
  
  output$download_long <- downloadHandler(
    filename = function() {
      paste(input$ind,"long format", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(data_long(), file, row.names = F)
    }
  )

}

shinyApp(ui, server)
