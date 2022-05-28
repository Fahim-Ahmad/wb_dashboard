# execute the codes from global.R to load packages, set data path, etc. ---------------------------------------
# file.edit("global.R")
source("global.R")

# ui part of the dashboard ---------------------------------------
ui <- shinyUI(
    bootstrapPage(
        navbarPage("WB Dashboard - A dashboard developed in R-shiny to visualize the World Bank data", header = tagList(use_theme(custom_theme))),
        fluidRow(style = "height:100%",
                 column(width = 8,
                        selectInput("ind", label = "", choices = ind_vec, selected = ind_vec[1], width = "100%"),
                        # conditionalPanel("input.ind != ''", uiOutput("slider")),
                        uiOutput("slider"),
                        tabsetPanel(
                            tabPanel("Map",
                                     # conditionalPanel("input.ind != ''", highchartOutput("map", height = "400px"))
                                     highchartOutput("map", height = "400px")
                                     ),
                            tabPanel("Table",
                                     box(style='width:850px; overflow-x: scroll; height:400px;overflow-y: scroll;',
                                         DT::dataTableOutput("table")
                                         )
                                     )
                            )
                        ),
                 column(width = 4,
                        br(),
                        dropdownButton(
                            HTML("<p><b>Developed by:</b> <a href='https://fahimahmad.netlify.app'> Fahim Ahmad</a></p>"),
                            HTML("<p><b>Source code:</b> <a href = 'https://github.com/Fahim-Ahmad/wb_dashboard'> GitHub</a></p>"),
                            HTML("<p>Please feel free to comment for improvement, report typos, or raise other concerns <a href = 'https://github.com/Fahim-Ahmad/wb_dashboard/issues'>here.</a></p>"),
                            
                            circle = TRUE,
                            status = "danger",
                            icon = icon("gear"),
                            tooltip = tooltipOptions(title = "Click to see app info!"),
                            width = "400px",
                            inputId = "app_info"
                        ),
                        br(),br(),br(),br(),br(),br(),br(),
                        downloadLink("download_wide", label = "Download data in wide format"),
                        HTML("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"),
                        downloadLink("download_long", label = "Download data in long format"),
                        br(),
                        highchartOutput("world")
                        )
                 ),
        fluidRow(
            column(width = 12,
                   # conditionalPanel("input.ind != ''", uiOutput("filter_by_country")),
                   uiOutput("filter_by_country"),
                   # conditionalPanel("input.ind != ''", highchartOutput("barchart", height = "250px"))
                   highchartOutput("barchart", height = "250px")
                   )
            )
        )
    )

# server part of the dashboard ---------------------------------------
server <- function(input, output, session) {
    
    data_wide <- reactive({
        # if (input$ind != '') {
            dt <- readxl::read_excel(paste0(path, input$ind, ".xls"), skip = 3, sheet = "Data", guess_max = 1000)
        # }
    })
    
    data_long <- reactive({
        # if (input$ind != '') {
            data_wide() %>% 
                pivot_longer(-c(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`), names_to = "year") %>% 
                filter(!is.na(value)) %>% 
                mutate(year = as.numeric(year))
        # }
    })
    
    output$slider <- renderUI({
        # if (input$ind != '') {
            sliderInput("year", label = "", min = min(data_long()$year), max = max(data_long()$year), value = max(data_long()$year), step = 1, animate = TRUE, width = "40%") 
        # }
    })
    
    title <- reactive({
        # if (input$ind != '') {
            readxl::read_excel(paste0(path, input$ind, ".xls"), sheet = "Metadata - Indicators") %>% pull(INDICATOR_NAME) 
        # }
    })
    
    caption <- reactive({
        # if (input$ind != '') {
            readxl::read_excel(paste0(path, input$ind, ".xls"), sheet = "Metadata - Indicators") %>% pull(SOURCE_NOTE) 
        # }
    })
    
    map_data <- reactive({
        # if (input$ind != '') {
            left_join(
                iso3166,
                data_long() %>% filter(year == input$year),
                by = c("a3" = "Country Code")
            ) %>% 
                rename("iso-a3" = "a3")
        # }
    })
    
    output$map <- renderHighchart({
        # if (input$ind != '') {
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
        # }
    })
    
    output$filter_by_country <- renderUI({
        # if (input$ind != '') {
            pickerInput(
                inputId = "country",
                label = "", 
                choices = unique(data_long()$`Country Name`)[unique(data_long()$`Country Name`) != "World"],
                selected = "Germany",
                options = list(style = "btn-primary", size = 5)
            )
        # }
    })
    
    output$barchart <- renderHighchart({
        # if (input$ind != '') {
            data_long() %>% 
                filter(`Country Name` == input$country) %>% 
                filter(!is.na(value)) %>% 
                hchart('column', hcaes(x = year, y = value))
        # }
    })
    
    output$table <- DT::renderDataTable(
        data_long() %>%
            rename(country = `Country Name`) %>% 
            mutate(value = round(value, 2)),
        options = list(pageLength = 100, lengthChange = FALSE, autoWidth = TRUE),
        rownames= FALSE
    )
    
    output$world <- renderHighchart({
        # if (input$ind != '') {
            highchart(type = "chart") %>% 
                hc_add_series(data_long() %>% 
                                  filter(`Country Name` == "World") %>% 
                                  filter(!is.na(value)) %>% 
                                  select(year, value) %>% 
                                  mutate(year = as.Date(ISOdate(year, 1, 1))) %>% 
                                  as.data.table() %>%
                                  as.xts.data.table(),
                              id = "value",
                              showInLegend = F
                ) %>% 
                hc_xAxis(type = "datetime") %>% 
                hc_subtitle(text = "World")
        # }
    })
    
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
