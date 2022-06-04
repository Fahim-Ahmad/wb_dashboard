# execute the codes from global.R to load packages, set data path, etc. ---------------------------------------
# file.edit("global.R")
source("global.R")

# ui part of the dashboard ---------------------------------------
ui <- shinyUI(
    bootstrapPage(
        navbarPage("WB Dashboard - A dashboard developed in R-shiny to visualize the World Bank data", header = tagList(use_theme(custom_theme))),
        fluidRow(style = "height:100%",
                 shiny.info::busy(
                     loader = "dots",
                     position = "bottom right"
                     ),
                 column(width = 8,
                        selectInput("ind", label = "", choices = indicators, selected = "Adjusted net savings, including particulate emission damage (% of GNI)", width = "100%"),
                        # conditionalPanel("input.ind != ''", uiOutput("slider")),
                        uiOutput("slider"),
                        tabsetPanel(
                            tabPanel("Map",
                                     # conditionalPanel("input.ind != ''", highchartOutput("map", height = "400px"))
                                     highchartOutput("map", height = "400px")
                                     ),
                            tabPanel("Treemap",
                                     highchartOutput("treemap", height = "400px")
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
                        downloadLink("download_wide", label = HTML("<b>Download data in wide format</b>")),
                        HTML("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;"),
                        downloadLink("download_long", label = HTML("<b>Download data in long format</b>")),
                        br(),
                        highchartOutput("world")
                        )
                 ),
        fluidRow(
            column(width = 12,
                   # conditionalPanel("input.ind != ''", uiOutput("filter_by_country")),
                   uiOutput("filter_by_country"),
                   helpText("Select more than one country for comparison."),
                   # conditionalPanel("input.ind != ''", highchartOutput("barchart", height = "250px"))
                   highchartOutput("bar_line_chart", height = "250px")
                   )
            )
        )
    )

# server part of the dashboard ---------------------------------------
server <- function(input, output, session) {
    
    file_name <- reactive({
        dt_info %>% 
            filter(INDICATOR_NAME == input$ind) %>% 
            pull(file_name)
    })
    
    data_wide <- reactive({
        # if (input$ind != '') {
            dt <- readxl::read_excel(paste0(path, file_name(), ".xls"), skip = 3, sheet = "Data", guess_max = 1000)
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
    
    caption <- reactive({
        # if (input$ind != '') {
            dt_info %>% 
                filter(INDICATOR_NAME == input$ind) %>% 
                pull(SOURCE_NOTE) %>% unique()
        # }
    })
    
    last_update <- reactive({
        # if (input$ind != '') {
        dt_info %>% 
            filter(INDICATOR_NAME == input$ind) %>% 
            pull(last_update) %>% unique()
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
                hc_colorAxis(stops = color_stops(n = 20, colors = c("#440154", "skyblue", "blue"))) %>% 
                hc_title(text = input$ind) %>%
                hc_subtitle(text = paste0("Last Updated Date: ", last_update())) %>% 
                hc_caption(text = caption()) %>% 
                hc_exporting(enabled = TRUE, filename = "map")
        # }
    })
    
    output$treemap <- renderHighchart({
        map_data() %>% 
            hchart('treemap', hcaes(name = `iso-a3`, x = year, value = value, color = value)) %>% 
            hc_colorAxis(stops = color_stops(colors = viridis::inferno(n = 10))) %>% 
            hc_title(text = input$ind) %>%
            hc_subtitle(text = paste0("Last Updated Date: ", last_update())) %>% 
            hc_caption(text = caption()) %>% 
            hc_exporting(enabled = TRUE, filename = "treemap")
    })
    
    output$filter_by_country <- renderUI({
        # if (input$ind != '') {
        
        dt_long <- left_join(data_long(), select(iso3166, a2, `Country Code` = a3), by = "Country Code") %>% mutate(a2 = tolower(a2))
        countries <- dt_long %>% filter(!is.na(a2)) %>% pull(`Country Name`) %>% unique()
        
        flags <- paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
                        dt_long %>% 
                            filter(!is.na(a2)) %>% 
                            pull(a2) %>% unique(),
                        ".svg"
        )
        
        pickerInput(
            inputId = "country",
            label = NULL,
            choices = countries,
            multiple = T,
            selected = "Germany",
            choicesOpt = list(content =
                                  mapply(countries, flags, FUN = function(country, flagUrl) {
                                      HTML(paste(tags$img(src=flagUrl, width=20, height=15),country))
                                      }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                              ),
            options = list(style = "btn-primary", size = 5,
                           `live-search`=TRUE
                           )
            )
        # }
    })
    
    output$bar_line_chart <- renderHighchart({
        # if (input$ind != '') {
            if(length(input$country) == 1) {
                data_long() %>% 
                    filter(`Country Name` == input$country) %>% 
                    filter(!is.na(value)) %>% 
                    hchart('column', hcaes(x = year, y = value)) %>% 
                    hc_exporting(enabled = TRUE, filename = "barchart")
            } else {
                data_long() %>% 
                    filter(`Country Name` %in% input$country) %>% 
                    filter(!is.na(value)) %>% 
                    hchart('line', hcaes(x = year, y = value, group = `Country Name`)) %>% 
                    hc_exporting(enabled = TRUE, filename = "country_comparison_linechar")
            }
        # }
    })
    
    output$table <- DT::renderDataTable(
        data_long() %>%
            rename(country = `Country Name`) %>% 
            mutate(value = round(value, 2)),
        options = list(pageLength = 100, lengthChange = FALSE, autoWidth = TRUE),
        rownames= FALSE
    )
    
    world_data <- reactive({
        data_long() %>% 
            filter(`Country Name` == "World") %>% 
            filter(!is.na(value)) %>% 
            select(year, value) %>% 
            mutate(year = as.Date(ISOdate(year, 1, 1)))
    })
    
    output$world <- renderHighchart({
        if (nrow(world_data())>0) {
            highchart(type = "chart") %>% 
                hc_add_series(world_data() %>% 
                                  as.data.table() %>%
                                  as.xts.data.table(),
                              id = "value",
                              showInLegend = F
                ) %>% 
                hc_xAxis(type = "datetime") %>% 
                hc_subtitle(text = "World") %>% 
                hc_exporting(enabled = TRUE, filename = "linechart")
        }
    })
    
    output$download_wide <- downloadHandler(
        filename = function() {
            paste(input$ind,"_wide format_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data_wide(), file, row.names = F)
        }
    )
    
    output$download_long <- downloadHandler(
        filename = function() {
            paste(input$ind,"_long format_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data_long(), file, row.names = F)
        }
    )
    
    # observe({
    showModal(
        modalDialog(
            HTML(paste0("Hello!<p> This dashboard aims to visualize the <u><a href = 'https://data.worldbank.org/indicator'> World Bank</a></u> (WB) data.<p>
                     Currently, it includes <b>", n_files, "</b> economy and growth related indicators and I plan to add more indicators in the future.")),
            footer = "",
            size = "m",
            easyClose = TRUE
        )
    )
    # })

}

shinyApp(ui, server)
