# Loading the required packages -----------------------------------------
if(!require(fresh)) install.packages("fresh")
if(!require(shiny)) install.packages("shiny")
if(!require(highcharter)) install.packages("highcharter")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(maps)) install.packages("maps")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table")
if(!require(shiny.info)) install.packages("shiny.info")

# store the information of each data file to use as vector of indicators, subtitle for the map, etc. -----------------------------------------
path <- "input/data/"
files <- list.files(path, pattern = ".xls")
date_info <- dt_info <- list()

for (file in files) {
  dt_info[[str_remove(file, ".xls")]] <- readxl::read_excel(paste0(path, file), sheet = "Metadata - Indicators")
  date_info[[str_remove(file, ".xls")]] <- readxl::read_excel(paste0(path, file), sheet = "Data", n_max = 2) %>% select(last_update = 2)
}
dt_info <- data.table::rbindlist(dt_info, idcol = "file_name")
date_info <- data.table::rbindlist(date_info, idcol = "file_name")
dt_info <- cbind(dt_info, date_info[, 2]) %>% mutate(last_update = as.character(last_update))

indicators <- unique(dt_info$INDICATOR_NAME)
n_files <- length(files)

# custom theme -----------------------------------------
custom_theme <- fresh::create_theme(
  bs_vars_navbar(
    padding_horizontal = "15px",
    default_bg = "#1cabe2",
    default_link_color = "#FFFFFF"
  ))


