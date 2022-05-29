# Loading the required packages -----------------------------------------
library(fresh)
library(shiny)
library(highcharter)
library(shinydashboard)
library(shinyWidgets)
library(maps)
library(tidyverse)
library(data.table)

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

# custom theme -----------------------------------------
custom_theme <- fresh::create_theme(
  bs_vars_navbar(
    padding_horizontal = "15px",
    default_bg = "#1cabe2",
    default_link_color = "#FFFFFF"
  ))


