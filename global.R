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
dt_info <- list()

for (file in files) {
  dt_info[[str_remove(file, ".xls")]] <- readxl::read_excel(paste0(path, file), sheet = "Metadata - Indicators")
}
dt_info <- data.table::rbindlist(dt_info, idcol = "file_name")
indicators <- unique(dt_info$INDICATOR_NAME)

# custom theme -----------------------------------------
custom_theme <- fresh::create_theme(
  bs_vars_navbar(
    padding_horizontal = "15px",
    default_bg = "#1cabe2",
    default_link_color = "#FFFFFF"
  ))


