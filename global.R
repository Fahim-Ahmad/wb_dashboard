# Loading the required packages
library(fresh)
library(shiny)
library(highcharter)
library(shinydashboard)
library(shinyWidgets)
library(maps)
library(tidyverse)
library(data.table)

# set the data path
path <- "input/data/"

# vector to used as choices in the selectInput() in the UI part of the dashboard
ind_vec <- c(
  # "Please select the indicator" = "",
  "Adjusted net savings" = "adjusted_net_savings",
  "GDP (current US$)" = "gdp_current_us",
  "GDP growth (annual %)" = "gdp_growth",
  "Gross savings (% of GDP)" = "gross_savings",
  "Central government debt, total (% of GDP)" = "central_government_debt"
)

# custom theme
custom_theme <- fresh::create_theme(
  bs_vars_navbar(
    padding_horizontal = "15px",
    default_bg = "#1cabe2",
    default_link_color = "#FFFFFF"
  ))


