# Loading the required packages
library(tidyverse)
library(data.table)
library(maps)
library(shiny)
library(shinydashboard)
library(highcharter)

# set the data path
path <- "input/data/"

# vector to used as choices in the selectInput() in the UI part of the dashboard
ind_vec <- c(
  "Please select the indicator" = "",
  "Adjusted net savings" = "adjusted_net_savings",
  "GDP (current US$)" = "gdp_current_us",
  "GDP growth (annual %)" = "gdp_growth",
  "Gross savings (% of GDP)" = "gross_savings",
  "Central government debt, total (% of GDP)" = "central_government_debt"
)

