
Sys.setenv(TZ = "America/New_York")

library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(scales)
library(plotly)
library(DT)
library(writexl)
library(viridis)
library(HMIS)
library(glue)
library(janitor)
library(shinyjs)
library(shinydisconnect)
#library(shinyHeatmap)
#library(shinylogs)

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2

if(dir.exists("www/metadata/")) {
  cat("All good")
} else {
  dir.create("www/metadata/")
}
s

