
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
library(HMIS) #github.com/kiadso/HMIS
library(glue)
library(janitor)
library(shinyjs)
library(shinydisconnect)
library(here)
#library(shinyHeatmap)
#library(shinylogs)

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2

if(dir.exists("www/metadata/")) {
  capture.output("All good",file=stderr())
} else {
  dir.create("www/metadata/")
}