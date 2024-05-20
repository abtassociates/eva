
Sys.setenv(TZ = "America/New_York")

library(tidyverse)
library(lubridate)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(scales)
library(DT)
library(readxl)
library(writexl)
library(glue)
library(janitor)
library(shinyjs)
library(shinydisconnect)
library(here)
library(shinycssloaders)
library(dtplyr)
library(data.table)

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2

if(dir.exists("metadata-analysis/metadata/")) {
  capture.output("All good", file = stderr())
} else {
  dir.create("metadata-analysis/metadata/")
}
source("hardcodes.R", local = TRUE) # hard-coded variables and data frames