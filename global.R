
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
library(ggplot2)
library(ggalluvial)
library(officer)
library(collapse)

options(shiny.maxRequestSize = 210763776) # was 190MB, is now 201 MB, aka 201*1024^2=210763776

if(dir.exists(here("metadata-analysis/metadata/"))) {
  capture.output("All good", file = stderr())
} else {
  dir.create(here("metadata-analysis/metadata/"))
}
source(here("hardcodes.R"), local = TRUE) # hard-coded variables and data frames

# # functions used throughout the app
# source("helper_functions.R", local = TRUE)

# runApp(display.mode = "showcase")

