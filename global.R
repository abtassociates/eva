
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
library(mirai)
library(plotly)
library(collapse)
library(flextable)
library(purrr)

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2
options(shiny.fullstacktrace = TRUE)
options(shiny.stacktraceon = TRUE)
set_collapse(na.rm = TRUE, verbose = FALSE)

if(dir.exists(here("metadata-analysis/metadata/"))) {
  capture.output("All good", file = stderr())
} else {
  dir.create(here("metadata-analysis/metadata/"))
}
source(here("hardcodes.R")) # hard-coded variables and data frames
source(here("helper_functions.R")) # functions used throughout the app

# Asynchronous processing, using mirai, of DQ and PDDE to save time------
# for a single user and multiple users
# Create DQ and PDDE script environment
daemons(1, output = TRUE)
mirai::everywhere({
  library(data.table)
  library(tidyverse)
  library(collapse)
  library(here)
  source(here("hardcodes.R"))
  source(here("helper_functions.R"))
  set_collapse(na.rm = TRUE, verbose = FALSE) # suppress join printouts
})
onStop(function() daemons(0))

