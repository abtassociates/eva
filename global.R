
Sys.setenv(TZ = "America/New_York")
Sys.setenv(RENV_PATHS_CACHE = "/renv/library/R-4.2/x86_64-pc-linux-gnu/")

# add package path to renv library
.libPaths(c(.libPaths(), "/renv/library/R-4.2/x86_64-pc-linux-gnu/"))

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

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2

if(dir.exists("metadata-analysis/metadata/")) {
  capture.output("All good", file = stderr())
} else {
  dir.create("metadata-analysis/metadata/")
}
