
Sys.setenv(TZ = "America/New_York")

library(tidyverse)
library(lubridate)
library(shiny)
library(bslib)
library(bsicons)
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

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2

if(dir.exists(here("metadata-analysis/metadata/"))) {
  capture.output("All good", file = stderr())
} else {
  dir.create(here("metadata-analysis/metadata/"))
}
source(here("hardcodes.R"), local = TRUE) # hard-coded variables and data frames

source(here('tab_instructions.R'), local = TRUE) # static HTML text elements

# # functions used throughout the app
# source("helper_functions.R", local = TRUE)

# runApp(display.mode = "showcase")

bslib_eva_theme <- bs_theme(
  version = 5,
  brand = TRUE,
  "card-bg" = "white",
  "navbar-bg" = "#16697A",
  "accordion-bg" = "white",
  "sidebar-bg" = "white",
  "dropdown-bg" = "white",
  "nav-underline-link-active-color" = "#16697A",
  "modal-content-bg" = "white",
  #bg = '#ecf0f5',fg='black',
  font_scale = 0.875 # units are rem, 1rem = 16px
)