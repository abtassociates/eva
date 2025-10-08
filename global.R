
Sys.setenv(TZ = "America/New_York")

library(tidyverse) # loads ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate
library(shiny)
library(bslib) # bslib: bootstrap UI and theming
library(bsicons) # bsicons: bootstrap icons for tabs
library(shinyWidgets) # shinyWidgets: for System filters (i.e. pickerInputs)
library(scales) # scales: rendering percent labels in ggplot2
library(DT) # DT: interactive data tables
library(writexl) # writexl: for XLSX downloads
library(glue) # glue: string interpolation for cleaner dynamic strings
library(shinyjs) # shinyjs: javascript functionality
library(shinydisconnect)  # shinydisconnect: show message when session crashes
library(here) # here: specify directories relatively
library(shinycssloaders) # shinycssloaders: show loading animation while chart/table loads
library(data.table) # data.table: performance
library(ggalluvial) # ggalluvial: for System Status Sankey chart
library(treemapify) # treemapify: for System Exit Types chart
library(officer) # officer: construct PPTs for download
library(mirai) # mirai: asynch/parallel processing for performance
library(collapse) # collapse: performance
library(flextable) # flextable: tables in PPT. (DT does not work well with PPT)
library(promises) # promises: asynch/parallel processing for performance
library(ggarchery) # ggarchery: arrows for System Exit Comparison dotplots


options(shiny.maxRequestSize = 232783872) # was 190MB, is now 222 MB, aka 222*1024^2=210763776
options(shiny.fullstacktrace = TRUE)
options(shiny.stacktraceon = TRUE)
set_collapse(na.rm = TRUE, verbose = FALSE, sort = FALSE)

if(dir.exists(here("metadata-analysis/metadata/"))) {
  capture.output("All good", file = stderr())
} else {
  dir.create(here("metadata-analysis/metadata/"))
}
source(here("hardcodes.R")) # hard-coded variables and data frames
source(here("helper_functions.R")) # functions used throughout the app
source(here('tab_instructions.R'), local = TRUE) # static HTML text elements

# Asynchronous processing, using mirai, of DQ and PDDE to save time------
# for a single user and multiple users
# Create DQ and PDDE script environment
daemons(1, output = TRUE)
mirai::everywhere({
  library(data.table)
  library(tidyverse)
  library(collapse)
  library(here)
  
  options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2
  options(shiny.fullstacktrace = TRUE)
  options(shiny.stacktraceon = TRUE)
  
  source(here("hardcodes.R"))
  source(here("helper_functions.R"))
  set_collapse(na.rm = TRUE, verbose = FALSE) # suppress join printouts
})
onStop(function() daemons(0))

bslib_eva_theme <- bs_theme(
  version = 5,
  brand = TRUE,
  "card-bg" = "white",
  "navbar-bg" = "#16697A",
  "accordion-bg" = "white",
  "accordion-border-radius" = "8px",
  "accordion-inner-border-radius" = "8px",
  "sidebar-bg" = "white",
  "dropdown-bg" = "white",
  "input-bg" = "white",
  "nav-underline-link-active-color" = "#16697A",
  "modal-content-bg" = "white",
  #bg = '#ecf0f5',fg='black',
  font_scale = 0.875 # units are rem, 1rem = 16px
)
