
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
#library(shinyHeatmap)
#library(shinylogs)

options(shiny.maxRequestSize = 200000000) # <- about 200MB, aka 200*1024^2

if(dir.exists("www/metadata/")) {
  cat("All good")
} else {
  dir.create("www/metadata/")
}

hc_psh_started_collecting_move_in_date <- ymd("20171001")

changelog <- tribble(
  ~Date, ~Change,
  "12-29-2022", "Fixes GitHub issue 118. Eva was not checking that all needed csvs were
  in the export. Now it checks this and rejects the export if they are not there.",
  
  "12-29-2022", "Fixes GitHub issue 118. Eva was missing some instances where a date
  variable is of the wrong type (e.g. ymd_hms instead of ymd). Now it rejects
  exports if an important variable has the wrong date type.",  
  
  "12-29-2022", "Client Counts report: if a user makes the Report Date Range so
  that the Start > End, Eva now alerts the user in the data tables to check dates.",
  
  "12-29-2022", "Rewrote PDDE issues' Guidance so that it is general guidance,
  then added Details column to include IDs to help admins find specific issues."

)
