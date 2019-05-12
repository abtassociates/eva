library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)
library(scales)
library(zoo)

updatedate <- file.info("data/Utilization.RData")$mtime

filebeginningdate <- 
  file.info("data/Utilization.RData")$mtime - years(2)

load("data/Utilization.RData")

load("data/QPR_SPDATs.RData")

choices_month <-
  format(seq.Date(
    from = as.Date(floor_date(today(), unit = "month") - years(2)),
    by = "month",
    length.out = 24
  ), "%b %Y")



