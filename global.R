library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)
library(scales)
library(zoo)

load("data/Utilization.RData")

load("data/QPR_SPDATs.RData")

filebeginningdate <- updatedate - years(2)



