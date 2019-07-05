library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)
library(scales)
library(zoo)

load("data/Utilization.RData")

load("data/QPR_SPDATs.RData")

load("data/QPR_EEs.RData")

QPR_EEs <- QPR_EEs %>% arrange(ProjectName)

filebeginningdate <- updatedate - years(2)



