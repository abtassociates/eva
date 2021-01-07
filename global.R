# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(scales)
library(plotly)
library(zoo)
library(DT)
library(writexl)
library(viridis)
library(HMIS)


load("data/Rminor_elevated.RData")

qpr_leavers <- qpr_leavers %>% arrange(ProjectName)

providers <- sort(validation$ProjectName) %>% unique() 

desk_time_providers <- validation %>%
  filter(entered_between(., 
                         format.Date(ymd(today() - years(1)), "%m-%d-%Y"), 
                         format.Date(ymd(today()), "%m-%d-%Y")) &
           ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13) &
           ProjectName != "Non-HMIS Shelter Clients")

dtproviders <- sort(desk_time_providers$ProjectName) %>% unique()

# filebeginningdate <- update_date - years(2)

tab_choices <- list(
  spdat1 = list(
    region_choices = c(unique(regions$RegionName[regions$County != "Mahoning"]))
  )
)


