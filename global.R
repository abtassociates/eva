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

filebeginningdate <- update_date - years(2)

# HOW TO SET UP YOUR SYMBOLIC LINKS ON YOUR SYSTEM (Windows-specific)
# command prompt As Administrator, then:
# mklink "C:\Users\HMIS1\Documents\R\Rminor\data\Data_Quality.RData" "C:\Users\
# HMIS1\Documents\R\COHHIO_HMIS\images\Data_Quality.RData"
# obviously replace these paths with what's on your PC. It's basically:
# mklink "where you want to add the link" "where you want the link to point"

# or check out the create_sym_links.R script in the COHHIO_HMIS repo!


