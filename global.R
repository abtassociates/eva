# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
#<https://www.gnu.org/licenses/>.

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

load("data/Data_Quality.RData")

QPR_EEs <- QPR_EEs %>% arrange(ProjectName)

providers <- sort(validation$ProjectName) %>% unique() 

filebeginningdate <- update_date - years(2)

# HOW TO SET UP YOUR SYMBOLIC LINKS ON YOUR SYSTEM (Windows-specific)
# command prompt As Administrator, then:
# mklink "C:\Users\HMIS1\Documents\R\Rminor\data\Data_Quality.RData" 
# "C:\Users\HMIS1\Documents\R\COHHIO_HMIS\images\Data_Quality.RData"
# obviously replace these paths with what's on your PC. It's basically:
# mklink "where you want to add the link" "where you want the link to point"


