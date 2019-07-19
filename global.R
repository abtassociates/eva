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

QPR_EEs <- QPR_EEs %>% arrange(ProjectName)

filebeginningdate <- updatedate - years(2)



