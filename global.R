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
Sys.setenv(TZ = "America/New_York")

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
library(glue)

load("images/Data_Quality.RData")

options(shiny.maxRequestSize = 200000000)

projects <- sort(validation$ProjectName) %>% unique()

desk_time_providers <- validation %>%
  dplyr::filter(
    (entered_between(., today() - years(1), today()) |
       exited_between(., today() - years(1), today())) &
      ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
  dplyr::select(ProjectName) %>% unique()
