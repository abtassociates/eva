# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
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

library(lubridate)
library(tidyverse)
library(here)

# hc = hard-coded here, used elsewhere
# meta = result comes from meta data
# calc = result is calculated

# Dates from Metadata -----------------------------------------------------
meta_HUDCSV_Export_Start <- Export() %>% pull(ExportStartDate)

meta_HUDCSV_Export_End <- Export() %>% pull(ExportEndDate)

meta_HUDCSV_Export_Date <- Export() %>% pull(ExportDate)


# Calculated Dates --------------------------------------------------------
calc_data_goes_back_to <- Exit() %>%
  # read_csv(here(paste0("data/", dataset_directory, "Exit.csv")),
  #          col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn") %>%
  arrange(ExitDate) %>%
  head(1) %>% 
  pull(ExitDate)

calc_full_date_range <- interval(meta_HUDCSV_Export_End, calc_data_goes_back_to)

FileEnd <- floor_date(today(), "month") - days(1)
FileStart <- floor_date(FileEnd, "month") - years(2) + months(1)
FilePeriod <- interval(FileStart,FileEnd)



# save(list = ls(), file = here("images/00_dates.RData"), compress = FALSE)
