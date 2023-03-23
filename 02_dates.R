
library(lubridate)
library(tidyverse)
library(here)

# meta = result comes directly from imported meta data
# calc = result is calculated

logToConsole("Running dates")

# Dates from Metadata -----------------------------------------------------
meta_HUDCSV_Export_Start <<- Export %>% pull(ExportStartDate)

meta_HUDCSV_Export_End <<- Export %>% pull(ExportEndDate)

meta_HUDCSV_Export_Date <<- Export %>% pull(ExportDate)

# Calculated Dates --------------------------------------------------------
calc_data_goes_back_to <- Exit %>%
  arrange(ExitDate) %>%
  head(1) %>% 
  pull(ExitDate)

calc_full_date_range <- interval(meta_HUDCSV_Export_End, calc_data_goes_back_to)
