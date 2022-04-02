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

# Hard-coded Dates --------------------------------------------------------

hc_check_dq_back_to <- mdy("10012019") # the default ReportStart for DQ reporting

hc_project_eval_start <- mdy("01012020")

hc_project_eval_end <- mdy("12312020")

hc_project_eval_docs_due <- mdy("04232021")

hc_bos_start_vaccine_data <- mdy("02052021")

hc_psh_started_collecting_move_in_date <- mdy("10012017") 

hc_began_collecting_covid_data <- mdy("04012020")

hc_outreach_to_cls <- mdy("10012019")

hc_began_requiring_spdats <- mdy("01012019")

hc_unsheltered_data_start <- mdy("01012019")

hc_prior_living_situation_required <- mdy("10012016")

hc_check_eligibility_back_to <- mdy("10012016")

hc_no_more_svcs_on_hh_members <- mdy("02012019")

hc_first_vaccine_administered_in_us <- mdy("12142020")

# Dates from Metadata -----------------------------------------------------

Export <-
  read_csv(here(paste0(
    "data/", dataset_directory, "/Export.csv"
  )),
  col_types = c("cicccccccTDDccciii")) %>%
  mutate(
    ExportStartDate = lubridate::ymd(ExportStartDate),
    ExportEndDate = lubridate::ymd(ExportEndDate),
    ExportDate = lubridate::ymd_hms(ExportDate)
  )

meta_export_start <- Export %>% pull(ExportStartDate)

meta_export_end <- Export %>% pull(ExportEndDate)

meta_export_pulled <- Export %>% pull(ExportDate)

# Calculated Dates --------------------------------------------------------

calc_data_goes_back_to <-
  read_csv(here(paste0("data/", dataset_directory, "Exit.csv")),
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn") %>%
  mutate(ExitDate = ymd(ExitDate)) %>%
  arrange(ExitDate) %>%
  head(1) %>% 
  pull(ExitDate)

calc_full_date_range <- interval(ymd(meta_HUDCSV_Export_End),
                                ymd(calc_data_goes_back_to))

calc_2_yrs_prior_end <- floor_date(today(), "month") - days(1)
calc_2_yrs_prior_start <-
  floor_date(ymd(calc_2_yrs_prior_end), "month") - years(2) + months(1)

calc_2_yrs_prior_range <- interval(ymd(calc_2_yrs_prior_start),
                                   ymd(calc_2_yrs_prior_end))



save(list = ls(), file = here("images/00_dates.RData"), compress = FALSE)
