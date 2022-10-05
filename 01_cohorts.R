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

# Some definitions:
# PH = PSH + RRH
# household = one or more people who present for housing/homeless services
# served = the Entry to Exit Date range crosses the Report Date range
# entered = the Entry Date is inside the Report Date range
# served_leaver = (regardless of Move-In) the Exit Date is inside the Report
#     Date range
# moved_in_leaver = a subset of served_leaver, these stays include a Move-In Date
#     where that's relevant (PH projects)
# moved_in = any stay in a non-PH project where the Entry to Exit Date range
#     crosses the Report Date range PLUS any stay in a PH project where the 
#     Move In Date to the Exit Date crosses the Report Date range
# hohs = heads of household
# adults = all adults in a household
# clients = all members of the household

library(tidyverse)
library(lubridate)
library(HMIS)

# Living Situations Groups (includes PLS, CLS, and destinations) 
#(Updated to match FY2022 DS) ---------------------------

perm_livingsituation <- c(3, 10, 11, 19:23, 26, 28, 31, 33, 34, 36)

lh_livingsituation <- c(1,16,18)

temp_livingsituation <- c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35) 

institutional_livingsituation <- c(4:7, 15, 25, 27, 29)

other_livingsituation <- c(8, 9, 17, 24, 30, 37, 99)

# Project Type Groupings --------------------------------------------------

lh_project_types <- c(1, 2, 4, 8)

ph_project_types <- c(3, 9, 13)

lh_at_entry_project_types <- c(1, 2, 3, 4, 8, 9, 13)

lh_ph_hp_project_types <- c(1, 2, 3, 4, 8, 9, 12, 13)

coc_funded_project_types <- c(2, 3, 13)

project_types_w_beds <- c(1, 2, 3, 8, 9)

# Funding Source Groupings ------------------------------------------------

va_fund_sources <- c(27, 30, 33, 37:42, 45)

rhy_fund_sources <- c(22:26)

path_fund_sources <- 21

ssvf_fund_sources <- 33

# Build Validation df for app ---------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectCommonName,
         ProjectName,
         ProjectType,
         HMISParticipatingProject) %>%
  filter(HMISParticipatingProject == 1 &
           operating_between(., calc_data_goes_back_to, ymd(meta_HUDCSV_Export_End)) &
           ProjectType %in% c(1:4, 8:9, 12:14)) 

smallEnrollment <- Enrollment %>% 
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    EntryDate,
    MoveInDate,
    ExitDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitAdjust,
    LivingSituation,
    Destination,
    DateCreated
  ) 

validation <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  select(
    ProjectID,
    ProjectName,
    ProjectType,
    EnrollmentID,
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    LivingSituation,
    Destination,
    DateCreated
  ) %>%
  filter(!is.na(EntryDate))

desk_time_providers <- validation %>%
  dplyr::filter(
    (entered_between(., today() - years(1), today()) |
       exited_between(., today() - years(1), today())) &
      ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
  dplyr::select(ProjectName) %>% unique()
