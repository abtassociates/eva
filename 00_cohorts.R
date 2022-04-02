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

if (!exists("Enrollment")) {
  load("images/COHHIOHMIS.RData")
  rlang::env_binding_lock(environment(), ls())
}

vars_we_want <- c(
  "PersonalID",
  "EnrollmentID",
  "CountyServed",
  "ProjectName",
  "ProjectID",
  "ProjectType",
  "HouseholdID",
  "AgeAtEntry",
  "RelationshipToHoH",
  "VeteranStatus",
  "EntryDate",
  "EntryAdjust",
  "MoveInDate",
  "MoveInDateAdjust",
  "ExitDate",
  "ExitAdjust",
  "Destination"
)

# Transition Aged Youth

tay <- Enrollment %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want)) %>%
  group_by(HouseholdID) %>%
  mutate(
    TAY = if_else(max(AgeAtEntry) < 25 & max(AgeAtEntry) >= 16, 1, 0)
  ) %>%
  ungroup() %>%
  filter(TAY == 1 & !is.na(ProjectName))

# Leaver and Stayer HoHs who were served during the reporting period
co_hohs_served <-  Enrollment %>%
  filter(served_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           RelationshipToHoH == 1) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))

summary_hohs_served <- co_hohs_served %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(hohs_served = n())

# Leaver HoHs served during the reporting period
co_hohs_served_leavers <-  Enrollment %>%
  filter(
      exited_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))	

summary_hohs_served_leavers <- co_hohs_served_leavers %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(hohs_served_leavers = n())

#	Leavers	who were Served During Reporting Period	Deaths
co_hohs_served_leavers_died <- Enrollment %>%
  filter(
      exited_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
      RelationshipToHoH == 1,
      Destination == 24
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))	

summary_hohs_served_leavers_died  <- co_hohs_served_leavers_died  %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(hohs_served_leavers_died = n())

#	Leavers and Stayers	who were Served During Reporting Period	All
co_clients_served <-  Enrollment %>%
  filter(served_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))

summary_clients_served <- co_clients_served %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(clients_served = n())

#	Leavers and Stayers	who were Served During Reporting Period	Adults
co_adults_served <-  Enrollment %>%
  filter(served_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))

summary_adults_served <- co_adults_served %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(adults_served = n())

#	Leavers and Stayers	who	Entered During Reporting Period	Adults

co_adults_entered <-  Enrollment %>%
  filter(entered_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))

summary_adults_entered <- co_adults_entered %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(adults_entered = n())

#	Leavers and Stayers	who	Entered During Reporting Period	HoHs
co_hohs_entered <- Enrollment %>%
  filter(
    entered_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))	

summary_hohs_entered <- co_hohs_entered %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(hohs_entered = n())

#	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	All
co_clients_moved_in <-  Enrollment %>%
  filter(
    stayed_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End)
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))

summary_clients_moved_in <- co_clients_moved_in %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(clients_moved_in = n())

#	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	Adults
co_adults_moved_in <-  Enrollment %>%
  filter(stayed_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))	

summary_adults_moved_in <- co_adults_moved_in %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(adults_moved_in = n())

#	Leavers	who were Served During Reporting Period (and Moved In)	All
co_clients_moved_in_leavers <-  Enrollment %>%
  filter(exited_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           stayed_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))	

summary_clients_moved_in_leavers <- co_clients_moved_in_leavers %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(clients_moved_in_leavers = n())

#	Leaver hohs	who were Served (and Moved In) During Reporting Period	HoHs
co_hohs_moved_in_leavers <-  Enrollment %>%
  filter(stayed_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           exited_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           RelationshipToHoH == 1) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want))	

summary_hohs_moved_in_leavers <- co_hohs_moved_in_leavers %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(hohs_moved_in_leavers = n())

#	Leavers	who were Served During Reporting Period (and Moved In)	Adults
co_adults_moved_in_leavers <-  Enrollment %>%
  filter(exited_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           stayed_between(., ymd(calc_data_goes_back_to), meta_HUDCSV_Export_End) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(all_of(vars_we_want)) 

summary_adults_moved_in_leavers <- co_adults_moved_in_leavers %>%
  distinct(PersonalID, ProjectName) %>%
  group_by(ProjectName) %>%
  summarise(adults_moved_in_leavers = n())

summary <- summary_clients_served %>%
  full_join(summary_clients_moved_in, by = "ProjectName") %>%
  full_join(summary_hohs_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_adults_served, by = "ProjectName") %>%
  full_join(summary_adults_moved_in, by = "ProjectName") %>%
  full_join(summary_clients_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_adults_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_hohs_served, by = "ProjectName") %>%
  full_join(summary_hohs_entered, by = "ProjectName") %>%
  full_join(summary_hohs_served_leavers, by= "ProjectName") %>%
  full_join(summary_adults_entered, by = "ProjectName") %>% 
  full_join(summary_hohs_served_leavers_died, by = "ProjectName")

rm(vars_we_want)

# APs ---------------------------------------------------------------------

project_addresses <- ProjectCoC %>%
  select(ProjectID, CoCCode, Address1, Address2, City, State, ZIP)

APs <- Project %>%
  inner_join(provider_geo, by = c("ProjectID", "ProjectName")) %>%
  filter(ProjectType == 14) %>%
  left_join(provider_services, by = "ProjectID") %>%
  select(
    ProjectID,
    ProjectAKA,
    OrganizationName,
    ProjectName,
    TargetPop,
    CountiesServed,
    ProjectAreaServed,
    ProjectHours,
    ProjectWebsite,
    ProjectTelNo
  ) %>%
  unique() %>%
  mutate(OrgLink = if_else(!is.na(ProjectWebsite), paste0(
    "<a href='",
    ProjectWebsite,
    "' target='_blank'>",
    ProjectAKA,
    "</a><small> (#",
    ProjectID,
    ")</small>"
  ), paste0(ProjectAKA,
           "<small> (#",
           ProjectID,
           ")</small>"))) %>%
  left_join(project_addresses, by = "ProjectID") %>%
  mutate(
    City = paste0(City, ", ", State, " ", ZIP),
    Addresses = coalesce(Address1, Address2)
  ) %>%
  select(ProjectID, ProjectAKA, OrganizationName, ProjectName, TargetPop,
         "ProjectCountyServed" = CountiesServed, ProjectAreaServed,
         ProjectHours, ProjectTelNo, OrgLink, CoCCode, Addresses, City)

write_csv(APs, "public_data/aps.csv")
  
rm(list = ls(pattern = "summary_"))


# PIT Counts --------------------------------------------------------------

BoS_PIT <- dplyr::tribble(
  ~Population, ~January2019Count, ~January2020Count,
  "Total", 3479, 3577,
  "Sheltered", 3479 - 814, 3577 - 986, # total minus unsheltered
  "Veterans", 159, 162,
  "Chronic", 330, 192
)

Mah_PIT <- dplyr::tribble(
  ~Population, ~January2019Count, ~January2020Count,
  "Total", 148, 100,
  "Sheltered", 133, 78,
  "Veterans", 3, 2,
  "Chronic", 18, 6
)


# Counties ----------------------------------------------------------------

bos_counties <- ServiceAreas %>%
  filter(CoC == "OH-507 Balance of State") %>%
  pull(County)

# Destinations Groups (FY2020) --------------------------------------------

perm_destinations <- c(3, 10, 11, 19:23, 26, 28, 31, 33, 34, 36)

temp_destinations <-  c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35) 

institutional_destinations <- c(4:7, 15, 25, 27, 29)

other_destinations <- c(8, 9, 17, 24, 30, 37, 99)

# Project Groupings -------------------------------------------------------

GPD_project_ids <- c(751, 776, 749, 1229, 127, 550)

fake_projects <- c(1027, 1849, 1028, 1033, 1032, 1029, 1931, 1030, 1031, 1317)

unsheltered_projects <- c(1695, 1680)

mahoning_projects <-
  c(696:697, 1327:1328, 1330:1331, 1392, 1638:1641, 1704, 1738, 2103, 2105,
    2110, 2322:2336, 2338:2360, 2362:2385, 2437)

# Project Type Groupings --------------------------------------------------

lh_project_types <- c(1, 2, 8)

ph_project_types <- c(3, 9, 13)

lh_at_entry_project_types <- c(1, 2, 3, 4, 8, 9, 13)

lh_ph_hp_project_types <- c(1, 2, 3, 4, 8, 9, 12, 13)

coc_funded_project_types <- c(2, 3, 13)

project_types_w_beds <- c(1, 2, 3, 8, 9)

# User Groups -------------------------------------------------------------

# HMIS Admins and CoC team UserIDs

COHHIO_admin_user_ids <- c(641, 835, 1041, 1239, 1563, 1624, 1628, 1868, 1698)

# Save it out -------------------------------------------------------------
# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/cohorts.RData")

