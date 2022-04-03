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

# PLEASE NOTE THIS SCRIPT OVERWRITES THE CLIENT.CSV FILE ON YOUR HARD DRIVE!
# IT REPLACES THE NAMES AND SSNS WITH DATA QUALITY SIGNIFIERS!
# IT CAN BE RUN ON A CLEAN CLIENT.CSV FILE OR ONE THAT'S BEEN OVERWRITTEN.

# This file is expecting the HUD CSV Export FY2020, unzipped

library(tidyverse)
library(lubridate)
library(readxl)
library(HMIS)

dataset_directory <- c("San-Diego3/")
directory <- paste0("data/", dataset_directory)

# calling in HMIS-related functions that aren't in the HMIS pkg
source("00_functions.R")

if (!exists("meta_HUDCSV_Export_Date")) source("00_dates.R")

# Affiliation -------------------------------------------------------------

Affiliation <- 
  read_csv(paste0(directory, "/Affiliation.csv"), 
           col_types = "cccTTcTc") 

# Client ------------------------------------------------------------------

  Client <-
    read_csv(paste0(directory, "/Client.csv"),
             col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc") 


# Replacing PII

if(ncol(read_csv(paste0(directory, "/Client.csv"))) == 36)
{Client <- Client %>%
  mutate(
    FirstName = case_when(
      NameDataQuality %in% c(8, 9) ~ "DKR",
      NameDataQuality == 2 ~ "Partial",
      NameDataQuality == 99 |
        is.na(NameDataQuality) |
        FirstName == "Anonymous" ~ "Missing",!(
          NameDataQuality %in% c(2, 8, 9, 99) |
            is.na(NameDataQuality) |
            FirstName == "Anonymous"
        ) ~ "ok"
    ),
    LastName = NULL,
    MiddleName = NULL,
    NameSuffix = NULL,
    SSN = case_when(
      (is.na(SSN) & !SSNDataQuality %in% c(8, 9)) |
        is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
      SSNDataQuality %in% c(8, 9) ~ "DKR",
      (nchar(SSN) != 9 & SSNDataQuality != 2) |
        substr(SSN, 1, 3) %in% c("000", "666") |
        substr(SSN, 1, 1) == 9 |
        substr(SSN, 4, 5) == "00" |
        substr(SSN, 6, 9) == "0000" |
        SSNDataQuality == 2 |
        SSN %in% c(
          111111111,
          222222222,
          333333333,
          444444444,
          555555555,
          777777777,
          888888888,
          123456789
        ) ~ "Invalid",
      SSNDataQuality == 2 & nchar(SSN) != 9 ~ "Incomplete"
    )
  )

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))}


CurrentLivingSituation <-
  read_csv(paste0(directory, "/CurrentLivingSituation.csv"),
            col_types = "cccDncnnnnncTTcTc") 

# Disabilities ------------------------------------------------------------

Disabilities <-
  read_csv(paste0(directory, "/Disabilities.csv"),
           col_types = "cccDnnnnnnnnnnnTTcTc")


# EmploymentEducation -----------------------------------------------------

EmploymentEducation <-
  read_csv(paste0(directory, "/EmploymentEducation.csv"),
           col_types = "cccDnnnnnnTTnTn")

# Exit --------------------------------------------------------------------

Exit <-
  read_csv(paste0(directory, "/Exit.csv"),
           col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")

# Project -----------------------------------------------------------------

Project <- 
  read_csv(paste0(directory, "/Project.csv"),
           col_types = "ccccDDnnnnnnnnnTTcTc") 

# EnrollmentCoC -----------------------------------------------------------

EnrollmentCoC <- 
  read_csv(paste0(directory, "/EnrollmentCoC.csv"), 
           col_types = "cccccDcnTTcTc")

# Enrollment --------------------------------------------------------------

Enrollment <-
  read_csv(paste0(directory, "/Enrollment.csv"),
           col_types =
             "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")

# Adding Exit Data to Enrollment because I'm not tryin to have one-to-one 
# relationships in this!

small_exit <- Exit %>% select(EnrollmentID, 
                              ExitDate, 
                              Destination, 
                              OtherDestination)

Enrollment <- left_join(Enrollment, small_exit, by = "EnrollmentID") %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate) |
                                ExitDate > today(),
                              today(), ExitDate))

rm(small_exit)

# Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
small_project <- Project %>%
  select(ProjectID, ProjectType, ProjectName) 

# getting HH information
# only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.
HHMoveIn <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  filter(ProjectType %in% c(3, 9, 13)) %>%
  mutate(
    AssumedMoveIn = if_else(
      ymd(EntryDate) < hc_psh_started_collecting_move_in_date &
        ProjectType %in% c(3, 9),
      1,
      0
    ),
    ValidMoveIn = case_when(
      AssumedMoveIn == 1 ~ EntryDate,
      AssumedMoveIn == 0 &
        ProjectType %in% c(3, 9) &
        ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(ExitAdjust) > ymd(MoveInDate) ~ MoveInDate,
      # the Move-In Dates must fall between the Entry and ExitAdjust to be
      # considered valid and for PSH the hmid cannot = ExitDate
      ymd(MoveInDate) <= ymd(ExitAdjust) &
        ymd(MoveInDate) >= ymd(EntryDate) &
        ProjectType == 13 ~ MoveInDate
    )
  ) %>% 
  filter(!is.na(ValidMoveIn)) %>%
  group_by(HouseholdID) %>%
  mutate(HHMoveIn = min(ValidMoveIn)) %>%
  ungroup() %>%
  select(HouseholdID, HHMoveIn) %>%
  unique()

HHEntry <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  group_by(HouseholdID) %>%
  mutate(FirstEntry = min(EntryDate)) %>%
  ungroup() %>%
  select(HouseholdID, "HHEntry" = FirstEntry) %>%
  unique() %>%
  left_join(HHMoveIn, by = "HouseholdID")


Enrollment <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  left_join(HHEntry, by = "HouseholdID") %>%
  mutate(
    MoveInDateAdjust = if_else(!is.na(HHMoveIn) &
                                 ymd(HHMoveIn) <= ymd(ExitAdjust),
                               if_else(ymd(EntryDate) <= ymd(HHMoveIn),
                                       HHMoveIn, EntryDate),
                               NA_real_), 
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
    )
  )

rm(small_project, HHEntry, HHMoveIn)

# Client Location

y <- EnrollmentCoC %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, "ClientLocation" = CoCCode) 

Enrollment <- Enrollment %>%
  left_join(y, by = "EnrollmentID")

rm(y)

# Event -------------------------------------------------------------------

Event <-
  read_csv(paste0(directory, "/Event.csv"),
           col_types = "cccDnnncnDTTcTc") 

# Funder ------------------------------------------------------------------

Funder <- 
  read_csv(paste0(directory, "/Funder.csv"),
           col_types = "ccnccDDTTcTc")

# HealthAndDV -------------------------------------------------------------

HealthAndDV <-
  read_csv(paste0(directory, "/HealthAndDV.csv"),
           col_types = "cccDnnnnnnnDnnnnnTTcTc")

# IncomeBenefits ----------------------------------------------------------

IncomeBenefits <- 
  read_csv(paste0(directory, "/IncomeBenefits.csv"),
           col_types = 
             "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")

# Inventory ---------------------------------------------------------------

Inventory <-
  read_csv(paste0(directory, "/Inventory.csv"),
           col_types = "cccnnnnnnnnnnnnDDTTcTc")

# Organization ------------------------------------------------------------

Organization <- 
  read_csv(paste0(directory, "/Organization.csv"),
           col_types = "ccncTTcTc")

# ProjectCoC --------------------------------------------------------------

ProjectCoC <- 
  read_csv(paste0(directory, "/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTc")

# Users ------------------------------------------------------------------
Users <- read_csv(paste0(directory, "/User.csv"),
                  col_types = "ccccccTTTc")

# Services ----------------------------------------------------------------

Services <- read_csv(paste0(directory, "/Services.csv"),
                  col_types = "cccDnnccnnnTTcTc")

# HUD CSV Specs -----------------------------------------------------------

HUD_specs <- read_csv("public_data/HUDSpecs.csv",
                      col_types = "ccnc") %>%
  as.data.frame()

# Adding Age at Entry to Enrollment ---------------------------------------
small_client <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)

rm(small_client)


# Assessments -------------------------------------------------------------

Assessment <- read_csv(paste0(directory, "/Assessment.csv"),
                       col_types = "cccDcnnnTTcTc")

AssessmentQuestions <- read_csv(paste0(directory, "/AssessmentQuestions.csv"),
                       col_types = "cccccnccTTcTc")

AssessmentResults <- read_csv(paste0(directory, "/AssessmentResults.csv"),
                       col_types = "ccccccTTcTc")

# Youth Education Status --------------------------------------------------

YouthEducationStatus <- read_csv(paste0(directory, "/YouthEducationStatus.csv"),
                                 col_types = "cccDnnnnTTcTc")

# Save it out -------------------------------------------------------------

# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/CSVExportDFs.RData", compress = FALSE)


