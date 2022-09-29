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

# if(!exists("dataset_directory")) dataset_directory <- "San-Diego3/"
# directory <- paste0("data/", dataset_directory)

# import

parseDate <- function(datevar) {
  newDatevar <- parse_date_time(datevar,
                                orders = c("Ymd", "mdY"))
  return(newDatevar)
}

importFile <- function(csvFile, col_types=NULL, guess_max=1000) {
  if (is.null(input$imported)) {return()}
  filename = glue::glue("{csvFile}.csv")
  data <- read_csv(unzip(zipfile = input$imported$datapath, files = filename)
                   ,col_types = col_types #,
                   #guess_max = min(guess_max, n_max) AS 9/8: was getting an error: Error in vroom::vroom: object 'n_max' not found
  )
  file.remove(filename)
  return(data)
}
Enrollment <- importFile("Enrollment",col_types="cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")

# if (is.null(input$imported)) {
#   return()
# } else {
#   Enrollment <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Enrollment.csv"),
#       col_types = "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")
#   }

if (is.null(input$imported)) {
  return()
} else {
  CurrentLivingSituation <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "CurrentLivingSituation.csv"),
      col_types = "cccDncnnnnncTTcTc")
}

# if (is.null(input$imported)) {
#   return()
# } else {
#   Disabilities <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Disabilities.csv"),
#       col_types = "cccDnnnnnnnnnnnTTcTc")
# }

if (is.null(input$imported)) {
  return()
} else {
  EmploymentEducation <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "EmploymentEducation.csv"),
      col_types = "cccDnnnnnnTTnTn")
}

if (is.null(input$imported)) {
  return()
} else {
  Exit <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Exit.csv"),
      col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Organization <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Organization.csv"),
      col_types = "ccncTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Project <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Project.csv"),
      col_types = "ccccDDnnnnnnnnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  EnrollmentCoC <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "EnrollmentCoC.csv"),
      col_types = "cccccDcnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Event <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Event.csv"),
      col_types = "cccDnnncnDTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Export <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Export.csv"),
      col_types = "cncccccccTDDcncnnn")
}

if (is.null(input$imported)) {
  return()
} else {
  Funder <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Funder.csv"),
      col_types = "ccnccDDTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  HealthAndDV <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "HealthAndDV.csv"),
      col_types = "cccDnnnnnnnDnnnnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  IncomeBenefits <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "IncomeBenefits.csv"),
      col_types = "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Inventory <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Inventory.csv"),
      col_types = "cccnnnnnnnnnnnnDDTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  ProjectCoC <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "ProjectCoC.csv"),
      col_types = "nncnccccnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  User <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "User.csv"),
      col_types = "ccccccTTTc")
}
# 
# if (is.null(input$imported)) {
#   return()
# } else {
#   Services <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Services.csv"),
#       col_types = "cccDnnccnnnTTcTc")
# }

if (is.null(input$imported)) {
  return()
} else {
  YouthEducationStatus <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "YouthEducationStatus.csv"),
      col_types = "cccDnnnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Client <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Client.csv"),
      col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")
}

if (is.null(input$imported)) {
  return()
} else {
  Assessment <-
    read_csv(
      unzip(zipfile = input$imported$datapath, files = "Assessment.csv"),
      col_types = "cccDcnnnTTcTc")
}

Project <- Project %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName),
            by = "OrganizationID")

# Client ------------------------------------------------------------------
Client <- Client %>%
  mutate(
    FirstName = case_when(
      NameDataQuality %in% c(8, 9) ~ "DKR",
      NameDataQuality == 2 ~ "Partial",
      NameDataQuality == 99 |
        is.na(NameDataQuality) |
        FirstName == "Anonymous" ~ "Missing",
      !(
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
    ),
    PersonalID = as.character(PersonalID)
  ) %>%
  mutate(SSN = case_when(is.na(SSN) ~ "ok", !is.na(SSN) ~ SSN))

# Enrollment --------------------------------------------------------------
Enrollment <- Enrollment %>%
  mutate(
    EntryDate = parseDate(EntryDate),
    DateCreated = parseDate(DateCreated),
    MoveInDate = parseDate(MoveInDate),
    DateToStreetESSH = parseDate(DateToStreetESSH),
    PersonalID = as.character(PersonalID)
  )

# Adding Exit Data to Enrollment because I'm not tryin to have one-to-one 
# relationships in this!
small_exit = Exit %>% select(EnrollmentID, Destination, ExitDate, OtherDestination)
Enrollment <- left_join(Enrollment, small_exit, by = "EnrollmentID") %>% 
  mutate(ExitAdjust = if_else(is.na(ExitDate) |
                                ExitDate > today(),
                              today(), ExitDate))

# Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
small_project <- Project %>% select(ProjectID, ProjectType, ProjectName) 

# getting HH information
# only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.
HHMoveIn <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  filter(ProjectType %in% c(3, 9, 13)) %>%
  mutate(
    AssumedMoveIn = if_else(
      EntryDate < hc_psh_started_collecting_move_in_date &
        ProjectType %in% c(3, 9),
      1,
      0
    ),
    ValidMoveIn = case_when(
      AssumedMoveIn == 1 ~ EntryDate,
      AssumedMoveIn == 0 &
        ProjectType %in% c(3, 9) &
        EntryDate <= MoveInDate &
        ExitAdjust > MoveInDate ~ MoveInDate,
      # the Move-In Dates must fall between the Entry and ExitAdjust to be
      # considered valid and for PSH the hmid cannot = ExitDate
      MoveInDate <= ExitAdjust &
        MoveInDate >= EntryDate &
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
                                 ymd(HHMoveIn) <= ExitAdjust,
                               if_else(EntryDate <= ymd(HHMoveIn),
                                       HHMoveIn, EntryDate),
                               NA_real_), 
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
    )
  )

# Adding Age at Entry to Enrollment
small_client <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)

# Client Location
small_location <- EnrollmentCoC %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, "ClientLocation" = CoCCode)  %>%
  mutate(EnrollmentID = EnrollmentID %>% as.character())

Enrollment <- Enrollment %>%
  left_join(small_location, by = "EnrollmentID")


rm(small_project, HHEntry, HHMoveIn, small_client, small_location)

# HUD CSV Specs -----------------------------------------------------------

HUD_specs <- read_csv("public_data/HUDSpecs.csv",
                      col_types = "ccnc") %>%
  as.data.frame()

