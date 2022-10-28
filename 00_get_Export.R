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



client_problems <- problems(Client)

Enrollment <- 
  importFile("Enrollment",
             col_types =
               "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Enrollment <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Enrollment.csv"),
#       col_types = "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")
#   }

CurrentLivingSituation <- importFile("CurrentLivingSituation",
                                     col_types = "cccDncnnnnncTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   CurrentLivingSituation <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "CurrentLivingSituation.csv"),
#       col_types = "cccDncnnnnncTTcTc")
# }

# if (is.null(input$imported)) {
#   return()
# } else {
#   Disabilities <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Disabilities.csv"),
#       col_types = "cccDnnnnnnnnnnnTTcTc")
# }

EmploymentEducation <- importFile("EmploymentEducation",
                                  col_types = "cccDnnnnnnTTnTn")
# if (is.null(input$imported)) {
#   return()
# } else {
#   EmploymentEducation <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "EmploymentEducation.csv"),
#       col_types = "cccDnnnnnnTTnTn")
# }

Exit <- importFile("Exit",
                   col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Exit <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Exit.csv"),
#       col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")
# }

Organization <- importFile("Organization",
                           col_types = "ccncTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Organization <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Organization.csv"),
#       col_types = "ccncTTcTc")
# }

Project <- importFile("Project", col_types = "ccccDDnnnnnnnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Project <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Project.csv"),
#       col_types = "ccccDDnnnnnnnnnTTcTc")
# }

EnrollmentCoC <- importFile("EnrollmentCoC", col_types = "cccccDcnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   EnrollmentCoC <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "EnrollmentCoC.csv"),
#       col_types = "cccccDcnTTcTc")
# }

Event <- importFile("Event", col_types = "cccDnnncnDTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Event <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Event.csv"),
#       col_types = "cccDnnncnDTTcTc")
# }

Funder <- importFile("Funder", col_types = "ccnccDDTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Funder <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Funder.csv"),
#       col_types = "ccnccDDTTcTc")
# }

HealthAndDV <- importFile("HealthAndDV", col_types = "cccDnnnnnnnDnnnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   HealthAndDV <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "HealthAndDV.csv"),
#       col_types = "cccDnnnnnnnDnnnnnTTcTc")
# }

IncomeBenefits <- 
  importFile("IncomeBenefits",
             col_types = 
               "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   IncomeBenefits <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "IncomeBenefits.csv"),
#       col_types = 
#"cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")
# }

Inventory <- importFile("Inventory",col_types = "cccnnnnnnnnnnnnDDTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Inventory <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Inventory.csv"),
#       col_types = "cccnnnnnnnnnnnnDDTTcTc")
# }

ProjectCoC <- importFile("ProjectCoC",col_types = "cccccccccnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   ProjectCoC <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "ProjectCoC.csv"),
#       col_types = "nncnccccnnTTcTc")
# }

User <- importFile("User",col_types = "ccccccTTTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   User <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "User.csv"),
#       col_types = "ccccccTTTc")
# }

Services <- importFile("Services",col_types = "cccDnnccnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Services <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Services.csv"),
#       col_types = "cccDnnccnnnTTcTc")
# }

YouthEducationStatus <- importFile("YouthEducationStatus",
                                   col_types = "cccDnnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   YouthEducationStatus <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "YouthEducationStatus.csv"),
#       col_types = "cccDnnnnTTcTc")
# }

Assessment <- importFile("Assessment",col_types = "cccDcnnnTTcTc")
# if (is.null(input$imported)) {
#   return()
# } else {
#   Assessment <-
#     read_csv(
#       unzip(zipfile = input$imported$datapath, files = "Assessment.csv"),
#       col_types = "cccDcnnnTTcTc")
# }
