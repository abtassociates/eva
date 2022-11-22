
library(tidyverse)
library(lubridate)
library(readxl)
library(HMIS)

client_problems <- problems(Client)

Enrollment <- 
  importFile("Enrollment",
             col_types =
               "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")

CurrentLivingSituation <- importFile("CurrentLivingSituation",
                                     col_types = "cccDncnnnnncTTcTc")

EmploymentEducation <- importFile("EmploymentEducation",
                                  col_types = "cccDnnnnnnTTnTn")

Exit <- importFile("Exit",
                   col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")

Organization <- importFile("Organization",
                           col_types = "ccncTTcTc")

Project <- importFile("Project", col_types = "ccccDDnnnnnnnnnTTcTc")


EnrollmentCoC <- importFile("EnrollmentCoC", col_types = "cccccDcnTTcTc")

Event <- importFile("Event", col_types = "cccDnnncnDTTcTc")

Funder <- importFile("Funder", col_types = "ccnccDDTTcTc")

HealthAndDV <- importFile("HealthAndDV", col_types = "cccDnnnnnnnDnnnnnTTcTc")

IncomeBenefits <- 
  importFile("IncomeBenefits",
             col_types = 
               "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")

Inventory <- importFile("Inventory",col_types = "cccnnnnnnnnnnnnDDTTcTc")

ProjectCoC <- importFile("ProjectCoC",col_types = "cccccccccnTTcTc")

User <- importFile("User",col_types = "ccccccTTTc")

Services <- importFile("Services",col_types = "cccDnnccnnnTTcTc")

YouthEducationStatus <- importFile("YouthEducationStatus",
                                   col_types = "cccDnnnnTTcTc")

Assessment <- importFile("Assessment",col_types = "cccDcnnnTTcTc")

