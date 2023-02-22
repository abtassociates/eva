
library(tidyverse)
library(lubridate)
library(readxl)
library(HMIS)

Assessment <- 
  importFile("Assessment", col_types = "cccDcnnnTTcTc")

Client <- 
  importFile("Client", col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")

client_problems <- problems(Client)

CurrentLivingSituation <- 
  importFile("CurrentLivingSituation", col_types = "cccDncnnnnncTTcTc")

Enrollment <- 
  importFile(
    "Enrollment",
    col_types = 
      "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")

EnrollmentCoC <- 
  importFile("EnrollmentCoC", col_types = "cccccDcnTTcTc")

EmploymentEducation <- 
  importFile("EmploymentEducation", col_types = "cccDnnnnnnTTcTc")

Exit <- 
  importFile("Exit", col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")

Event <- 
  importFile("Event", col_types = "cccDnnncnDTTcTc")

Export <- 
  importFile("Export", col_types = "cncccccccTDDcccnnn")

Funder <- 
  importFile("Funder", col_types = "ccnccDDTTcTc")

HealthAndDV <- 
  importFile("HealthAndDV", col_types = "cccDnnnnnnnDnnnnnTTcTc")

IncomeBenefits <- 
  importFile(
    "IncomeBenefits",
    col_types =
      "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")

Inventory <- 
  importFile("Inventory", col_types = "cccnnnnnnnnnnnnDDTTcTc")

Organization <- 
  importFile("Organization", col_types = "ccncTTcTc")

Project <- 
  importFile("Project", col_types = "ccccDDnnnnnnnnnTTcTc")

ProjectCoC <- 
  importFile("ProjectCoC", col_types = "cccccccccnTTcTc")

Services <- 
  importFile("Services", col_types = "cccDnnccnnnTTcTc")

User <- 
  importFile("User", col_types = "ccccccTTTc")

YouthEducationStatus <- 
  importFile("YouthEducationStatus", col_types = "cccDnnnnTTcTc")

# problems() is a built-in function that collects the problems reported in theconsole from a read_csv() call.
problems <- rbind(
  # problems(Affiliation),
  problems(Assessment),
  # problems(AssessmentQuestions),
  # problems(AssessmentResults),
  problems(Client),
  problems(CurrentLivingSituation),
  # problems(Disabilities),
  problems(EmploymentEducation),
  problems(Enrollment),
  problems(EnrollmentCoC),
  problems(Event),
  problems(Exit),
  problems(Export),
  problems(Funder),
  problems(HealthAndDV),
  problems(IncomeBenefits),
  problems(Inventory),
  problems(Organization),
  problems(Project),
  problems(ProjectCoC),
  problems(Services),
  problems(User),
  problems(YouthEducationStatus)
)

