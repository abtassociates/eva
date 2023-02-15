directory <- "sandbox/mini-non-shiny-environment/data/"

# Read in each file -------------------------------------------------------

Assessment <- 
  read_csv(paste0(directory, "Assessment.csv"),
           col_types = "cccDcnnnTTcTc")

Client <- 
  read_csv(paste0(directory, "Client.csv"), 
             col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")

client_problems <- problems(Client)

CurrentLivingSituation <- 
  read_csv(paste0(directory, "CurrentLivingSituation.csv"),
             col_types = "cccDncnnnnncTTcTc")

Enrollment <- 
  read_csv(paste0(directory, "Enrollment.csv"),
    col_types = 
      "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")

EnrollmentCoC <- 
  read_csv(paste0(directory, "EnrollmentCoC.csv"),
             col_types = "cccccDcnTTcTc")

EmploymentEducation <- 
  read_csv(paste0(directory, "EmploymentEducation.csv"),
             col_types = "cccDnnnnnnTTcTc")

Exit <- 
  read_csv(paste0(directory, "Exit.csv"), col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")

Event <- 
  read_csv(paste0(directory, "Event.csv"), col_types = "cccDnnncnDTTcTc")

Export <- 
  read_csv(paste0(directory, "Export.csv"), col_types = "cncccccccTDDcccnnn")

Funder <- 
  read_csv(paste0(directory, "Funder.csv"), col_types = "ccnccDDTTcTc")

HealthAndDV <- 
  read_csv(paste0(directory, "HealthAndDV.csv"), col_types = "cccDnnnnnnnDnnnnnTTcTc")

IncomeBenefits <- 
  read_csv(paste0(directory, 
    "IncomeBenefits.csv"),
    col_types =
      "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")

Inventory <- 
  read_csv(paste0(directory, "Inventory.csv"), col_types = "cccnnnnnnnnnnnnDDTTcTc")

Organization <- 
  read_csv(paste0(directory, "Organization.csv"), col_types = "ccncTTcTc")

Project <- 
  read_csv(paste0(directory, "Project.csv"), col_types = "ccccDDnnnnnnnnnTTcTc")

ProjectCoC <- 
  read_csv(paste0(directory, "ProjectCoC.csv"), col_types = "cccccccccnTTcTc")

Services <- 
  read_csv(paste0(directory, "Services.csv"), col_types = "cccDnnccnnnTTcTc")

User <- 
  read_csv(paste0(directory, "User.csv"), col_types = "ccccccTTTc")

YouthEducationStatus <- 
  read_csv(paste0(directory, "YouthEducationStatus.csv"), col_types = "cccDnnnnTTcTc")


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

