data_directory <- "sandbox/mini-non-shiny-environment/data/"

# Read in each file -------------------------------------------------------

Assessment <- 
  read_csv(paste0(data_directory, "Assessment.csv"),
           col_types = "cccDcnnnTTcTc")

Client <- 
  read_csv(paste0(data_directory, "Client.csv"), 
             col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")

client_problems <- problems(Client)

CurrentLivingSituation <- 
  read_csv(paste0(data_directory, "CurrentLivingSituation.csv"),
             col_types = "cccDncnnnnncTTcTc")

Enrollment <- 
  read_csv(paste0(data_directory, "Enrollment.csv"),
    col_types = 
      "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc")

EnrollmentCoC <- 
  read_csv(paste0(data_directory, "EnrollmentCoC.csv"),
             col_types = "cccccDcnTTcTc")

EmploymentEducation <- 
  read_csv(paste0(data_directory, "EmploymentEducation.csv"),
             col_types = "cccDnnnnnnTTcTc")

Exit <- 
  read_csv(paste0(data_directory, "Exit.csv"), col_types = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc")

Event <- 
  read_csv(paste0(data_directory, "Event.csv"), col_types = "cccDnnncnDTTcTc")

Export <- 
  read_csv(paste0(data_directory, "Export.csv"), col_types = "cncccccccTDDcccnnn")

Funder <- 
  read_csv(paste0(data_directory, "Funder.csv"), col_types = "ccnccDDTTcTc")

HealthAndDV <- 
  read_csv(paste0(data_directory, "HealthAndDV.csv"), col_types = "cccDnnnnnnnDnnnnnTTcTc")

IncomeBenefits <- 
  read_csv(paste0(data_directory, 
    "IncomeBenefits.csv"),
    col_types =
      "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc")

Inventory <- 
  read_csv(paste0(data_directory, "Inventory.csv"), col_types = "cccnnnnnnnnnnnnDDTTcTc")

Organization <- 
  read_csv(paste0(data_directory, "Organization.csv"), col_types = "ccncTTcTc")

Project <- 
  read_csv(paste0(data_directory, "Project.csv"), col_types = "ccccDDnnnnnnnnnTTcTc")

ProjectCoC <- 
  read_csv(paste0(data_directory, "ProjectCoC.csv"), col_types = "cccccccccnTTcTc")

Services <- 
  read_csv(paste0(data_directory, "Services.csv"), col_types = "cccDnnccnnnTTcTc")

User <- 
  read_csv(paste0(data_directory, "User.csv"), col_types = "ccccccTTTc")

YouthEducationStatus <- 
  read_csv(paste0(data_directory, "YouthEducationStatus.csv"), col_types = "cccDnnnnTTcTc")


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

