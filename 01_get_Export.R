
logToConsole("Running get export")

# create a list of file names and column types
file_list <- list(
  Assessment = "cccDcnnnTTcTc",
  Client = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc",
  CurrentLivingSituation = "cccDncnnnnncTTcTc",
  Enrollment = "cccDcnnnnnDnnnDDDnnnnccccnnnDnnnncnnnnnnnnnnnncnnnnnnnnnnnnnnnnnnnnTTcTc",
  EnrollmentCoC = "cccccDcnTTcTc",
  EmploymentEducation = "cccDnnnnnnTTcTc",
  Exit = "cccDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTcTc",
  Event = "cccDnnncnDTTcTc",
  Export = "cncccccccTDDcccnnn",
  Funder = "ccnccDDTTcTc",
  HealthAndDV = "cccDnnnnnnnDnnnnnTTcTc",
  IncomeBenefits = "cccDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnnnTTcTc",
  Inventory = "cccnnnnnnnnnnnnDDTTcTc",
  Organization = "ccncTTcTc",
  Project = "ccccDDnnnnnnnnnTTcTc",
  ProjectCoC = "cccccccccnTTcTc",
  Services = "cccDnnccnnnTTcTc",
  User = "ccccccTTTc",
  YouthEducationStatus = "cccDnnnnTTcTc"
)

# loop over the file names and import each file with its corresponding column types
for (i in seq_along(file_list)) {
  file_name <- names(file_list)[i]
  col_types <- file_list[[i]]
  assign(file_name, importFile(file_name, col_types = col_types))
}

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

