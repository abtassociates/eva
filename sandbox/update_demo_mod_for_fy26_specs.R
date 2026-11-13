library(data.table)
library(collapse) # for fmutate
library(zip)

random_hash <- function(n = 1, length = 64) {
  vapply(
    seq_len(n),
    function(x) {
      paste0(sample(c(0:9, letters[1:6]), length, replace = TRUE), collapse = "")
    },
    character(1)
  )
}

remove_orphan_enrollments <- function(temp_dir, csv_name, enrollment_ids) {
  csv_path <- file.path(temp_dir, paste0(csv_name, ".csv"))
  return(
    fread(csv_path) |>
      fsubset(EnrollmentID %iin% enrollment_ids) |>
      fwrite(file = csv_path)
  )
}

remove_orphan_project_ids <- function(temp_dir, csv_name, project_ids) {
  csv_path <- file.path(temp_dir, paste0(csv_name, ".csv"))
  return(
    fread(csv_path) |>
      fsubset(ProjectID %iin% project_ids) |>
      fwrite(file = csv_path)
  )
}
clean_client <- function(temp_dir) {
  # 4. Read, hash, and overwrite the CSV in the temp folder
  csv_path <- file.path(temp_dir, "Client.csv")
  client <- fread(csv_path)
  n <- nrow(client)
  
  client |>
    fmutate(
      FirstName  = random_hash(n),
      MiddleName = random_hash(n),
      LastName   = random_hash(n),
      SSN = paste0(
        random_hash(n),
        sprintf("%04d", sample(0:9999, n, replace = TRUE))
      ),
      RaceNone = fifelse(RaceNone == 0, 9, RaceNone)
    ) |>
    colorder(DOBDataQuality, Sex, pos="after") |>
    fwrite(file = csv_path)
}

clean_disabilities <- function(temp_dir) {
  csv_path <- file.path(temp_dir, "Disabilities.csv")
  
  fread(csv_path) |>
    frename(
      TcellCount = TCellCount,
      TcellSource = TCellSource
    ) |>
    fwrite(file = csv_path)
}
clean_enrollment <- function(temp_dir, enrollment) {
  csv_path <- file.path(temp_dir, "Enrollment.csv")
  return(enrollment |>
    colorder(ThresholdScore, MentalHealthConsultation, pos="after") |>
    fwrite(file = csv_path))
}
clean_organization <- function(temp_dir) {
  project <- fread(file.path(temp_dir, "Project.csv"))
  organization_ids <- funique(project$OrganizationID)
  csv_path <- file.path(temp_dir, "Organization.csv")
  return(
    fread(csv_path) |>
      fsubset(OrganizationID %iin% organization_ids) |>
      fwrite(file = csv_path)
  )
  
}

clean_zip_for_fy26_specs <- function(zip_path) {
  # 1. Create a dedicated temporary directory
  temp_dir <- tempfile(pattern = "unzip_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE)) # Cleans up temp files when done
  
  file.copy(zip_path, gsub("Demo Dataset Mods/", "Demo Dataset Mods/Old/", zip_path), overwrite = FALSE)
  
  # 2. Extract the entire ZIP into the temp folder
  utils::unzip(zipfile = zip_path, exdir = temp_dir)
  
  enrollment <- fread(file.path(temp_dir, "Enrollment.csv"))
  
  
  enrollment_ids <- funique(enrollment$EnrollmentID)
  personal_ids <- funique(enrollment$PersonalID)
  project_ids <- funique(enrollment$ProjectID)
  
  clean_client(temp_dir)
  clean_disabilities(temp_dir)
  clean_enrollment(temp_dir, enrollment)
  
  remove_orphan_enrollments(temp_dir, "Assessment", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "Disabilities", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "YouthEducationStatus", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "IncomeBenefits", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "HealthAndDV", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "CurrentLivingSituation", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "Event", enrollment_ids)
  remove_orphan_enrollments(temp_dir, "EmploymentEducation", enrollment_ids)
  
  
  remove_orphan_project_ids(temp_dir, "Project", project_ids)
  remove_orphan_project_ids(temp_dir, "CEParticipation", project_ids)
  remove_orphan_project_ids(temp_dir, "Funder", project_ids)
  remove_orphan_project_ids(temp_dir, "HMISParticipation", project_ids)
  remove_orphan_project_ids(temp_dir, "ProjectCoC", project_ids)
  remove_orphan_project_ids(temp_dir, "Inventory", project_ids)
  
  clean_organization(temp_dir)
  unlink(zip_path)
  # 5. Overwrite the original zip file with the updated contents
  zip::zip(
    zipfile = zip_path,
    files = list.files(temp_dir),
    root = temp_dir
  )
  
}