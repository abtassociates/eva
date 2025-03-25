directory <- "sandbox/mini-non-shiny-environment/"

library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(readxl)
library(writexl)
library(glue)
library(janitor)
library(here)
library(shinytest2)
library(zip)

source(here("global.R"))

source(paste0(directory, "helper_functions.R"))

# Hard codes --------------------------------------------------------------

source(paste0(directory, "hardcodes.R"))

# Get Export --------------------------------------------------------------
upload_filepath <- "/media/sdrive/projects/CE_Data_Toolkit/Data Sets/FY24-ICF-hashed-current-good.zip"
utils::unzip(upload_filepath, exdir = here(paste0(directory, "data")))

source(paste0(directory, "01_get_Export.R"))


# Org A (OrgId = 4), Org B = 6, Org O = 96
organization_ids <- c("4","6", "96")

# get corresponding project IDs
project_ids <- Project %>%
  filter(OrganizationID %in% organization_ids) %>%
  pull(ProjectID) %>%
  unique()

# get corresponding EnrollmentIDs
filtered_enrollments <- Enrollment %>%
  filter(ProjectID %in% project_ids) %>%
  select(EnrollmentID, PersonalID)

# Assign unique lists
enrollment_ids <- unique(filtered_enrollments$EnrollmentID)
personal_ids <- unique(filtered_enrollments$PersonalID)

# for each file in the csv, loop through the file names in the csv
for (file in c(unique(cols_and_data_types$File), "Disabilities")) {
  print(paste0("Updating ", file, " for demo.zip"))
  
  # Handling Disabilities separately because it's not part of 01_get_export
  # since we don't use it
  if(file == "Disabilities") {
    f = utils::unzip(zipfile = upload_filepath, files="Disabilities.csv")
    Disabilities <- as.data.frame(fread(Disablities))
    file.remove(paste0(file, ".csv"))
  }
  
  # filter the lowest level IDs based on the pulled lists above
  # Other than in Client, if we filter by PersonalID, we'll get enrollments from other projects
  df <- get(file) 
  if ("EnrollmentID" %in% colnames(df)) {
    df <- df %>% filter(EnrollmentID %in% enrollment_ids)
  } else if ("PersonalID" %in% colnames(df)) {
    df <- df %>% filter(PersonalID %in% personal_ids)
  } else if ("ProjectID" %in% colnames(df)) {
    df <- df %>% filter(ProjectID %in% project_ids)
  } else if ("OrganizationID" %in% colnames(df)) {
    df <- df %>% filter(OrganizationID %in% organization_ids)
  } 
  
  if(file == "Client") {
    df <- df %>% 
      # Add NbN overlap data
      bind_rows(
        tibble(
          PersonalID = "999999",
          NameDataQuality = 1,
          SSNDataQuality = 1,
          DOBDataQuality = 1,
          AmIndAKNative = 0,
          Asian = 0,
          BlackAfAmerican = 0,
          HispanicLatinaeo = 0,
          MidEastNAfrican = 0,
          NativeHIPacific = 1,
          White = 0,
          RaceNone = 0,
          VeteranStatus = 1,
          DateCreated = as.POSIXct("2022-09-22 15:48"),
          DateUpdated = as.POSIXct("2022-09-22"),
          UserID = "18",
          ExportID = "1036"
        )
      ) %>%
      # Add FSA issue - invalid date format
      mutate(DateUpdated = format(DateUpdated, "%d-%m-%y"))
      
  } else if(file == "CurrentLivingSituation") {
    df <- df %>% 
      # Add FSA issues - invalid living situation
      mutate(
        CurrentLivingSituation = ifelse(EnrollmentID == "813537", 999, CurrentLivingSituation)
      )
  } else if(file == "Disabilities") {
    # Get rid of all the many disabilities records. Right now, a person can have  
    # multiple Disabilities records per InformationDate per EnrollmentID
    # And we don't even use Disabilities right now. Let's just do 1 per person per DisabilityType
    df <- df %>%
      group_by(PersonalID, DisabilityType) %>%
      slice(1) %>%
      ungroup()
  } else if(file == "Enrollment") {
    df <- df %>% 
      # Add NbN overlap data
      bind_rows(
        tibble(
          EnrollmentID = c("999999", "999998", "999997", "999996"),
          PersonalID = rep("999999", 4),
          ProjectID = rep("999999", 4),
          EntryDate = rep(as.Date("2021-12-31"), 4),
          HouseholdID = rep("s_863799", 4),
          RelationshipToHoH = rep(1, 4),
          DisablingCondition = rep(0, 4),
          DateCreated = as.POSIXct(rep("2022-09-22 15:48", 4)),
          DateUpdated = as.POSIXct(rep("2022-09-23 12:27", 4)),
          UserID = rep("18", 4),
          ExportID = rep("1036", 4)
        )
      )
  } else if(file == "Exit") {
    df <- df %>% 
      # Add NbN overlap data
      bind_rows(
        tibble(
          ExitID = c("999999", "999998", "999997", "999996"),
          EnrollmentID = c("999999", "999998", "999997", "999996"),
          PersonalID = rep("999999", 4),
          ExitDate = as.Date(rep(c("2024-01-10", "2024-01-11"), 2)),
          Destination = rep(410, 4),
          DateCreated = as.POSIXct(rep("2022-09-22 15:48", 4)),
          DateUpdated = as.POSIXct(rep("2022-09-23 12:27", 4)),
          UserID = rep("18", 4),
          ExportID = rep("1036", 4)
        )
      )
  } else if(file == "Export") {
    # Add some demo-specific info to export
    df$SourceID = "DEMO999"
    df$SourceName = "DEMO-CoC"
  } else if(file == "Project") {
    df <- df %>% 
      # Add PDDE issue - missing RRH Subtype
      mutate(
        RRHSubType = ifelse(ProjectID == "1606", NA, RRHSubType)
      ) %>% 
      # Add NbN overlap data
      bind_rows(
        tibble(
          ProjectID = "999999",
          OrganizationID = "4",
          ProjectName = "Test NbN Project",
          ProjectType = 1,
          OperatingStartDate = ymd("20240427"),
          ContinuumProject = 1,
          DateCreated	= as.POSIXct("2021-04-27 10:05"),
          DateUpdated = as.POSIXct("2021-04-27 11:25"),
          UserID = "18",
          ExportID = "1036"
        )
      )
  } else if(file == "ProjectCoC") {
    df <- df %>% 
      # Add PDDE issue -missing geography info
      mutate(
        Geocode = ifelse(ProjectID == "1377", NA, Geocode)
      )
  } else if(file == "Services") {
    df <- df %>% 
      # Add NbN overlap data - 1 enrollment has 2 overlapping/identical DateProvideds
      # and another has an overlapping DateProvided with the first enrollment
      # Two other enrollments capture a scenario that we thought was getting flagged but shouldn't be
      bind_rows(
        tibble(
          ServicesID = c(
            "9999991", "9999992", "9999993", "9999994", "9999999",
            "9999995", "9999996", "9999997", "9999998", "9999990",
            "9999981", "9999982", "9999983", "9999984",
            "9999985", "9999986", "9999987", "9999988"
          ),
          EnrollmentID = c(
            rep("999999", 5), 
            rep("999998", 5),
            rep("999997", 4),
            rep("999996", 4)
          ),
          PersonalID = rep("999999", 18),
          DateProvided = as.Date(
            c("2024-01-01", "2024-01-03", "2024-01-03", "2024-01-05", "2024-01-07", 
              "2024-01-02", "2024-01-04", "2024-01-06", "2024-01-08", "2024-01-05",
              "2024-01-09", "2024-01-11", "2024-01-13", "2024-01-15", 
              "2024-01-10", "2024-01-12", "2024-01-14", "2024-01-16"
            )),
          RecordType = rep(200, 18),
          TypeProvided = rep(2, 18),
          DateCreated = as.POSIXct(rep("2022-04-25 12:53", 18)),
          DateUpdated = as.POSIXct(rep("2022-04-25 12:53", 18)),
          UserID = rep("18", 18),
          ExportID = rep("1036", 18)
        )
      )
  } else if(file == "User") {
    # Insert "issues" to be flagged by Eva
    df <- df %>% filter(UserID == "18")
  } 
  
  write.csv(
    df,
    here(paste0(directory, "data/", file, ".csv")),
    na = "",
    row.names = FALSE
  )
}

demo_zip <- "/media/sdrive/projects/CE_Data_Toolkit/Data Sets/demo.zip"
zipr(
  zipfile = demo_zip, 
  files = list.files(here("sandbox/mini-non-shiny-environment/data"),
                     pattern = "*.csv", full.names = TRUE),
  mode = "cherry-pick" # so the files are at the top directory
)

## xz compression makes the file small enough to get around GitHub's 100MB size limit
