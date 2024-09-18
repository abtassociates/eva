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
utils::unzip("/media/sdrive/projects/CE_Data_Toolkit/Data Sets/FY24-ICF-hashed-current-good.zip", exdir=here(paste0(directory, "data")))
source(paste0(directory, "01_get_Export.R"))


# Org A (OrgId = 4), Org B = 6, Org N = 95
organization_ids <- c("4","6")
# get project IDs
project_ids <- Project %>% filter(OrganizationID %in% organization_ids) %>% pull(ProjectID) %>% unique()

enrollment_ids <- Enrollment %>% filter(ProjectID %in% project_ids) %>% pull(EnrollmentID) %>% unique()

personal_ids <- Enrollment %>% filter(ProjectID %in% project_ids) %>% pull(PersonalID) %>% unique()
# for each file in the csv, loop through the file names in the csv
for (file in unique(cols_and_data_types$File)) {
  print(file)
  if(file == "Export") next
  
  if(file == "Organization") {
    write.csv(
      Organization %>% filter(OrganizationID %in% organization_ids),
      here(paste0(directory, "data/Organization.csv")),
      na = "",
      row.names = FALSE
    )
    next
  }
  if(file == "User") {
    write.csv(
      User %>% filter(UserID == "18"),
      here(paste0(directory, "data/User.csv")),
      na = "",
      row.names = FALSE
    )
    next
  }
  
  # Add FSA issues
  if(file == "Client") {
    write.csv(
      Client %>% 
        filter(PersonalID %in% personal_ids) %>%
        mutate(DateUpdated = format(DateUpdated, "%d-%m-%y")),
      here(paste0(directory, "data/Client.csv")),
      na = "",
      row.names = FALSE
    )
    next
  }
  
  if(file == "CurrentLivingSituation") {
    write.csv(
      CurrentLivingSituation %>% 
        filter(EnrollmentID %in% enrollment_ids) %>%
        mutate(
          CurrentLivingSituation = ifelse(
            EnrollmentID == 813537, 999, CurrentLivingSituation)
          ),
      here(paste0(directory, "data/CurrentLivingSituation.csv")),
      na = "",
      row.names = FALSE
    )
    next
  }
  
  
  if(file == "Project") {
    write.csv(
      Project %>% 
        filter(ProjectID %in% project_ids) %>%
        mutate(
          RRHSubType = ifelse(ProjectID == 1606, NA, RRHSubType)
        ),
      here(paste0(directory, "data/Project.csv")),
      na = "",
      row.names = FALSE
    )
    next
  }
  
  if(file == "ProjectCoC") {
    write.csv(
      ProjectCoC %>% 
        filter(ProjectID %in% project_ids) %>%
        mutate(
          Geocode = ifelse(ProjectID == 1377, NA, Geocode),
        ),
      here(paste0(directory, "data/ProjectCoC.csv")),
      na = "",
      row.names = FALSE
    )
    next
  }
  
  tryCatch({
    write.csv(
      get(file) %>% filter(EnrollmentID %in% enrollment_ids),
      here(paste0(directory, "data/", file, ".csv")),
      na = "",
      row.names = FALSE
    )
  }, error = function(e) {
    tryCatch({
      write.csv(
        get(file) %>% filter(PersonalID %in% personal_ids),
        here(paste0(directory, "data/", file, ".csv")),
        na = "",
        row.names = FALSE
      )
    }, error = function(e2) {
      tryCatch({
        write.csv(
          get(file) %>% filter(ProjectID %in% project_ids),
          here(paste0(directory, "data/", file, ".csv")),
          na = "",
          row.names = FALSE
        )
      }, error = function(e2) {
        write.csv(
          get(file) %>% filter(OrganizationID %in% organization_ids),
          here(paste0(directory, "data/", file, ".csv")),
          na = "",
          row.names = FALSE
        )
      })
    })
  })
}

demo_zip <- "/media/projects/projects/CE_Data_Toolkit/Data Sets/FY24-ICF-demo_small.zip"
zipr(
  zipfile = demo_zip, 
  files = list.files(here("sandbox/mini-non-shiny-environment/data"), pattern = "*.csv", full.names = TRUE),
  mode = "cherry-pick" # so the files are at the top directory
)

# Now before running Eva with this new zip, 
# put a browser() at the end of the successful load block
# then run the following to save the workspace as demo.Rdata
# save(list = c(ls(envir = .GlobalEnv, all.names = TRUE), ls(all.names = TRUE)), file = "demo.RData", compress="xz")
# # this saves everything in the global and calling environment. 
## The global environment includes the meta_ variables,
## the calling environment/observe includes everything else: functions, data frames, and values
## xz compression makes the file small enough to get around GitHub's 100MB size limit