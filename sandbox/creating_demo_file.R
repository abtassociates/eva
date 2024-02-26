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
unzip("/media/projects/projects/CE_Data_Toolkit/Data Sets/FY24-ICF-hashed-current-good.zip", exdir=here("mini-non-shiny-environment/data"))
source(paste0(directory, "01_get_Export.R"))


# Org A (OrgId = 4), Org B = 6, Org N = 95
orgs <- c("4","6")
# get project IDs
project_ids <- Project %>% filter(OrganizationID %in% orgs) %>% pull(ProjectID) %>% unique()

enrollment_ids <- Enrollment %>% filter(ProjectID %in% project_ids) %>% pull(EnrollmentID) %>% unique()

personal_ids <- Enrollment %>% filter(ProjectID %in% project_ids) %>% pull(PersonalID) %>% unique()
# for each file in the csv, loop through the file names in the csv
for (file in unique(cols_and_data_types$File)) {
  print(file)
  if(file == "Export") next
  
  if(file == "Organization") {
    write.csv(
      Organization %>% filter(OrganizationID %in% orgs),
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
  
  tryCatch({
    write.csv(
      get(file) %>% filter(ProjectID %in% project_ids),
      here(paste0(directory, "data/", file, ".csv")),
      na = "",
      row.names = FALSE
    )
  }, error = function(e) {
    tryCatch({
      write.csv(
        get(file) %>% filter(EnrollmentID %in% enrollment_ids),
        here(paste0(directory, "data/", file, ".csv")),
        na = "",
        row.names = FALSE
      )
    }, error = function(e2) {
      write.csv(
        get(file) %>% filter(PersonalID %in% personal_ids),
        here(paste0(directory, "data/", file, ".csv")),
        na = "",
        row.names = FALSE
      )
    })
  })
}

demo_zip <- "/media/projects/projects/CE_Data_Toolkit/Data Sets/FY24-ICF-demo_small.zip"
zipr(
  zipfile = demo_zip, 
  files = list.files(here("sandbox/mini-non-shiny-environment/data"), pattern = "*.csv", full.names = TRUE),
  mode = "cherry-pick" # so the files are at the top directory
)

# Now run Eva and upload this file, pausing after 06_PDDE to save the environment
# save(list = c(ls(envir = .GlobalEnv, all.names = TRUE), ls(all.names = TRUE)), file = "demo.RData", compress="xz")
test_that("Creasting demo.Rdata", {
  app <- AppDriver$new(
    variant = platform_variant(os_name = FALSE), 
    name = "build_demo_rdata", 
    seed = 12345,
    load_timeout = 2e+05)
  
  app$set_inputs(Go_to_upload = "click")
  app$wait_for_idle(timeout = 2e+05)
  app$upload_file(imported = demo_zip)
  
  app$wait_for_idle(timeout = 1e+06)
  save(list = c(ls(envir = .GlobalEnv, all.names = TRUE), ls(all.names = TRUE)), file = "demo.RData", compress="xz")
})