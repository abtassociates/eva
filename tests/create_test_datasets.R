#
# PURPOSE: Create test datasets, to be used for automated testing
#
library(tidyverse)
library(zip)
library(here)
library(collapse)
library(data.table)
source(here("hardcodes.R"), local = TRUE)
source(here("helper_functions.R"), local = TRUE)
source(here("load_machine_readable_specs.R"))

# unzip main test data to temp directory. 
# this will allow us to overwrite individual csv files
unzip(zipfile = here("tests/FY26-test-good.zip"),
      exdir = here("tests/temp"))

# function to save a directory of CSVs as a zip file for upload
save_new_zip <- function(zipfname, files_directory) {
  system("sync")
  zipr(
    zipfile = here(paste0("tests/temp/",zipfname)), 
    files = list.files(here(paste0("tests/temp/",files_directory)), pattern = "*.csv$", full.names = TRUE),
    mode = "cherry-pick" # so the files are at the top directory
  )
  Sys.sleep(1)
}

# store the original data as an R data set, so we can modify from scratch each time
csv_files <- list.files(here("tests/temp"), pattern = "*.csv$",
                        full.names = TRUE)

names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))
original_data <- lapply(names(csv_files), importFile, upload_filepath = here("tests/FY26-test-good.zip"))
names(original_data) <- tools::file_path_sans_ext(basename(csv_files))

# AS 2/10/26: Commenting this out because with the new machine-readable specs, we're checking these files, too
# remove unused files
# original_data <- original_data[!names(original_data) %in% c("Affiliation",
#                                                             "AssessmentResults",
#                                                             "AssessmentQuestions",
#                                                             "Disabilities")]
# store a reduced-size dataset (1 row per csv file)
# we don't need so much data for initially valid import checks
reduced_data <- lapply(original_data, function(x) x[1, ])

dir.create(here("tests/temp/reduced"), showWarnings = FALSE)

lapply(names(reduced_data), function(fname) {
  write.csv(
    reduced_data[[fname]], 
    paste0(here("tests/temp/reduced//"), fname, ".csv"), 
    row.names = FALSE, 
    na = "")
})

reduced_files <- list.files(here("tests/temp/reduced"), pattern = "*.csv",
                            full.names = TRUE)
names(reduced_files) <- tools::file_path_sans_ext(basename(reduced_files))

############### INITIALLY VALID IMPORT TESTS #################
# Unhashed ---------------------------------------------------
data <- reduced_data[["Export"]]
data$HashStatus <- 1
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na = "")
Sys.sleep(1)
save_new_zip("FY26-test-unhashed.zip", "reduced")

# CSVVersion -------------------------------------------------
data <- reduced_data[["Export"]]
data$CSVVersion <- '2022 v1'
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na = "")
Sys.sleep(1)
save_new_zip("FY26-test-wrong-csv-version.zip", "reduced")

# Missing Export (APR or LSA) --------------------------------
file.remove(reduced_files[["Export"]])
save_new_zip("FY26-test-missing-export.zip", "reduced")
write.csv(reduced_data[["Export"]], reduced_files[["Export"]], row.names=FALSE, na = "") # bring export dataset back
Sys.sleep(3)

# Missing Files ----------------------------------------------
file.remove(reduced_files[["Enrollment"]])
file.remove(reduced_files[["Exit"]])
save_new_zip("FY26-test-missing-multiple-files.zip", "reduced")
write.csv(reduced_data[["Enrollment"]],
          reduced_files[["Enrollment"]],
          row.names = FALSE,
          na = "")
write.csv(reduced_data[["Exit"]],
          reduced_files[["Exit"]],
          row.names = FALSE,
          na = "")
Sys.sleep(1)

# Wrong File Type
gz1 <- gzfile(here("tests/temp/FY26-test-wrong-file-type.gz"), "w")
write.csv(data.frame(), gz1)
Sys.sleep(1)
close(gz1)

############### VALID FILES #################
# Initial Fixes to pass machine-readable specs
if("TcellCount" %in% names(original_data$Disabilities))
  original_data$Disabilities <- original_data$Disabilities |>
    frename(
      TCellCount = TcellCount,
      TCellSource = TcellSource
    )

original_data$Disabilities <- original_data$Disabilities |>
  fsubset(EnrollmentID != "696923") # fixes non-matching enrollment issue

original_data$Enrollment <- original_data$Enrollment |>
  colorder(DateCreated, DateUpdated, UserID, DateDeleted, ExportID, pos="end") # fixes MentalHealthConsultation being out-of-order

original_data$Client <- original_data$Client |>
  colorder(DOBDataQuality, Sex, pos="after") # fixes Sex being out-of-order

original_data$Inventory <- original_data$Inventory |>
  fsubset(!InventoryID %in% c("4627", "4626"))

original_data$Services <- original_data$Services |>
  fmutate(
    SubTypeProvided = fifelse(
      ServicesID %in% c("4701489","4619566","4616611","4594911","4630274","4271320","4265356","4352817","4352818","4593821","4593825","4481912","4668968","4593619","4702917","4702916","4619563","4618136","4641247","4629927","4649143","4627720","4656719","4658078","4658077","4708744","4710028","4737855"),
      1,
      fifelse(
        ServicesID %in% c("4458004","4653986", "4654974", "4668954","4707540"),
        11,
        SubTypeProvided
      )
    )
  )
  
# FSA ---------------------------------------------------
reduced_data_fsa <- lapply(original_data, function(x) x[ifelse(nrow(x) >= 6, 6, 1)])
source(here("tests/update_test_good_fsa.R"), local = TRUE)

dir.create(here("tests/temp/reduced_fsa"), showWarnings = FALSE)
lapply(names(reduced_data_fsa), function(fname) {
  write.csv(reduced_data_fsa[[fname]],
            paste0(here("tests/temp/reduced_fsa//"),
                   fname, ".csv"),
            row.names = FALSE, na="")
  Sys.sleep(1)
})
Sys.sleep(1)
save_new_zip("FY26-test-fsa.zip", "reduced_fsa")

# DQ AND PDDE ---------------------------------------------------
source(here("tests/update_test_good_dq.R"), local = TRUE)

# overwrite the original csv files in temp
mapply(function(df, df_name) {
  write.csv(df,
            file= file(csv_files[[df_name]], encoding = if(df_name == "Project") "Windows-1252" else "UTF-8"),
            row.names = FALSE,
            na = "")
}, original_data, names(original_data), SIMPLIFY = FALSE)

save_new_zip("FY26-test-main-valid.zip", "")

print("done creating test datasets")
