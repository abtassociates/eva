#
# PURPOSE: Create test datasets, to be used for automated testing
#
library(tidyverse)
library(zip)
library(here)
source(here("hardcodes.R"), local = TRUE)
source(here("helper_functions.R"), local = TRUE)

# unzip main test data to temp directory. 
# this will allow us to overwrite individual csv files
unzip(zipfile = here("tests/FY24-ICF-hashed-current-good.zip"),
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
csv_files <- list.files(here("tests/temp/"), pattern = "*.csv$",
                        full.names = TRUE)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))
original_data <- lapply(csv_files, data.table::fread)

# remove unused files
original_data <- original_data[!names(original_data) %in% c("Affiliation",
                                                            "AssessmentResults",
                                                            "AssessmentQuestions",
                                                            "Disabilities")]
# store a reduced-size dataset (1 row per csv file)
# we don't need so much data for initially valid import checks
reduced_data <- lapply(original_data, function(x) if(nrow(x)) x[1, ])

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
save_new_zip("FY24-ICF-unhashed.zip", "reduced")

# CSVVersion -------------------------------------------------
data <- reduced_data[["Export"]]
data$CSVVersion <- '2022 v1'
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na = "")
Sys.sleep(1)
save_new_zip("FY24-ICF-wrong-csv-version.zip", "reduced")

# Missing Export (APR or LSA) --------------------------------
file.remove(reduced_files[["Export"]])
save_new_zip("FY24-ICF-missing-export.zip", "reduced")
write.csv(reduced_data[["Export"]], reduced_files[["Export"]], row.names=FALSE, na = "") # bring export dataset back
Sys.sleep(3)

# Missing Files ----------------------------------------------
file.remove(reduced_files[["Enrollment"]])
file.remove(reduced_files[["Exit"]])
save_new_zip("FY24-ICF-missing-multiple-files.zip", "reduced")
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
gz1 <- gzfile(here("tests/temp/FY24-ICF-wrong-file-type.gz"), "w")
write.csv(data.frame(), gz1)
Sys.sleep(1)
close(gz1)
############### VALID FILES #################
# FSA ---------------------------------------------------
reduced_data_fsa <- lapply(original_data, function(x) if(nrow(x)) x[ifelse(nrow(x) >= 6, 6, 1)])
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
save_new_zip("FY24-ICF-fsa-test.zip", "reduced_fsa")

# DQ AND PDDE ---------------------------------------------------
# convert to data.frame and fix column types
original_data_fixed_cols <- mapply(function(df, df_name) {
  existing_cols <- intersect(cols_and_data_types$Column, names(df))
  
  as.data.frame(df) %>%
    mutate(across(all_of(existing_cols), 
                  .fns = ~ {
                    dtype <- cols_and_data_types$DataType[
                      cols_and_data_types$Column == cur_column() &
                        cols_and_data_types$File == df_name
                    ]
                    switch(dtype,
                           "character" = as.character(.),
                           "numeric" = as.numeric(.),
                           "integer" = as.integer(.),
                           "factor" = as.factor(.),
                           "logical" = as.logical(.),
                           "date" = as.Date(.),
                           .)
                  }))
}, original_data, names(original_data), SIMPLIFY = FALSE)

source(here("tests/update_test_good_dq.R"), local = TRUE)

# overwrite the original csv files in temp
mapply(function(df, df_name) {
  write.csv(df,
            file= file(csv_files[[df_name]], encoding = if(df_name == "Project") "Windows-1252" else "UTF-8"),
            row.names = FALSE,
            na = "")
}, original_data_fixed_cols, names(original_data_fixed_cols), SIMPLIFY = FALSE)

save_new_zip("FY24-ICF-main-valid.zip", "")

print("done creating test datasets")
