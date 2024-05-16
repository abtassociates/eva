################################
# PURPOSE: Create test datasets, to be used for automated testing
################################
library(tidyverse)
library(zip)
source(here("hardcodes.R"), local = TRUE)
source(here("helper_functions.R"), local = TRUE)

# unzip main test data to temp directory. 
# this will allow us to overwrite individual csv files
unzip(here("tests/FY24-ICF-hashed-current-good.zip"), exdir = here("tests/temp"))

# function to save a directory of CSVs as a zip file for upload
save_new_zip <- function(zipfname, files_directory) {
  zipr(
    zipfile = paste0(here("tests/temp/"),zipfname), 
    files = list.files(paste0(here("tests/temp/"),files_directory), pattern = "*.csv", full.names = TRUE),
    mode = "cherry-pick" # so the files are at the top directory
  )
}

# store the original data as an R data set, so we can modify from scratch each time
csv_files <- list.files(here("tests/temp"), pattern = "*.csv$", full.names = TRUE)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))
original_data <- lapply(csv_files, data.table::fread)

# store a reduced-size dataset (1 row per csv file)
# we don't need so much data for initially valid import checks
reduced_data <- lapply(original_data, function(x) if(nrow(x)) x[1, ])

dir.create(here("tests/temp/reduced"))

lapply(names(reduced_data), function(fname) {
  write.csv(
    reduced_data[[fname]], 
    paste0(here("tests/temp/reduced/"),fname, ".csv"), 
    row.names = FALSE, 
    na="")
})

reduced_files <- list.files(here("tests/temp/reduced"), pattern = "*.csv", full.names = TRUE)
names(reduced_files) <- tools::file_path_sans_ext(basename(reduced_files))

############### INITIALLY VALID IMPORT TESTS #################
# Unhashed ---------------------------------------------------
data <- reduced_data[["Export"]]
data$HashStatus <- 1
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na = "")
save_new_zip("FY24-ICF-unhashed.zip", "reduced")

# CSVVersion -------------------------------------------------
data <- reduced_data[["Export"]]
data$CSVVersion <- '2022 v1'
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na = "")
save_new_zip("FY24-ICF-wrong-csv-version.zip", "reduced")

# Missing Export (APR or LSA) --------------------------------
file.remove(reduced_files[["Export"]])
save_new_zip("FY24-ICF-missing-export.zip", "reduced")
write.csv(reduced_data[["Export"]], reduced_files[["Export"]], na = "") # bring export dataset back

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

# Wrong File Type
gz1 <- gzfile(here("tests/temp/FY24-ICF-wrong-file-type.gz"), "w")
write.csv(data.frame(), gz1)
close(gz1)

################# FSA ######################
reduced_data_fsa <- lapply(original_data, function(x) if(nrow(x)) x[6, ])
dir.create(here("tests/temp/reduced_fsa"))
lapply(names(reduced_data_fsa), function(fname) {
    write.csv(reduced_data_fsa[[fname]],
              paste0(here("tests/temp/reduced_fsa/"),
                     fname, ".csv"),
              row.names = FALSE, na="")
})
save_new_zip("FY24-ICF-fsa-test.zip", "reduced_fsa")

print("done creating test datasets")
