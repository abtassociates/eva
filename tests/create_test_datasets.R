################################
# PURPOSE: Create test datasets, to be used for automated testing
################################
library(tidyverse)
library(zip)
source("hardcodes.R", local = TRUE)
source("helper_functions.R", local = TRUE)

setwd("tests")

# start with the main test dataset
unzip("FY24-ICF-hashed-current-good.zip", exdir = "temp")

# function to save the file as a zip file
save_new_zip <- function(zipfname, files_directory) {
    zipr(
        zipfile = zipfname, 
        files = list.files(files_directory, pattern = "*.csv", full.names = TRUE),
        mode = "cherry-pick" # so the files are at the top directory
    )
}
# store the original data so we can modify from scratch each time
csv_files <- list.files("temp", pattern = "*.csv", full.names = TRUE)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))
original_data <- lapply(csv_files, data.table::fread)

# store a reduced-size dataset
# we don't need so much data for initially valid import checks
reduced_data <- lapply(original_data, function(x) if(nrow(x)) x[1, ])
dir.create("temp/reduced")
lapply(names(reduced_data), function(fname) {
    write.csv(reduced_data[[fname]], paste0("temp/reduced/",fname, ".csv"), row.names = FALSE, na="")
})
reduced_files <- list.files("temp/reduced", pattern = "*.csv", full.names = TRUE)
names(reduced_files) <- tools::file_path_sans_ext(basename(reduced_files))

############### INITIALLY VALID IMPORT TESTS #################
# Unhashed ---------------------------------------------------
data <- reduced_data[["Export"]]
data$HashStatus <- 1
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na="")
save_new_zip("temp/FY24-ICF-unhashed.zip", "temp/reduced")

# CSVVersion -------------------------------------------------
data <- reduced_data[["Export"]]
data$CSVVersion <- '2022 v1'
write.csv(data, reduced_files[["Export"]], row.names = FALSE, na="")
save_new_zip("temp/FY24-ICF-wrong-csv-version.zip", "temp/reduced")

# Wrong File Type --------------------------------------------
save_new_zip("temp/FY24-ICF-wrong-file-type.gz", "temp/reduced")

# Missing Export (APR or LSA) --------------------------------
file.remove(reduced_files[["Export"]])
save_new_zip("temp/FY24-ICF-missing-export.zip", "temp/reduced")
write.csv(reduced_data[["Export"]], reduced_files[["Export"]], na="") # bring export dataset back

# Missing Files ----------------------------------------------
file.remove(reduced_files[["Enrollment"]])
file.remove(reduced_files[["Exit"]])
save_new_zip("temp/FY24-ICF-missing-multiple-files.zip", "temp/reduced")
write.csv(reduced_data[["Enrollment"]], reduced_files[["Enrollment"]], row.names = FALSE, na="")
write.csv(reduced_data[["Exit"]], reduced_files[["Exit"]], row.names = FALSE, na="")

setwd("..")

print("done creating test datasets")