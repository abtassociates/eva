library(fs)

# Get the app directory
app_dir <- getwd()

# Set the path to the sandbox directory
sandbox_dir <- file.path(app_dir, "sandbox/mini-non-shiny-environment")

simple_file_copy <- function(fname) {
  app_version = file.path(app_dir, fname)
  sandbox_location = file.path(sandbox_dir, fname)
  file.copy(app_version, sandbox_location)
  return(TRUE)
}

replace_code <- function(filepath, replacelist) {
  # read in the contents of the script
  lines <- readLines(filepath)

  # loop through the replacements and update the lines
  for (i in seq_along(replacelist)) {
    lines <- gsub(names(replacelist)[i], replacelist[[i]], lines)
  }
  
  # write the updated script back out to the original file
  cat(lines, file = filepath, sep = "\n")
  return(TRUE)
}

simple_file_copy("hardcodes.R")

simple_file_copy("helper_functions.R")
helper_script_path <- file.path(sandbox_dir, "helper_functions.R")
replace_code(helper_script_path,  list("importFile" = "importFileSandbox"))

simple_file_copy("02_export_dates.R")
simple_file_copy("03_file_structure_analysis.R")
simple_file_copy("04_initial_data_prep.R")
simple_file_copy("05_DataQuality.R")
simple_file_copy("05_DataQuality_functions.R")
simple_file_copy("06_PDDE_Checker.R")
simple_file_copy("client_counts_functions.R")
