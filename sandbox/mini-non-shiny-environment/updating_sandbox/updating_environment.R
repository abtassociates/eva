library(fs)
library(here)

# # Get the app directory
# app_dir <- "~/DQ_CE/"

# Set the path to the sandbox directory
sandbox_dir <- here("sandbox/mini-non-shiny-environment/")

replace_code <- function(fname, replacelist) {
  filepath <- paste0(sandbox_dir, fname)
  # read in the contents of the script
  lines <- readLines(filepath)

  # loop through the replacements and update the lines
  for (i in seq_along(replacelist)) {
    lines <- gsub(names(replacelist)[i], replacelist[[i]], lines)
  }
  
  # Remove lines containing the pattern logToConsole("SOME TEXT HERE")
  pattern <- "logToConsole\\(\".*\"\\)"
  remove_lines <- grepl(pattern, lines)
  lines <- lines[!remove_lines]
  
  # write the updated script back out to the original file
  writeLines(lines, filepath, sep = "\n")
  # write the updated script back out to the original file
  # cat(lines, file = filepath, sep = "\n")
}

files_to_copy <- list(
  "/hardcodes.R",
  "/helper_functions.R",
  "/01_get_Export.R",
  "/02_export_dates.R",
  "/03_file_structure_analysis.R",
  "/04_initial_data_prep.R",
  "/client_counts_functions.R",
  "/05_DataQuality_functions.R",
  "/05_DataQuality.R",
  "/06_PDDE_Checker.R",
  "/07_System_overview.R",
  "/09_system_status.R",
  "/system_composition_server.R",
  "/system_inflow_outflow_server.R",
  "/system_status_server.R"
)

file.copy(from = paste0(here(), files_to_copy),
          to = paste0(sandbox_dir, files_to_copy),
          overwrite = TRUE)

# intentional code adjustments for differences between Sandbox and Regular.
for(i in files_to_copy) {
  if(i == "/01_get_Export.R") {
    # the sandbox/mini-non-shiny-environment/data folder already contains the csvs from the FY24 Hashed Current good
    # so the import process doesn't require potentially including a zip file upload path
    replace_code("/01_get_Export.R",  list("importFile(upload_filepath, " = "importFileSandbox("))
  } else {
    replace_code(i,  list())
  }
}
