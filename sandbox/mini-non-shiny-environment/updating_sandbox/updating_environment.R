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
  "/04_initial_data_prep.R"
)

file.copy(from = paste0(here(), files_to_copy),
          to = paste0(sandbox_dir, files_to_copy),
          overwrite = TRUE)

replace_code("01_get_Export.R",  list("importFile" = "importFileSandbox"))
replace_code("02_export_dates.R",  list())
replace_code("03_file_structure_analysis.R",  list())
replace_code("04_initial_data_prep.R",  list())
