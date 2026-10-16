
# First log to console, so we want all the info
logToConsoleFull(session, "Running get export")

csvs <- unique(cols_and_data_types$CSV)

# for each file in the csv, loop through the file names in the csv
for (file in csvs) {
  #import the csv and save it as a data frame
  logToConsole(session, paste0("importing ", file))
  assign(file, importFile(upload_filepath, csvFile=file))
}

# Check for malformed csvs
nulls <- csvs[vapply(csvs, \(d) is.null(get(d)), logical(1))]
if(length(nulls) > 0) {
  nulls_formatted <- glue::glue_collapse(glue("{nulls}.csv"), sep = ", ", last = ", and ")

  show_invalid_popup(
    issueID = 122,
    title = "Unsuccessful Upload: Malformed CSV(s)",
    popupText = glue::glue(
      "The following CSVs are malformed, either with improper quoting or missing comma separators: {nulls_formatted}"
    )
  )
  logMetadata(session, "Unsuccessful upload - Malformed CSV(s)")
  
  intentional_stop(session, message = "Malformed CSV(s)")
}