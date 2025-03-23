bracket_regex <- "[\\[\\]<>\\{}]"

# Function to detect non-UTF-8 characters and brackets
detect_bracket_characters <- function(dt, file) {
  # Vectorized detection for non-UTF-8 and bracket issues
  # Identify character columns only
  char_cols <- names(dt)[sapply(dt, is.character)]
  if (length(char_cols) == 0) next
  
  results <- lapply(char_cols, function(col) {
    bracket_rows <- which(grepl(bracket_regex, dt[[col]], perl=TRUE))
    if (length(bracket_rows) == 0) return(NULL)
    
    data.table(
      File = file,
      Location = paste0("column ", col, ", row ", bracket_rows),
      Text = dt[[col]][bracket_rows]
    )
  })
  
  rbindlist(results, use.names = TRUE, fill = TRUE)
}

bracket_files_detail <- function() {
  file_list <- unique(cols_and_data_types$File)
  withProgress(
    message = "Downloading Impermissible Character Export...", {
    results <- lapply(file_list, function(file) {
      dt <- importFile(upload_filepath = input$imported$datapath, csvFile = file)  # Load file
      incProgress(1 / length(file_list))
      detect_bracket_characters(dt, file)
    })
    
    rbindlist(results, use.names = TRUE, fill = TRUE)
  })
}
