bracket_regex <- "\\[|\\]|\\<|\\>|\\{|\\}"

# Function to detect non-UTF-8 characters and brackets
detect_bracket_characters <- function(dt, file, encoding) {
  # Vectorized detection for non-UTF-8 and bracket issues
  # Identify character columns only
  char_cols <- names(dt)[sapply(dt, is.character)]

  # Create a list of problematic entries
  problematic_characters <- purrr::map_dfr(char_cols, function(col) {
    if(all(is.na(dt[[col]]))) return(tibble::tibble())
    bracket_rows <- stringi::stri_detect_regex(dt[[col]], bracket_regex)
    
    # Create tibbles for invalid rows to store the problematic text and cell location
    if (any(bracket_rows, na.rm = TRUE)) {
      row_indices <- which(bracket_rows)
      tibble::tibble(
        File = file,
        Location = paste0("column ", col, ", row ", row_indices),
        Text = dt[[col]][row_indices]
      )
    } else {
      tibble::tibble()
    }
  })
  
  return(problematic_characters)
}

bracket_files_detail <- reactive({
  req(input$imported)
  file_list <- unique(cols_and_data_types$File)
  
  withProgress(
    message = "Downloading Impermissible Character Export...", {
    results <- lapply(file_list, function(file) {
      dt <- importFile(file)  # Load file
      incProgress(1 / length(file_list))
      detect_bracket_characters(dt, file)
    })
    
    rbindlist(results, use.names = TRUE, fill = TRUE)
  })
})
