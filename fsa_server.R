bracket_regex <- "[\\[\\]<>\\{}]"

fread_type_mapping <- list(
  "c" = "character",
  "d" = "double",
  "i" = "integer",
  "T" = "POSIXct",
  "l" = "logical",
  "D" = "Date",
  "n" = "numeric"  # Skip column
)

# Function to detect non-UTF-8 characters and brackets
detect_bracket_characters <- function(dt, file) {
  # Vectorized detection for non-UTF-8 and bracket issues
  # Identify character columns only
  char_cols <- names(dt)[sapply(dt, is.character)]

  results <- lapply(char_cols, function(col) {
    bracket_rows <- which(stringi::stri_detect_regex(dt[[col]], bracket_regex, na.rm = TRUE))
    if (length(bracket_rows) == 0) return(NULL)
    
    data.table(
      File = file,
      Location = paste0("column ", col, ", row ", bracket_rows),
      Text = dt[[col]][bracket_rows]
    )
  })
  
  rbindlist(results, use.names = TRUE, fill = TRUE)
}

bracket_files_detail <- reactive({
  req(input$imported)
  file_list <- unique(cols_and_data_types$File)
  
  col_types <- lapply(file_list, function(file) {
    types_str <- get_col_types(input$imported$datapath, file)
    types <- strsplit(types_str, "")[[1]]
    sapply(types, function(type) fread_type_mapping[[type]])
  })
  names(col_types) <- file_list
  
  withProgress(
    message = "Downloading Impermissible Character Export...", 
    value = 0,
    {
      files_with_brackets <- purrr::map_dfr(file_list, function(file) {
        # Import original data, since object names have been overwritten 
        # with windows-1252 chars removed
        filename <- utils::unzip(zipfile = input$imported$datapath, files = glue("{file}.csv"))
        dt <- fread(
          here(filename),
          colClasses = unname(col_types[[file]])
        )
        
        incProgress(1/length(file_list))
        
        possible_encodings <- readr::guess_encoding(filename)
        most_likely_encoding <- possible_encodings$encoding[
          which.max(possible_encodings$confidence)
        ]
        
        file.remove(filename) # remove the csv
        
        return(
          detect_bracket_characters(dt, file, most_likely_encoding)
        )
      })
    }
  )
  
  # Combine all data frames in the list into one data frame
  return(files_with_brackets)
})
