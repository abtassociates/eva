bracket_regex <- "\\[|\\]|\\<|\\>|\\{|\\}"

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
detect_bracket_characters <- function(dt, file, encoding) {
  # Vectorized detection for non-UTF-8 and bracket issues
  # Identify character columns only
  char_cols <- names(dt)[sapply(dt, is.character)]

  # Create a list of problematic entries
  problematic_characters <- purrr::map_dfr(char_cols, function(col) {
    bracket_rows <- stringi::stri_extract_all_regex(dt[[col]], bracket_regex, omit_no_match = TRUE) %>%
      purrr::map_lgl( ~ length(.) > 0) # Check if any match
    
    # Create tibbles for invalid rows to store the problematic text and cell location
    if (any(bracket_rows)) {
      row_indices <- which(bracket_rows)
      tibble::tibble(
        File = file,
        Location = paste0("column ", col, ", row ", row_indices),
        Text = dt[[col]][row_indices]
      )
    }
  })
  
  return(problematic_characters)
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
