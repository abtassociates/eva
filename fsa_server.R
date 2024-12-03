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
detect_problematic_and_bracket_characters <- function(dt, file) {
  problematic_characters <- list()
  for (col in names(dt)) {
    if (is.character(dt[[col]])) {
      # Detect non-UTF-8 characters
      invalid_utf8_rows <- which(!stringi::stri_enc_isutf8(dt[[col]]))
      if (length(invalid_utf8_rows) > 0) {
        problematic_characters$utf8_issues <- lapply(invalid_utf8_rows, function(row) {
          list(
            row = row, 
            col = col,
            chars= iconv(
              unlist(stringi::stri_extract_all_regex(dt[[col]][row], "[\x80-\xFF]")),
              from = "WINDOWS-1252",
              to = "UTF-8"
            )
          )
        })
      }
      
      # Detect brackets
      bracket_rows <- stringi::stri_extract_all_regex(dt[[col]], bracket_regex, omit_no_match = TRUE)
      if (any(sapply(bracket_rows, function(x) length(x) > 0))) {
        problematic_characters$brackets <- Filter(Negate(is.null), lapply(
            seq_along(bracket_rows), 
            function(i) {
              if(length(bracket_rows[[i]] > 0)) list(
                row = i, 
                col = col,
                chars = unlist(bracket_rows[[i]])
              )
            }
          )
        )
      }
    }
  }
  if(length(problematic_characters) > 0) {
    details <- data.frame()
    for (issue_type in names(problematic_characters)) {
      for (x in problematic_characters[[issue_type]]) {
        details <- rbind(
          details,
          data.frame(
            File = file,
            Detail = paste0(
              "Found impermissible character(s) in ", 
              file, ".csv, ", 
              "column ", x$col, 
              ", line ", x$row, 
              ": ", paste(x$chars, collapse=", ")
            ),
            stringsAsFactors = FALSE
          )
        )
      }
    }
    return(details)
  }
}

non_utf8_files_detail <- reactive({
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
      # Initialize an empty list to store the results
      files_with_non_utf8 <- list()
      
      fnum <- length(file_list)
      files_with_non_utf8 <- lapply(file_list, function(file) {
        # Import original data, since object names have been overwritten 
        # with windows-1252 chars removed
        filename <- utils::unzip(zipfile = input$imported$datapath, files = glue("{file}.csv"))
        dt <- fread(
          here(filename),
          colClasses = unname(col_types[[file]]),
          na.strings = "NA"
        )
        file.remove(filename)
        
        incProgress(1/fnum)
        
        return(
          detect_problematic_and_bracket_characters(dt, file)
        )
      })
    }
  )
  
  # Combine all data frames in the list into one data frame
  return(
    bind_rows(files_with_non_utf8) %>% 
      merge_check_info(checkIDs = 134) %>%
      select(all_of(issue_display_cols))
  )
})
