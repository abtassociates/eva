
# Age ---------------------------------------------------------------------

age_years <- function(earlier, later)
{
  floor(decimal_date(later) - decimal_date(earlier))
  
}

# Display Helpers ---------------------------------------------------------

living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client prefers not to answer",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 99 ~ "Data not collected",
    ReferenceNo == 101 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 116 ~ "Place not meant for habitation",
    ReferenceNo == 118 ~ "Safe Haven",
    ReferenceNo == 204 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 205 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 206 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 207 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 225 ~ "Long-term care facility or nursing home",
    ReferenceNo == 215 ~ "Foster care home of foster care group home",
    ReferenceNo == 327 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 302 ~ "Transitional housing",
    ReferenceNo == 332 ~ "Host Home (non-crisis)",
    ReferenceNo == 329 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 312 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 313 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 314 ~ "H/Motel paid for by household",
    ReferenceNo == 313 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 335 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 336 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 335 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 423 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 422 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 435 ~ "Rental by client, with ongoing housing subsidy",
    ReferenceNo == 410 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 426 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 421 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 411 ~ "Owned by client, no ongoing housing subsidy"
  )
}

rental_subsidy_types <- function(ReferenceNo){
  case_when(
    ReferenceNo == 419 ~ "VASH",
    ReferenceNo == 420 ~ "Other subsidy",
    ReferenceNo == 428 ~ "GPD TIP",
    ReferenceNo == 431 ~ "RRH or equivalent",
    ReferenceNo == 433 ~ "HCV vouncer (tenant or project based) (not dedicated)",
    ReferenceNo == 434 ~ "Public housing unit",
    ReferenceNo == 436 ~ "Emergency Housing Voucher",
    ReferenceNo == 437 ~ "Family Unification Program Voucher (FUP)",
    ReferenceNo == 438 ~ "Foster Youth to Independence Initiative (FYI)",
    ReferenceNo == 439 ~ "Permanent Supportive Housing",
    ReferenceNo == 440 ~ "Other permanent housing dedicated for formerly homeless persons",
  )
}

project_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Emergency Shelter (NbN)",
    ReferenceNo == 0 ~ "Emergency Shelter (E/E)",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 7 ~ "Other",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 9 ~ "PH - Housing Only",
    ReferenceNo == 10 ~ "PH - Housing with Services",
    ReferenceNo == 11 ~ "Day Shelter",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
  )
}

project_type_abb <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "ES (NbN)",
    ReferenceNo == 0 ~ "ES (E/E)",
    ReferenceNo == 2 ~ "TH",
    ReferenceNo == 3 ~ "PSH",
    ReferenceNo == 4 ~ "OUT",
    ReferenceNo == 6 ~ "SSO",
    ReferenceNo == 7 ~ "Other",
    ReferenceNo == 8 ~ "SH",
    ReferenceNo == 9 ~ "OPH no Svcs",
    ReferenceNo == 10 ~ "OPH w/ Svcs",
    ReferenceNo == 11 ~ "DAY",
    ReferenceNo == 12 ~ "HP",
    ReferenceNo == 13 ~ "RRH",
    ReferenceNo == 14 ~ "CE"
  )
}

rel_to_hoh <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "HoH",
    ReferenceNo == 2 ~ "HoHs child",
    ReferenceNo == 3 ~ "HoHs partner/spouse",
    ReferenceNo == 4 ~ "HoHs other relation",
    ReferenceNo == 5 ~ "Non-relation member",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

enhanced_yes_no_translator <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 0 ~ "No",
    ReferenceNo == 1 ~ "Yes",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client declined",
    ReferenceNo == 99 ~ "Data not collected",
    TRUE ~ "something's wrong"
  )
}

translate_HUD_yes_no <- function(column_name){
  case_when(
    column_name == 1 ~ "Yes", 
    column_name == 0 ~ "No",
    column_name %in% c(dkr_dnc) ~ "Unknown",
    TRUE ~ "something's wrong"
  )
}

# Translate to Values -----------------------------------------------------

replace_yes_no <- function(column_name) {
  case_when(column_name == "No" | is.na(column_name) ~ 0,
            column_name == "Yes" ~ 1,
            TRUE ~ "something's wrong")
}

# Import Helper -----------------------------------------------------------

parseDate <- function(datevar) {
  newDatevar <- parse_date_time(datevar,
                                orders = c("Ymd", "mdY"))
  return(newDatevar)
}

importFile <- function(upload_filepath = NULL, csvFile, guess_max = 1000) {
  if(isTRUE(str_sub(upload_filepath, -4, -1) != ".zip")) {
    capture.output("User tried uploading a non-zip file!") 
  }

  filename <- str_glue("{csvFile}.csv")
  if(!is.null(upload_filepath))
    filename = utils::unzip(
      zipfile = upload_filepath, 
      files=filename, 
      exdir=dirname(tempfile())
    )
  filename <- paste0(tempdir(), "/", basename(filename))
  
  colTypes <- get_col_types(upload_filepath, csvFile)
  
  # import data
  data <- data.table::fread(
    filename,
    colClasses = unlist(unname(colTypes)),
    na.strings="NA"
  )
  
  # handle dates - new data.table converts to IDate, but we want "Date" for FSA
  data[, (names(data)) := lapply(.SD, function(x) {
    if (is.character(x)) x[x == ""] <- NA
    else if (inherits(x, "IDate")) x <- as.Date(x)
    return(x)
  }), .SDcols = names(data)]
  
  if(csvFile == "Client") {
    ignore_cols <- unlist(strsplit(Sys.getenv("IGNORE_COLUMNS"), ","))
    data[, ignore_cols] <- NULL
  }
  
  # Convert to dataframe
  setDF(data)

  if(csvFile != "Export" & "DateDeleted" %in% colnames(data)){
    data <- data %>%
      filter(is.na(DateDeleted))
  }
  
  attr(data, "encoding") <- guess_encoding(filename)$encoding[1]
  data <- convert_data_to_utf8(data)
  
  # remove the csv
  file.remove(filename)
  return(data)
}

get_col_types <- function(upload_filepath, file) {
  # returns the datatypes as a named list, using data.table::fread column types,
  # based on the order of the columns in the imported file, rather than the expected order
  # get the column data types expected for the given file
  col_types <- cols_and_data_types %>%
    filter(File == file) %>%
    mutate(DataType = data_type_mapping[as.character(DataType)])
  
  cols_in_file <- colnames(read.table(
    paste0(tempdir(), "/", file, ".csv"),
    head = TRUE,
    nrows = 1,
    sep = ",", 
    comment.char = ""))
  
  # get the data types for those columns
  data_types <- sapply(cols_in_file, function(col_name) {
    ifelse(col_name %in% col_types$Column,
           col_types$DataType[col_types$Column == col_name],
           "character")
  })

  return(data_types)
}

logMetadata <- function(detail) {
  d <- data.frame(
    SessionToken = session$token,
    Datestamp = Sys.time(),
    Details = detail
  )
  
  filename <- here("metadata-analysis/metadata/metadata.csv")
  
  invisible(write_csv(
    x = d,
    filename,
    append = TRUE,
    col_names = !file.exists(filename)
  ))
}

headerGeneric <- function(tabTitle, extraHTML = NULL) {
  renderUI({
    if(valid_file() == 1) {
      list(h2(tabTitle),
           h4(strong("Date Range of Current File: "),
            paste(
             format(meta_HUDCSV_Export_Start(), "%m-%d-%Y"),
             "to",
             format(meta_HUDCSV_Export_End(), "%m-%d-%Y")
           )),
           extraHTML
      )
    } else {
      h4("This tab will show relevant data once you have uploaded your HMIS CSV Export.")
    }
  })
}

logSessionData <- function() {

  d <- data.frame(
    SessionToken = session$token,
    Datestamp = Sys.time(),
    CoC = Export()$SourceID,
    ExportID = Export()$ExportID,
    SourceContactFirst = Export()$SourceContactFirst,
    SourceContactLast = Export()$SourceContactLast,
    SourceContactEmail = Export()$SourceContactEmail,
    SoftwareName = Export()$SoftwareName,
    ImplementationID = Export()$ImplementationID
  )
  
  # put the export info in the log
  capture.output(d, file = stderr())
  
    
  filename <- here("metadata-analysis/metadata/sessiondata.csv")
  write_csv(
    x = d,
    filename,
    append = TRUE,
    col_names = !file.exists(filename)
  )
}

logToConsole <- function(msg) {
  d <- data.frame(
    SessionToken = session$token,
    Datestamp = Sys.time(),
    CoC = Export()$SourceID,
    ExportID = Export()$ExportID,
    Msg = msg
  )
  capture.output(d, file = stderr())
}
  
date_stamped_filename <- function(filename) {
  paste(filename, Sys.Date(), ".xlsx", sep = "")
}

nice_names <- function(df){
  
  names_from_janitor <- c("Ho h", "Co c", "Adjust")
  hmis_abbreviations <- c("HoH", "CoC", "")
  
  names(hmis_abbreviations) <- names_from_janitor
  
  df_names <- df %>%
    clean_names(
      "title",
      abbreviations = c("ID",
                        "ESSH",
                        "AMI",
                        "HH",
                        "VAMC",
                        "HP",
                        "RRH",
                        "PSH",
                        "ES",
                        "TH",
                        "SH",
                        "CE",
                        "CSV")) %>%
    colnames()
  
  final_names <- str_replace_all(df_names, hmis_abbreviations)
  
  colnames(df) <- final_names
  
  df
}


# Old to New Living SItuations --------------------------------------------

fy22_to_fy24_living_situation <- function(value){
  case_when(
    value %in% c(3, 19, 20, 28, 31, 33, 34, 38, 39) ~ 435,
    value %in% c(1, 16, 18) ~ value + 100,
    value %in% c(4, 5, 6, 7, 15, 25) ~ value + 200,
    value %in% c(2, 12, 13, 14, 27, 29, 32, 35, 36) ~ value + 300,
    value %in% c(10, 11, 21, 22, 23, 26) ~ value + 400,
    value %in% c(8, 9, 17, 24, 30, 37, 99) ~ value,
    is.na(value) ~ NA,
    TRUE ~ 0 # 0 would mean something's wrong
  )
}


# Sandbox -----------------------------------------------------------------

importFileSandbox <- function(csvFile) {
  filename = str_glue("{csvFile}.csv")
  data <- read_csv(paste0(directory, "data/", filename)
                   ,col_types = get_col_types(csvFile)
                   ,na = ""
  )
  return(data)
}


# Generate check data from evachecks.csv ----------------------------------

merge_check_info <- function(data, checkIDs) {
  return(data %>%
    bind_cols(
      evachecks %>% filter(ID %in% c(checkIDs))
    )
  )
}
merge_check_info_dt <- function(data, checkIDs) {
  return(
    cbind(
      data,
      as.data.table(evachecks)[ID %in% c(checkIDs)]
    )
  )
}


############################
# CUSTOM Rprof() FUNCTION
############################
custom_rprof <- function(expr, source_file_name, code_block_name = NULL) {
  startTime <- Sys.time()
  
  # Start profiling
  Rprof(tmp <- tempfile(), line.profiling = TRUE, numfiles=500L)
  
  # Evaluate the expression
  eval(expr)
  if(is.null(code_block_name)) {
    print(paste0(source_file_name, " took: "))
  } else {
    print(paste0(code_block_name, " in ", source_file_name, " took: "))
  }
  # Stop profiling
  Rprof(NULL)
  
  print(Sys.time() - startTime)
  
  # Get profiling summaries
  x <- summaryRprof(tmp, lines = "show")$by.total
  
  # Filter rows related to the source file
  x_final <- x[grepl(source_file_name, row.names(x)), ]
  
  # Order by time
  # x_final <- x_final[order(-x_final$total.time),]
  
  # Print the final results
  print(x_final)
  
  # Remove the temporary file
  unlink(tmp)
  unlink("Rprof.out")
}


# Misc --------------------------------------------------------------------
reset_postvalid_components <- function() {
  dq_main_df(NULL)
  session$sendInputMessage('orgList', list(choices = NULL))
  session$sendInputMessage('currentProviderList', list(choices = NULL))
  session$sendCustomMessage('dateRangeCount', list(
    min = NULL,
    start = ymd(today()),
    max = NULL,
    end = ymd(today())
  ))
  pdde_main(NULL)
  
  shinyjs::hide("sys_inflow_outflow_download_btn")
  shinyjs::hide("sys_inflow_outflow_download_btn_ppt")
  
  shinyjs::hide("sys_status_download_btn")
  shinyjs::hide("sys_status_download_btn_ppt")
  
  shinyjs::hide("sys_comp_download_btn")
  shinyjs::hide("sys_comp_download_btn_ppt")
}

# essentially resets the app
reset_app <- function() {
  lapply(visible_reactive_vals, function(r) r(NULL))
  valid_file(0)
  initially_valid_import(0)
  windowSize(input$dimension)
  reset_postvalid_components()
}

getNameByValue <- function(vector, val) {
  return(
    sub(
      "^[^.]+\\.",
      "",
      paste(names(unlist(vector))[unlist(vector) == val], collapse = ", ")
    )
  )
}

# for a set of 1/0, or checkbox, variables, check whether no other variables 
# were checked except for the specified ones
no_cols_selected_except <- function(df, list, exception) {
  rowSums(df[exception], na.rm = TRUE) > 0 &
    rowSums(df[setdiff(list, exception)], na.rm = TRUE) == 0
}

any_cols_selected_except <- function(df, list, exception) {
  rowSums(df[list] == 1, na.rm = TRUE) > 0 &
    rowSums(df[exception] == 1, na.rm = TRUE) == 0
}

# for a set of 1/0, or checkbox, variables, check whether at least 
# the specified numbers of variables were checked, except for the specified ones
min_cols_selected_except <- function(df, list, exception, num_cols_selected) {
  rowSums(df[exception], na.rm = TRUE) == 0 &
    rowSums(df[setdiff(list, exception)], na.rm = TRUE) >= num_cols_selected
}


summarize_df <- function(df) {
  lapply(df, function(col) {summary(col)})
}


replace_char_at <- function(string, position, replacement) {
  paste0(
    substr(string, 1, position - 1),
    replacement,
    substr(string, position + 1, nchar(string))
  )
}

# interpret the data in their original encoding, so they display nicely
# The resulting characters are almost always UTF-8 characters
# While a byte sequence may not be UTF-8, the character itself usually has a UTF-8 representation
# So, e.g., if there's a ‰ in a Windows-encoded file, we want to interpret
# as Windows, so it will display ‰, rather than the Windows+non-UTF8 byte \x89
# But if there's a ‰ in a UTF-8 encoded file, it's already been interpreted correctly
convert_data_to_utf8 <- function(data) {
  file_encoding <- attr(data, "encoding")
  if(file_encoding %in% c("UTF-8","ASCII")) return(data)
  
  dt <- as.data.table(data)
  
  # Fix encoding in all character columns in place
  for (col in names(dt)) {
    if (is.character(dt[[col]])) {
      # Original column before conversion
      original_col <- dt[[col]]
      
      # Interpret characters in a non-UTF-8 encoded file correctly
      # E.g. ‰ in a UTF-8 file, will come in as ‰ and should not be 
      if(is.na(file_encoding)) file_encoding <- "ISO-8559-1"

      tryCatch({
        converted_col <- iconv(original_col, from = file_encoding, to = "UTF-8")  
        # Identify changes by comparing original and converted values
        if (length(which(original_col != converted_col)) > 0) {
          dt[[col]] <- converted_col
        }
      }, error = function(e) {
        print("Conversion failed! Unknown encoding!")
      })      
    }
  }
  
 return(as_tibble(dt))
}