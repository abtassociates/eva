
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

importFile <- function(csvFile, guess_max = 1000) {
  filename = str_glue("{csvFile}.csv")
  data <- read_csv(unzip(zipfile = input$imported$datapath, files = filename)
                   ,col_types = get_col_types(csvFile)
                   ,na = ""
  )
  file.remove(filename)
  return(data)
}

get_col_types <- function(file) {
  # get the column data types expected for the given file
  col_types <- cols_and_data_types %>%
    filter(File == file) %>%
    mutate(DataType = data_type_mapping[as.character(DataType)]) %>%
    pull(DataType) %>%
    paste0(collapse = "")
  return(col_types)
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
             format(meta_HUDCSV_Export_Start, "%m-%d-%Y"),
             "to",
             format(meta_HUDCSV_Export_End, "%m-%d-%Y")
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
    CoC = Export$SourceID,
    ExportID = Export$ExportID,
    SourceContactFirst = Export$SourceContactFirst,
    SourceContactLast = Export$SourceContactLast,
    SourceContactEmail = Export$SourceContactEmail,
    SoftwareName = Export$SoftwareName
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
    CoC = Export$SourceID,
    ExportID = Export$ExportID,
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

#############################
# SANDBOX
#############################
importFileSandbox <- function(csvFile) {
  filename = str_glue("{csvFile}.csv")
  data <- read_csv(paste0(directory, "data/", filename)
                   ,col_types = get_col_types(csvFile)
                   ,na = ""
  )
  return(data)
}

############################
# GENERATE CHECK DATA FROM EVACHECKS.XLSX
############################
merge_check_info <- function(data, checkIDs) {
  return(data %>%
    bind_cols(
      evachecks %>% filter(ID %in% c(checkIDs))
    )
  )
}
