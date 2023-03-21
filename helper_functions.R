
# Age ---------------------------------------------------------------------

age_years <- function(earlier, later)
{
  floor(decimal_date(later) - decimal_date(earlier))
  
}

# Display Helpers ---------------------------------------------------------

living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 1 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 2 ~ "Transitional housing",
    ReferenceNo == 3 ~ "Permanent housing (other than RRH) for formerly homeless persons",
    ReferenceNo == 4 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 5 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 6 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 7 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 32 ~ "Host Home (non-crisis)",
    ReferenceNo == 13 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 36 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 18 ~ "Safe Haven",
    ReferenceNo == 15 ~ "Foster care home of foster care group home",
    ReferenceNo == 12 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 25 ~ "Long-term care facility or nursing home",
    ReferenceNo == 22 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 35 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 16 ~ "Place not meant for habitation",
    ReferenceNo == 23 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 29 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 14 ~ "H/Motel paid for by household",
    ReferenceNo == 26 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 27 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 28 ~ "Rental by client, with GPD TIP housing subsidy",
    ReferenceNo == 19 ~ "Rental by client, with VASH housing subsidy",
    ReferenceNo == 31 ~ "Rental by client, with RRH or equivalent subsidy",
    ReferenceNo == 33 ~ "Rental by client, with HCV voucher",
    ReferenceNo == 34 ~ "Rental by client in a public housing unit",
    ReferenceNo == 10 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 20 ~ "Rental by client, with other ongoing housing subsidy",
    ReferenceNo == 21 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 11 ~ "Owned by client, no ongoing housing subsidy",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

project_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 0 ~ "Emergency Shelter (NbN)",
    ReferenceNo == 1 ~ "Emergency Shelter (E/E)",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 9 ~ "PH - Housing Only",
    ReferenceNo == 10 ~ "PH - Housing with Services",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
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
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 99 ~ "Data not collected",
    TRUE ~ "something's wrong"
  )
}

translate_HUD_yes_no <- function(column_name){
  case_when(
    column_name == 1 ~ "Yes", 
    column_name == 0 ~ "No",
    column_name %in% c(8, 9, 99) ~ "Unknown",
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
  if (is.null(input$imported)) {return()}
  filename = str_glue("{csvFile}.csv")
  data <- read_csv(unzip(zipfile = input$imported$datapath, files = filename)
                   ,col_types = get_col_types(csvFile)
                   ,na = ""
  )
  file.remove(filename)
  return(data)
}

zip_initially_valid <- function () {
  # extract file names from their uploaded zip
  zipContents <- unzip(zipfile = input$imported$datapath, list=TRUE)
  
  zipFiles <- zipContents$Name %>% 
    str_replace(".csv", "")
  
  # expected files
  expected_files = unique(cols_and_data_types$File)
  
  # get missing files by comparing what we expect with what we got
  missing_files <- expected_files[!(expected_files %in% zipFiles)]

  valid_file(0)
  if(grepl("/", zipContents$Name[1])) {
    title = "Your zip file is misstructured"
    err_msg = "It looks like you may have unzipped your HMIS csv because the
    individual csv files are contained within a subdirectory."
    logMetadata("Unsuccessful upload - zip file was misstructured")
  } 
  else if(length(missing_files)) {
    title = "Missing Files"
    err_msg = HTML(str_glue(
    "Your zip file appears to be missing the following files:<br/><br/>
    
    {paste(missing_files,collapse=', ')}<br/><br/>
    
    You either uploaded something 
    other than an HMIS CSV export or your export does not contain all the files outlined in 
    the HMIS CSV Export specifications. Be sure that you haven't accidentally uploaded an APR 
    or an LSA. If you are not sure how to run the hashed HMIS CSV Export in your HMIS,
    please contact your HMIS vendor."))
    logMetadata("Unsuccessful upload - wrong/incomplete dataset")
  } 
  else if(!is_hashed()) {
    title = "You uploaded an unhashed data set"
    err_msg = "You have uploaded an unhashed version of the HMIS CSV Export. If you
          are not sure how to run the hashed HMIS CSV Export in your HMIS, please
          contact your HMIS vendor."
    logMetadata("Unsuccessful upload - not hashed")
  } else {
    return(TRUE)
  }

  if(!valid_file()) {
    showModal(
      modalDialog(
        title = title,
        err_msg,
        easyClose = TRUE
      )
    )
    reset("imported")
    return(FALSE)
  }
}

is_hashed <- function() {
  # read Export file
  Export <<- importFile("Export")

  logSessionData()
  
  # read Client file
  Client <- importFile("Client")
  
  # decide if the export is hashed
  return(  
    # TRUE
    Export$HashStatus == 4 &
    min(nchar(Client$FirstName), na.rm = TRUE) ==
    max(nchar(Client$FirstName), na.rm = TRUE)
  )
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
  
  filename <- "www/metadata/metadata.csv"
  write_csv(
    x = d,
    filename,
    append = TRUE,
    col_names = !file.exists(filename)
  )
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
  capture.output(d, file=stderr())
  
    
  filename <- "www/metadata/sessiondata.csv"
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
  capture.output(d, file=stderr())
}
  
date_stamped_filename <- function(filename) {
  paste(filename, Sys.Date(), ".xlsx", sep = "")
}
