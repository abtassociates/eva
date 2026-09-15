######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

show_invalid_popup <- function(popupText = NULL, issueID, title) {
  reset_app(session)
  
  showModal(
    modalDialog(
      HTML(
        ifelse(
          is.null(popupText), 
          evachecks %>% fsubset(ID == issueID) %>% pull(Guidance), 
          popupText
        )
      ),
      title = title,
      easyClose = TRUE
    )
  )
}


validate_file_type <- function(upload_filepath) {
  ext <- tolower(tools::file_ext(upload_filepath))
  if (ext == "zip") return(TRUE)
  
  show_invalid_popup(
    popupText = glue::glue("You uploaded the wrong file type. The uploaded file is a 
    {ext}, not a .zip file. To use Eva, please upload a hashed HMIS 
    CSV Export, which is a .zip file. If you are not sure how to run a hashed 
    HMIS CSV Export in your HMIS, please contact your HMIS vendor."),
    issueID = 127, 
    title = "Unsuccessful Upload: You did not upload a zip file"
  )
  logMetadata(session, "Unsuccessful upload - zip file not .zip")
  return(FALSE)
}

validate_zip_structure <- function(zip_files) {
  has_folders <- any(grepl("/", zip_files))
  if(!has_folders) return(TRUE)
  
  show_invalid_popup(
    issueID = 122,
    title = "Unsuccessful Upload: Misstructured directory"
  )
  logMetadata(session, "Unsuccessful upload - zip file was misstructured")
  
  return(FALSE)
}

validate_missing_files <- function(zip_files, expected_files) {
  missing_files <- setdiff(expected_files, zip_files)
  
  if(length(missing_files) == 0) return(TRUE)
  
  missing_files_list <- paste(
    glue::glue('<li>{missing_files}.csv</li>'),
    collapse = "\n"
  )
  show_invalid_popup(
    popupText = glue::glue("Your uploaded .zip file does not contain all of the required 
    files to do an an analysis of your HMIS data. Your .zip file appears to be
    missing the following files: 
    
    <ul>{missing_files_list}</ul>
    
    You either uploaded something other than an HMIS CSV Export, or your export 
    does not contain all the files outlined in the HMIS CSV Export 
    specifications. To use Eva, please upload a hashed HMIS CSV Export that 
    meets all of HUD's specifications. If you are not sure how to run a hashed
    HMIS CSV Export in your HMIS, please contact your HMIS vendor."),
    issueID = 125,
    title = "Unsuccessful Upload: Missing files"
  )
  logMetadata(session, "Unsuccessful upload - incomplete dataset")
  return(FALSE)
}

hasNoExportRow <- function() {
  returnVal <- FALSE # assume there's 1+ rows

  # This is the first time we're importing Export.csv. 
  # Saving it for easier reference later
  session$userData$Export <- importFile(upload_filepath, "Export")
  
  if(fnrow(session$userData$Export) == 0) {
    # in order to log the session (which we do here because it's the soonest we 
    # have access to the Export data needed for logging the session)
    # we need to add a row to it
    session$userData$Export <- rowbind(session$userData$Export, tibble_row())
    returnVal <- TRUE
  }
  logSessionData(session)
  return(returnVal)
}

isFY2026Export <- function() {
  return(
    grepl("2026", as.character(session$userData$Export$CSVVersion))
  )
}

validate_export_rows <- function() {
  if (hasNoExportRow()) {
    show_invalid_popup(
      issueID = 142,
      title = "Unsuccessful Upload: The Export.csv file in your uploaded .zip file has no data.",
      popupText = "Export.csv should have one and only one row. Please upload a hashed HMIS CSV Export that meets all of HUD's specifications. 
      If you are not sure how to resolve this issue, please contact your HMIS vendor."
    )
    logMetadata(session, "Unsuccessful upload - Export.csv has no rows")
    return(FALSE)
  }
  
  if (fnrow(session$userData$Export) > 1) {
    show_invalid_popup(
      issueID = 140,
      title = "Unsuccessful Upload: The Export.csv file in your uploaded .zip file contains more than 1 row.",
      popupText = "Export.csv should only have 1 row. Please upload a hashed HMIS CSV Export that meets all of HUD's specifications. 
      If you are not sure how to resolve this issue, please contact your HMIS vendor."
    )
    logMetadata(session, "Unsuccessful upload - Export.csv has more than 1 row")
    return(FALSE)
  }
  return(TRUE)
}


# function to check if the file is hashed
is_hashed <- function() {

  # read Client file
  session$userData$Client <- importFile(upload_filepath, "Client")
  
  if(is.null(session$userData$Client) || fnrow(session$userData$Client) == 0)
    return(TRUE)
  
  return(  
    session$userData$Export$HashStatus == 4 &&
    all(c("FirstName", "MiddleName", "LastName", "SSN") %in% names(session$userData$Client)) &&
    all(
      session$userData$Client$FirstName |> na_rm() |> vlengths() == 64L,
      session$userData$Client$MiddleName |> na_rm() |> vlengths() == 64L,
      session$userData$Client$LastName |> na_rm() |> vlengths() == 64L,
      session$userData$Client$SSN  |> na_rm() |> vlengths() == 68L
    )
  )
}


validate_version <- function() {
  if (isFY2026Export()) return(TRUE)
  
  show_invalid_popup(
    issueID = 124,
    title = "Unsuccessful Upload: Your HMIS CSV Export is out of date"
  )
  logMetadata(session, "Unsuccessful upload - out of date HMIS CSV Export")
  return(FALSE)
}


validate_hashed <- function() {
  if (is_hashed()) return(TRUE)
  
  show_invalid_popup(
    issueID = 126,
    title = "Unsuccessful Upload: Your data set is either unhashed or hashed in the wrong format",
    popupText = "Eva expects a hashed HMIS CSV Export that conforms to the SHA-256 format as specified in the HMIS CSV Format Specifications. 
    If you are not sure how to run a SHA-256 hashed HMIS CSV Export in your HMIS, please contact your HMIS vendor."
  )
  logMetadata(session, "Unsuccessful upload - not hashed")
  return(FALSE)
}

validate_non_empty_files <- function(upload_filepath, expected_files) {
  empty_files <- zip::zip_list(upload_filepath) %>% 
    fsubset(gsub(".csv", "", basename(filename)) %in% expected_files & uncompressed_size <= 1)
  
  if (nrow(empty_files) == 0) return(TRUE)
  
  show_invalid_popup(
    issueID = 150,
    title = "Unsuccessful Upload - Empty file(s)",
    popupText = glue::glue("In your HMIS CSV Export, the following files are empty: 
                           {paste(empty_files$filename, collapse=', ')}. All files should at least contain headers.")
  )
  logMetadata(session, "Unsuccessful upload - empty files")
  return(FALSE)
}

expected_files <- unique(cols_and_data_types$CSV)
zip_files <- utils::unzip(zipfile = upload_filepath, list = TRUE)$Name %>% str_replace(".csv", "")

# Run validations in order; short-circuiting (&&) stops at the first failure
is_valid <- validate_file_type(upload_filepath) &&
  validate_zip_structure(zip_files, expected_files) &&
  validate_missing_files(zip_files, expected_files) &&
  validate_export_rows() &&
  validate_version() &&
  validate_hashed() &&
  validate_non_empty_files(upload_filepath, expected_files)

if (is_valid)
  session$userData$initially_valid_import(1)