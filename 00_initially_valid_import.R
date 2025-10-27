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


# function to check if the file is hashed
is_hashed <- function() {

  # read Client file
  session$userData$Client <- importFile(upload_filepath, "Client")
  
  # decide if the export is hashed
  return(  
    # TRUE
    session$userData$Export$HashStatus == 4 &
      min(nchar(session$userData$Client$FirstName), na.rm = TRUE) ==
      max(nchar(session$userData$Client$FirstName), na.rm = TRUE)
  )
}

# extract file names from their uploaded zip
if(tolower(tools::file_ext(upload_filepath)) != "zip") {
  show_invalid_popup(
    popupText = glue::glue("You uploaded the wrong file type. The uploaded file is a 
    {tolower(tools::file_ext(upload_filepath))}, not a .zip file. To use Eva, please upload a hashed HMIS 
    CSV Export, which is a .zip file.  If you are not sure how to run a hashed 
    HMIS CSV Export in your HMIS, please contact your HMIS vendor."),
    issueID = 127, 
    title = "Unsuccessful Upload: You did not upload a zip file"
  )
  logMetadata(session, "Unsuccessful upload - zip file not .zip")
} else {
  zipContents <- utils::unzip(zipfile = upload_filepath, list = TRUE)
    
  zipFiles <- zipContents$Name %>% str_replace(".csv", "")
    
  # expected files
  expected_files <- unique(cols_and_data_types$File)
  
  # get missing files by comparing what we expect with what we got
  missing_files <- expected_files[!(expected_files %in% zipFiles)]

  # empty files
  empty_files <- expected_files[
    !file.size(paste0(tempdir(), "/", unique(cols_and_data_types$File), ".csv"))
  ]
    
  ### Now check whether the file is hashed, has the expected structure, and contains
  # the expected csv files
  if(grepl("/", zipContents$Name[1])) {
    show_invalid_popup(
      issueID = 122,
      title = "Unsuccessful Upload: Misstructured directory"
    )
    logMetadata(session, "Unsuccessful upload - zip file was misstructured")
  } else if(length(missing_files)) {
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
  } else if(hasNoExportRow()) {
    show_invalid_popup(
      issueID = 142,
      title = "Unsuccessful Upload: The Export.csv file in your uploaded .zip file has no data.",
      popupText = "Export.csv should have one and only one row. Please upload a hashed HMIS CSV Export that meets all of HUD's specifications. 
      If you are not sure how to resolve this issue, please contact your HMIS vendor."
    )
    logMetadata(session, "Unsuccessful upload - Export.csv has no rows")
  } else if(fnrow(session$userData$Export) > 1) {
    show_invalid_popup(
      issueID = 140,
      title = "Unsuccessful Upload: The Export.csv file in your uploaded .zip file contains more than 1 row.",
      popupText = "Export.csv should only have 1 row. Please upload a hashed HMIS CSV Export that meets all of HUD's specifications. 
      If you are not sure how to resolve this issue, please contact your HMIS vendor."
    )
    logMetadata(session, "Unsuccessful upload - Export.csv has more than 1 row")
  } else if(!isFY2026Export()) {
    show_invalid_popup(
      issueID = 124,
      title = "Unsuccessful Upload: Your HMIS CSV Export is out of date"
    )
    logMetadata(session, "Unsuccessful upload - out of date HMIS CSV Export")
  } else if(!is_hashed()) {
    show_invalid_popup(
      issueID = 126,
      title = "Unsuccessful Upload: You uploaded an unhashed data set"
    )
    logMetadata(session, "Unsuccessful upload - not hashed")
  } else if(length(empty_files) > 0) {
    show_invalid_popup(
      issueID = 150,
      title = glue::glue("Unsuccessful Upload: In your HMIS CSV Export, the following files are empty: {paste(empty_files, collapse=', ')}")
    )
    logMetadata(session, "Unsuccessful upload - not hashed")
  } else {
    session$userData$initially_valid_import(1)
  }
}
