######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

show_invalid_popup <- function(popupText = NULL, issueID, title) {
  reset_app()
  
  showModal(
    modalDialog(
      HTML(
        ifelse(
          is.null(popupText), 
          evachecks %>% filter(ID == issueID) %>% pull(Guidance), 
          popupText
        )
      ),
      title = title,
      easyClose = TRUE
    )
  )
}

hasGT1ExportRow <- function() {
  Export(importFile(upload_filepath, "Export"))
  file.remove("Export.csv")
  
  # this is the soonest we can log the session data, with 
  # the export info, since this is the first time we import the Export.csv file
  logSessionData() 
  
  return(nrow(Export()) > 1)
}

isFY2024Export <- function() {
  return(
    grepl("2024", as.character(Export()$CSVVersion))
  )
}


# function to check if the file is hashed
is_hashed <- function() {

  # read Client file
  Client <- importFile(upload_filepath, "Client")
  
  # decide if the export is hashed
  return(  
    # TRUE
    Export()$HashStatus == 4 &
      min(nchar(Client$FirstName), na.rm = TRUE) ==
      max(nchar(Client$FirstName), na.rm = TRUE)
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
  logMetadata("Unsuccessful upload - zip file not .zip")
} else {
  zipContents <- utils::unzip(zipfile = upload_filepath, list = TRUE)
    
  zipFiles <- zipContents$Name %>% str_replace(".csv", "")
    
  # expected files
  expected_files <- unique(cols_and_data_types$File)
  
  # get missing files by comparing what we expect with what we got
  missing_files <- expected_files[!(expected_files %in% zipFiles)]

  ### Now check whether the file is hashed, has the expected structure, and contains
  # the expected csv files
  if(grepl("/", zipContents$Name[1])) {
    show_invalid_popup(
      issueID = 122,
      title = "Unsuccessful Upload: Misstructured directory"
    )
    logMetadata("Unsuccessful upload - zip file was misstructured")
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
    logMetadata("Unsuccessful upload - incomplete dataset")
  } else if(hasGT1ExportRow()) {
    show_invalid_popup(
      issueID = 140,
      title = "Unsuccessful Upload: The Export.csv file in your uploaded .zip file contains more than 1 row.",
      popupText = "Export.csv should only have 1 row. Please upload a hashed HMIS CSV Export that meets all of HUD's specifications. 
      If you are not sure how to resolve this issue, please contact your HMIS vendor."
    )
    logMetadata("Unsuccessful upload - Export.csv has more than 1 row")
  } else if(!isFY2024Export()) {
    show_invalid_popup(
      issueID = 124,
      title = "Unsuccessful Upload: Your HMIS CSV Export is out of date"
    )
    logMetadata("Unsuccessful upload - out of date HMIS CSV Export")
  } else if(!is_hashed()) {
    show_invalid_popup(
      issueID = 126,
      title = "Unsuccessful Upload: You uploaded an unhashed data set"
    )
    logMetadata("Unsuccessful upload - not hashed")
  } else {
    initially_valid_import(1)
  }
}
