######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

show_invalid_popup <- function(popupText, issueID, title) {
  reset_app()
  
  showModal(
    modalDialog(
      popupText,
      title = title,
      easyClose = TRUE
    )
  )
}

# function to check if the file is hashed
is_hashed <- function() {

  # read Client file
  Client <- importFile(upload_filepath, "Client")
  file.remove("Client.csv")
  
  # decide if the export is hashed
  return(  
    # TRUE
    Export()$HashStatus == 4 &
      min(nchar(Client$FirstName), na.rm = TRUE) ==
      max(nchar(Client$FirstName), na.rm = TRUE)
  )
}

isFY2024Export <- function() {
  Export(importFile(upload_filepath, "Export"))
  file.remove("Export.csv")
  
  # this is the soonest we can log the session data, with 
  # the export info, since this is the first time we import the Export.csv file
  logSessionData() 
  
  return(
    grepl("2024", as.character(Export()$CSVVersion))
  )
}

# extract file names from their uploaded zip
if(tolower(tools::file_ext(upload_filepath)) != "zip") {
  show_invalid_popup(
    popupText = paste0(
      "The uploaded file is a .", 
      tolower(tools::file_ext(upload_filepath)),
      ", not a .zip."
    ),
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
      popupText = evachecks[ID == 122, "Guidance"],
      issueID = 122,
      title = "Unsuccessful Upload: Misstructured directory"
    )
    logMetadata("Unsuccessful upload - zip file was misstructured")
  } else if("Export" %in% missing_files) {
    show_invalid_popup(
      popupText = evachecks[ID == 123, "Guidance"],
      issueID = 123,
      title = "Unsuccessful Upload: Missing Export.csv; Possible wrong dataset"
    )
    logMetadata("Unsuccessful upload - not an HMIS CSV Export")
  } else if(!isFY2024Export()) {
    show_invalid_popup(
      popupText = evachecks[ID == 124, "Guidance"],
      issueID = 124,
      title = "Unsuccessful Upload: Your HMIS CSV Export is out of date"
    )
    logMetadata("Unsuccessful upload - out of date HMIS CSV Export")
  } else if(length(missing_files)) {
    show_invalid_popup(
      popupText = "The uploaded .zip file does not contain all of the required 
      files to do an analysis of your HMIS data. Thus, Eva cannot read the .zip file.",
      issueID = 125,
      title = "Unsuccessful Upload: Missing files"
    )
    logMetadata("Unsuccessful upload - incomplete dataset")
  } else if(!is_hashed()) {
    show_invalid_popup(
      popupText = evachecks[ID == 126, "Guidance"],
      issueID = 126,
      title = "Unsuccessful Upload: You uploaded an unhashed data set"
    )
    logMetadata("Unsuccessful upload - not hashed")
  } else {
    initially_valid_import(1)
  }
}
