######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

show_invalid_popup <- function(issueID) {
  initially_valid_df <- evachecks %>% filter(ID == issueID)

  initially_valid_import(0)

  showModal(
    modalDialog(
      initially_valid_df$Guidance,
      title = initially_valid_df$Issue,
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
  show_invalid_popup(127)
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
    show_invalid_popup(122)
    logMetadata("Unsuccessful upload - zip file was misstructured")
    reset_reactivevals()
  } else if("Export" %in% missing_files) {
    show_invalid_popup(123)
    logMetadata("Unsuccessful upload - not an HMIS CSV Export")
    reset_reactivevals()
  } else if(!isFY2024Export()) {
    show_invalid_popup(124)
    logMetadata("Unsuccessful upload - out of date HMIS CSV Export")
    reset_reactivevals()
  } else if(length(missing_files)) {
    evachecks <- evachecks %>% filter(ID == 125) %>% 
      mutate(Guidance = HTML(str_glue(
        "Your zip file appears to be missing the following files:<br/><br/>
      
        {paste(missing_files, collapse = ', ')}<br/><br/>
        
        You either uploaded something other than an HMIS CSV export or your export 
        does not contain all the files outlined in the HMIS CSV Export specifications.
        If you are not sure how to run the hashed HMIS CSV Export in your HMIS,
        please contact your HMIS vendor."))
      )
    show_invalid_popup(125)
    logMetadata("Unsuccessful upload - incomplete dataset")
    reset_reactivevals()
  } else if(!is_hashed()) {
    show_invalid_popup(126)
    logMetadata("Unsuccessful upload - not hashed")
    reset_reactivevals()
  } else {
    initially_valid_import(1)
  }
}
