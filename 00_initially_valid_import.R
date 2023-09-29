######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

initially_valid_import <- FALSE

# extract file names from their uploaded zip
if(tolower(tools::file_ext(input$imported$datapath)) != "zip") {
  initially_valid_df <- evachecks %>% filter(ID = 127)
} else {

  zipContents <- unzip(zipfile = input$imported$datapath, list=TRUE)
    
  zipFiles <- zipContents$Name %>% str_replace(".csv", "")
    
  # expected files
  expected_files <- unique(cols_and_data_types$File)
  
  # get missing files by comparing what we expect with what we got
  missing_files <- expected_files[!(expected_files %in% zipFiles)]
  
  # function to check if the file is hashed
  is_hashed <- function() {
    # read Export file
    Export <<- importFile("Export")
    
    # this is the soonest we can log the session data, with 
    # the export info, since this is the first time we import the Export.csv file
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

  isFY2024Export <- function() {
    return(
      grepl("2024", as.character(importFile("Export")$CSVVersion))
    )
  }
  
  ### Now check whether the file is hashed, has the expected structure, and contains
  # the expected csv files
  if(grepl("/", zipContents$Name[1])) {
    initially_valid_df <- evachecks %>% filter(ID = 122)
    logMetadata("Unsuccessful upload - zip file was misstructured")
  } else if("Export" %in% missing_files) {
    initially_valid_df <- evachecks %>% filter(ID == 123)
    logMetadata("Unsuccessful upload - not an HMIS CSV Export")
  } else if(!isFY2024Export()) {
    initially_valid_df <- evachecks %>% filter(ID == 124)
    logMetadata("Unsuccessful upload - out of date HMIS CSV Export")
  } else if(length(missing_files)) {
    initially_valid_df <- evachecks %>% filter(ID == 125) %>% 
      mutate(Guidance = HTML(str_glue(
        "Your zip file appears to be missing the following files:<br/><br/>
      
        {paste(missing_files, collapse = ', ')}<br/><br/>
        
        You either uploaded something other than an HMIS CSV export or your export 
        does not contain all the files outlined in the HMIS CSV Export specifications.
        If you are not sure how to run the hashed HMIS CSV Export in your HMIS,
        please contact your HMIS vendor."))
      )
    logMetadata("Unsuccessful upload - incomplete dataset")
  } else if(!is_hashed()) {
    initially_valid_df <- evachecks %>% filter(ID == 126)
    logMetadata("Unsuccessful upload - not hashed")
  } else {
    initially_valid_import = TRUE
  }
}

# if it failed any of the checks, show the pop-up
if(!initially_valid_import) {
  showModal(
    modalDialog(
      initially_valid_df$Guidance,
      title = initially_valid_df$Issue,
      easyClose = TRUE
    )
  )
  reset("imported")
}
