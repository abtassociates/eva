######################
# PURPOSE: This program will check whether the uploaded file is hashed and at
# least looks like a valid HMIS upload (i.e. contains the expected set of csv 
# files)
# if the file is a hashed HMIS, we will proceed with processing
# if it is not, we will show them a pop-up indicating the problem
######################

# extract file names from their uploaded zip
if(tolower(tools::file_ext(input$imported$datapath)) != "zip") {
  title <- "Wrong File Type"
  err_msg <- HTML(str_glue(
    "Eva can only import .zip files. If you are using a 7-zip file, 
    please contact your vendor for help converting to the .zip format."))
  initially_valid_import <- FALSE
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
  browser()
  isFY2024Export <- function() {
    return(grepl("2024", as.character(importFile("Export")$CSVVersion)) &
             !"Export" %in% missing_files)
  }
  
  ### Now check whether the file is hashed, has the expected structure, and contains
  # the expected csv files
  initially_valid_import = FALSE
  
  if(grepl("/", zipContents$Name[1])) {
    title <- "Your zip file is misstructured"
    err_msg <- str_squish("It looks like you may have unzipped your HMIS csv 
          because the individual csv files are contained within a subdirectory.")
    
    logMetadata("Unsuccessful upload - zip file was misstructured")
    
  } else if(!isFY2024Export()) {
    title <- "Your HMIS CSV Export is out of date"
    err_msg <- str_squish("It looks like you either uploaded an FY2022 HMIS CSV 
      Export or your vendor needs to update your Export.csv's CSVVersion. If you
      are not sure how to obtain an FY2024 HMIS CSV Export in your HMIS, please
      contact your HMIS vendor.")
    
    logMetadata("Unsuccessful upload - not FY2024")
  } else if(length(missing_files)) {
    title <- "Missing Files"
    err_msg <- HTML(str_glue(
      "Your zip file appears to be missing the following files:<br/><br/>
    
      {paste(missing_files, collapse = ', ')}<br/><br/>
      
      You either uploaded something other than an HMIS CSV export or your export 
      does not contain all the files outlined in the HMIS CSV Export specifications.
      Be sure that you haven't accidentally uploaded an APR or an LSA. If you are 
      not sure how to run the hashed HMIS CSV Export in your HMIS, please contact 
      your HMIS vendor."))
    
    logMetadata("Unsuccessful upload - wrong/incomplete dataset")
    
  } else if(!is_hashed()) {
    title <- "You uploaded an unhashed data set"
    err_msg <- str_squish("You have uploaded an unhashed version of the HMIS CSV 
      Export. If you are not sure how to run the hashed HMIS CSV Export in your 
      HMIS, please contact your HMIS vendor.")
    
    logMetadata("Unsuccessful upload - not hashed")
    
  } else {
    initially_valid_import = TRUE
  }
}

# if it failed any of the checks, show the pop-up
if(!initially_valid_import) {
  showModal(
    modalDialog(
      title = title,
      err_msg,
      easyClose = TRUE
    )
  )
  reset("imported")
}