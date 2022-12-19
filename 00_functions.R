
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

importFile <- function(csvFile, col_types = NULL, guess_max = 1000) {
  if (is.null(input$imported)) {return()}
  filename = glue::glue("{csvFile}.csv")
  data <- read_csv(unzip(zipfile = input$imported$datapath, files = filename)
                   ,col_types = col_types
  )
  file.remove(filename)
  return(data)
}


getDQReportDataList <- function(dqData, dqOverlaps = NULL, bySummaryLevel = NULL) {
  select_list = c("Organization Name" = "OrganizationName",
                  "Project ID" = "ProjectID",
                  "Project Name" = "ProjectName",
                  "Issue" = "Issue",
                  "Personal ID" = "PersonalID",
                  "Household ID" = "HouseholdID",
                  "Entry Date"= "EntryDate")
  
  high_priority <- dqData %>% 
    filter(Type == "High Priority") %>% 
    select(all_of(select_list))
  
  errors <- dqData %>%
    filter(Type == "Error") %>% 
    select(all_of(select_list))
  
  warnings <- dqData %>%
    filter(Type == "Warning") %>% 
    select(all_of(select_list))
  
  dqOverlapDetails <- dqOverlaps %>%
    select(-c(Issue, Type, Guidance, PreviousIssue)) %>%
    relocate(OrganizationName,
            ProjectID,
            ProjectName,
            ProjectType,
            EnrollmentID,
            HouseholdID,
            PersonalID,
            EntryDate,
            FirstDateProvided,
            "MoveInDate" = MoveInDateAdjust,
            ExitDate,
            PreviousOrganizationName,
            PreviousProjectID,
            PreviousProjectName,
            PreviousProjectType,
            PreviousEnrollmentID,
            PreviousHouseholdID,
            PreviousPersonalID,
            PreviousEntryDate,
            PreviousFirstDateProvided,
            "PreviousMoveInDate" = PreviousMoveInDateAdjust,
            PreviousExitDate
    )
  
  mainsummary <- rbind(
      dqData %>% select(Type, Issue, PersonalID),
      dqOverlaps %>% select(Type, Issue, PersonalID)
    ) %>%
    # group_by(ProjectName, Type, Issue) %>%
    group_by(Type, Issue) %>%
    summarise(Clients = n()) %>%
    select(Type, Clients, Issue) %>%
    arrange(Type, desc(Clients))
  
  bySummaryLevel2 <- rlang::sym(bySummaryLevel)
  byunitsummary <- rbind(
      dqData %>% select(!!bySummaryLevel2, Type, Issue, PersonalID),
      dqOverlaps %>% select(!!bySummaryLevel2, Type, Issue, PersonalID)
    ) %>%
    group_by(!!bySummaryLevel2, Type, Issue) %>%
    summarise(Clients = n()) %>%
    select(!!bySummaryLevel2, Type, Clients, Issue) %>%
    arrange(Type, desc(Clients), !!bySummaryLevel2)
    
    
  guidance <- dqData %>%
    select(Type, Issue, Guidance) %>%
    unique() %>%
    mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>%
    arrange(Type)
  
  exportDetail <- data.frame(c("Export Start", "Export End", "Export Date"),
                             c(meta_HUDCSV_Export_Start,
                               meta_HUDCSV_Export_End,
                               meta_HUDCSV_Export_Date))
  colnames(exportDetail) = c("Export Field", "Value")
  
  exportDFList <- list(
    exportDetail = exportDetail,
    mainsummary = mainsummary,
    byunisummary = byunitsummary,
    guidance = guidance,
    high_priority = high_priority,
    errors = errors,
    warnings = warnings,
    overlaps = dqOverlapDetails
  )
  
  names(exportDFList) <- c(
    "Export Detail",
    paste(if_else(bySummaryLevel == "OrganizationName", "System","Organization"),"Summary"),
    paste(if_else(bySummaryLevel == "OrganizationName", "Organization","Project"),"Summary"),
    "Guidance",
    "High Priority",
    "Errors", 
    "Warnings",
    "Overlap Details"
  )
  
  exportDFList <- exportDFList[sapply(exportDFList, 
                                      function(x) dim(x)[1]) > 0]
  
  return(exportDFList)
}

zip_initially_valid <- function () {
  zipContents <- unzip(zipfile = input$imported$datapath, list=TRUE)
  requiredFiles <- c("Client.csv",
    "Enrollment.csv",
    "Exit.csv",
    "Services.csv",
    "CurrentLivingSituation.csv",
    "Project.csv",
    "Inventory.csv",
    "EnrollmentCoC.csv",
    "Organization.csv",
    "Export.csv"
  )
  missing_files = requiredFiles[!(requiredFiles %in% zipContents$Name)]

  valid_file(0)
  if(grepl("/", zipContents$Name[1])) {
    title = "Your zip file is mis-structured"
    err_msg = "It looks like you may have unzipped your HMIS csv because the
    individual csv files are contained within a subdirectory."
    write(paste(session$token, "|",Sys.time(),": Unsuccessful - file was mistructured"),
          "www/metadata/upload_metadata.txt", append = TRUE)
    
  } 
  else if(length(missing_files)) {
    title = "Wrong Dataset"
    err_msg = "You uploaded something other than a HUD CSV export. Be sure
    that you haven't accidentally uploaded an APR or an LSA. If you
          are not sure how to run the hashed HMIS CSV Export in your HMIS, please
          contact your HMIS vendor.
    "
    write(paste(session$token, "|",Sys.time(),": Unsuccessful - wrong dataset"),
          "www/metadata/upload_metadata.txt", append = TRUE)
  } 
  else if(!is_hashed()) {
    title = "You uploaded an unhashed data set"
    err_msg = "You have uploaded an unhashed version of the HMIS CSV Export. If you
          are not sure how to run the hashed HMIS CSV Export in your HMIS, please
          contact your HMIS vendor."
    write(paste(session$token, "|",Sys.time(),": Unsuccessful - not hashed"),
          "www/metadata/upload_metadata.txt", append = TRUE)
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
  Export <<- importFile("Export", col_types = "cncccccccTDDcncnnn")
  # read Client file
  Client <- importFile("Client",
                       col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")
  
  # decide if the export is hashed
  return(  
    # TRUE
    Export$HashStatus == 4 &
    min(nchar(Client$FirstName), na.rm = TRUE) ==
    max(nchar(Client$FirstName), na.rm = TRUE)
  )
}

# Non-Residential Long Stayers --------------------------------------------

calculate_long_stayers <- function(input, projecttype){
  
  served_in_date_range %>%
    select(all_of(vars_prep), ProjectID) %>%
    mutate(
      Days = as.numeric(difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate)),
      Issue = "Days Enrollment Active Exceeds CoC-specific Settings",
      Type = "Warning",
      Guidance = str_squish("You have at least one active enrollment that has been
         active for longer than the days set for this Project Type in your
         CoC-specific Settings on the Home tab.")
    ) %>%
    filter(is.na(ExitDate) &
             ProjectType == projecttype &
             input < Days) %>% 
    select(all_of(vars_we_want))
  
}
