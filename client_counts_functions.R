##### REPORTING COLUMNS ######
# these are the main columns that we will report out in the app and exports
clientCountDetailCols <- c("PersonalID",
                           "RelationshipToHoH",
                           "EntryDate",
                           "MoveInDateAdjust",
                           "ExitDate",
                           "Status")
##### MAIN DATAFRAME ######
# this is the primary client count dataset, calculating
# their status and number of days enrolled. 
# This will be used to create the summary and detail datasets used in the
# app, as well as the datasets used in the export
client_count_data_df <- reactive({
  ReportStart <- input$dateRangeCount[1]
  ReportEnd <- input$dateRangeCount[2]

  validation %>%
    mutate(
      PersonalID = as.character(PersonalID),
      RelationshipToHoH = case_when(
        RelationshipToHoH == 1 ~ "Head of Household",
        RelationshipToHoH == 2 ~ "Child",
        RelationshipToHoH == 3 ~ "Spouse or Partner",
        RelationshipToHoH == 4 ~ "Other relative",
        RelationshipToHoH == 5 ~ "Unrelated household member",
        RelationshipToHoH == 99 ~ "Data not collected (please correct)"
      ),
      Status = case_when(
        ProjectType %in% c(ph_project_types) &
          is.na(MoveInDateAdjust) &
          is.na(ExitDate) ~ "Active No Move-In",
        ProjectType %in% c(ph_project_types) &
          !is.na(MoveInDateAdjust) &
          is.na(ExitDate) ~ paste0("Currently Moved In (",
                                   ymd(ReportEnd) - ymd(MoveInDateAdjust),
                                   " days)"),
        ProjectType %in% c(ph_project_types) &
          is.na(MoveInDateAdjust) &
          !is.na(ExitDate) ~ "Exited No Move-In",
        ProjectType %in% c(ph_project_types) &
          !is.na(MoveInDateAdjust) &
          !is.na(ExitDate) ~ "Exited with Move-In",
        !ProjectType %in% c(ph_project_types) &
          is.na(ExitDate) ~ paste0("Currently in project (",
                                   ymd(ReportEnd) - ymd(EntryDate),
                                   " days)"),
        !ProjectType %in% c(ph_project_types) &
          !is.na(ExitDate) ~ "Exited project"
      ),
      sort = ymd(ReportEnd) - ymd(EntryDate)
    ) %>%
    arrange(desc(sort), HouseholdID, PersonalID) %>%
    # make sure to include all columns that will be needed for the various uses
    select(
      PersonalID,
      HouseholdID,
      RelationshipToHoH,
      EntryDate,
      MoveInDateAdjust,
      ExitDate,
      Status,
      ProjectID,
      ProjectName,
      OrganizationName,
      ProjectType
    ) %>%
    filter(served_between(., ReportStart, ReportEnd))
})

##### SUMMARY STUFF ######
# this reactive df is the one used for the summary table in the app. 
# using the function above, it gets and then combines the counts of households
# and people/clients
client_count_summary_df <- reactive({
  # this function summarizes a project-specific client_count, returning a dataset with counts by status
  client_count_summary_by <- function(vname, client_counts) {
    df <- client_counts %>%
      mutate(Status = sub(" \\(.*", "", Status)) %>%
      select(all_of(vname), Status) %>%
      unique() %>%
      group_by(Status)
    return(df)
  }
  
  client_counts <- client_count_data_df() %>%
    filter(ProjectName == input$currentProviderList)
  
  hhs <- client_count_summary_by("HouseholdID", client_counts) %>%
    summarise(Households = n())
  
  clients <- client_count_summary_by("PersonalID", client_counts) %>%
    summarise(Clients = n())
  
  full_join(clients, hhs, by = "Status")
})


##### DOWNLOADING STUFF ######
# make sure these columns are there; they wouldn't be after pivoting if nobody had that status
necessaryCols <- c(
  "Currently in project",
  "Active No Move-In",
  "Currently Moved In"
)

keepCols <- c(
  "OrganizationName", 
  "ProjectID", 
  "ProjectName"
)

# function to pivot statuses to cols for the summary datasets
pivot_and_sum <- function(df, isDateRange = FALSE) {
  if(isDateRange) necessaryCols <- c(
    necessaryCols,
    "Exited project",
    "Exited with Move-In",
    "Exited No Move-In"
  )
  
  pivoted <- df %>%
    # remove the person-specific enrollment days from those statuses (e.g. (660 days))
    # and make sure everyone gets all necessary columns from status (even if they have no projects of that type)
    mutate(
      Status = sub(" \\(.*", "", Status)
    ) %>%
    distinct_at(vars(!!keepCols, Status, ProjectType, PersonalID)) %>%
    select(-PersonalID) %>%
    mutate(n=1) %>%
    complete(nesting(!!!syms(c(keepCols, "ProjectType"))),Status = necessaryCols, fill = list(n = 0)) %>%
    pivot_wider(names_from = Status, values_from = n, values_fn = sum) %>%
    mutate(
      across(!!necessaryCols, ~ 
               replace(., is.na(.) &
                         ProjectType %in% c(psh_project_type,
                                            rrh_project_type), 0)),
      "Currently in Project" = case_when(
        ProjectType %in% c(ph_project_types)  ~ 
          rowSums(select(., `Currently Moved In`, `Active No Move-In`),
                  na.rm = TRUE),
        TRUE ~ replace_na(`Currently in project`, 0)
      )
    ) %>% 
    relocate(`Currently in Project`, .after = ProjectName)
  
  exportTestValues(client_count_download = pivoted)
  
  return(pivoted)
}

get_clientcount_download_info <- function(file) {
  # initial dataset that will make summarizing easier
  validationDF <- client_count_data_df()
  
  ### VALIDATION DATE RANGE TAB ###
  # counts for each status, by project, across the date range provided
  validationDateRange <- 
    pivot_and_sum(
      validationDF, isDateRange = TRUE
    ) %>%
    mutate(
      "Exited Project" = case_when(
        ProjectType %in% c(ph_project_types) ~ 
          rowSums(select(., `Exited with Move-In`, `Exited No Move-In`),
                  na.rm = TRUE),
        TRUE ~ `Exited project`
      )
    ) %>%
    relocate(`Exited Project`, .after=`Currently Moved In`) %>%
    select(-c(`Currently in project`, `Exited project`, ProjectType)) %>%
    arrange(OrganizationName, ProjectName)
  
  ### VALIDATION CURRENT TAB ###
  # counts for each status, by project for just the current date
  validationCurrent <- 
    pivot_and_sum(
      validationDF %>%
        filter(served_between(., input$dateRangeCount[2], input$dateRangeCount[2]))
    ) %>%
    select(-c(`Currently in project`, ProjectType)) %>%
    arrange(OrganizationName, ProjectName)

  ### VALIDATION DETAIL TAB ###
  validationDetail <- validationDF %>% # full dataset for the detail
    select(!!keepCols, !!clientCountDetailCols) %>%
    arrange(OrganizationName, ProjectName, EntryDate)
  
  exportDFList <- list(
    validationCurrent = validationCurrent %>% nice_names(),
    validationDateRange = validationDateRange %>% nice_names(),
    validationDetail = validationDetail %>% nice_names()
  )
  
  names(exportDFList) = c(
    "Validation - Current",
    "Validation - Date Range",
    "Validation - Detail"
  )
  
  write_xlsx(exportDFList,
             path = file)
}
