##### REPORTING COLUMNS ######
# these are the main columns that we will report out in the app and exports
clientCountDetailCols <- c("Personal ID",
                           "Relationship to Head of Household",
                           "Entry Date" = "EntryDate",
                           "Move In Date (RRH/PSH Only)",
                           "Exit Date" = "ExitDate",
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
        ProjectType %in% c(3, 13) &
          is.na(MoveInDateAdjust) &
          is.na(ExitDate) ~ "Active No Move-In",
        ProjectType %in% c(3, 13) &
          !is.na(MoveInDateAdjust) &
          is.na(ExitDate) ~ paste0("Currently Moved In (",
                                   today() - MoveInDateAdjust,
                                   " days)"),
        ProjectType %in% c(3, 13) &
          is.na(MoveInDateAdjust) &
          !is.na(ExitDate) ~ "Exited No Move-In",
        ProjectType %in% c(3, 13) &
          !is.na(MoveInDateAdjust) &
          !is.na(ExitDate) ~ "Exited with Move-In",
        !ProjectType %in% c(3, 13) &
          is.na(ExitDate) ~ paste0("Currently in project (",
                                   today() - EntryDate, 
                                   " days)"),
        !ProjectType %in% c(3, 13) &
          !is.na(ExitDate) ~ "Exited project"
      ),
      sort = today() - EntryDate,
      MoveInDateAdjust = format.Date(MoveInDateAdjust, "%m-%d-%Y")
    ) %>%
    arrange(desc(sort), HouseholdID, PersonalID) %>%
    # make sure to include all columns that will be needed for the various uses
    select(
      "Personal ID" = PersonalID,
      "Household ID" = HouseholdID,
      "Relationship to Head of Household" = RelationshipToHoH,
      EntryDate,
      "Move In Date (RRH/PSH Only)" = MoveInDateAdjust,
      ExitDate,
      Status,
      "Project ID" = ProjectID,
      "Project Name" = ProjectName,
      "Organization" = OrganizationName,
      ProjectType
    ) %>%
    filter(served_between(., ReportStart, ReportEnd))
})

##### SUMMARY STUFF ######
# this reactive df is the one used for the summary table in the app. 
# using the function above, it gets and then combines the counts of households and people/clients
client_count_summary_df <- reactive({
  # this function summarizes a project-specific client_count, returning a dataset with counts by status
  client_count_summary_by <- function(vname, client_counts) {
    df <- client_counts %>%
      mutate(Status = sub(" \\(.*", "", Status)) %>%
      select(vname, Status) %>%
      unique() %>%
      group_by(Status)
    return(df)
  }
  client_counts <- client_count_data_df() %>%
    filter(`Project Name` == input$currentProviderList)
  
  hhs <- client_count_summary_by("Household ID", client_counts) %>%
    summarise(Households = n())
  
  clients <- client_count_summary_by("Personal ID", client_counts) %>%
    summarise(Clients = n())
  
  full_join(clients, hhs, by = "Status")
})

##### DOWNLOADING STUFF ######
get_clientcount_download_info <- function(file) {
  keepCols <- c("Organization", "Project ID", "Project Name")
  
  # initial dataset thta will make summarizing easier
  validationDF <- client_count_data_df() %>%
    mutate(n = 1)
  
  # this function pivots by project and gets the counts of people with each status
  pivot_and_sum <- function(df, isDateRange = FALSE) {
    
    # make sure these columns are there; they wouldn't be after pivoting if nobody had that status
    necessaryCols <- c(
      "Currently in project",
      "Active No Move-In",
      "Currently Moved In"
    )
    
    if(isDateRange) necessaryCols <- c(
      necessaryCols,
      "Exited project",
      "Exited with Move-In",
      "Exited No Move-In"
    )
    
    pivoted <- df %>%
      # remove the person-specific enrollment days from those statuses (e.g. (660 days))
      mutate(
        Status = sub(" \\(.*", "", Status)
      ) %>%
      select(keepCols, n, Status, ProjectType, `Personal ID`) %>%
      unique() %>%
      select(keepCols, n, Status, ProjectType) %>%
      pivot_wider(names_from = Status, values_from = n, values_fn = sum) %>%
      add_column(!!!necessaryCols[setdiff(names(necessaryCols), names(df))]) %>%
      select(!!keepCols, !!necessaryCols, ProjectType) %>%
      mutate(
        across(!!necessaryCols, ~ replace(., is.na(.) &
                                            ProjectType %in% c(3, 13), 0)),
        "Currently in Project" = case_when(
          ProjectType %in% c(3, 13)  ~ 
            rowSums(select(., `Currently Moved In`, `Active No Move-In`),
                    na.rm = TRUE),
          TRUE ~ `Currently in project`
        )
      ) %>% 
      relocate(`Currently in Project`, .after = `Project Name`)
    return(pivoted)
  }
  
  ### VALIDATION DATE RANGE TAB ###
  # counts for each status, by project, across the date range provided
  validationDateRange <- pivot_and_sum(validationDF, isDateRange = TRUE) %>%
    mutate(
      "Exited Project" = case_when(
        ProjectType %in% c(3,13) ~ 
          rowSums(select(., `Exited with Move-In`, `Exited No Move-In`),
                  na.rm = TRUE),
        TRUE ~ `Exited project`
      )
    ) %>%
    relocate(`Exited Project`, .after=`Currently Moved In`) %>%
    select(-c(`Currently in project`, `Exited project`, ProjectType))
  
  ### VALIDATION CURRENT TAB ###
  # counts for each status, by project for just the current date
  validationCurrent <- pivot_and_sum(validationDF %>% 
                                       filter(served_between(., input$dateRangeCount[2], input$dateRangeCount[2]))
  ) %>%
    select(-c(`Currently in project`, ProjectType))
  
  ### VALIDATION DETAIL TAB ###
  validationDetail <- validationDF %>% # full dataset for the detail
    select(!!keepCols, !!clientCountDetailCols)
  
  exportDFList <- list(
    validationCurrent = validationCurrent,
    validationDateRange = validationDateRange,
    validationDetail = validationDetail
  )
  
  names(exportDFList) = c(
    "Validation - Current",
    "Validation - Date Range",
    "Validation - Detail"
  )
  
  write_xlsx(exportDFList, path = file)
}