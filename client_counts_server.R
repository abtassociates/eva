##### REPORTING COLUMNS ######
# these are the main columns that we will report out in the app and exports
clientCountDetailCols <- c("PersonalID",
                           "EnrollmentID",
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

  session$userData$validation %>%
    fmutate(
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
    roworder(-sort, HouseholdID, PersonalID) %>% 
    # make sure to include all columns that will be needed for the various uses
    fselect(
      PersonalID,
      EnrollmentID,
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
    fsubset(EntryDate <= ReportEnd &
             (is.na(ExitDate) | ExitDate >= ReportStart))
})

##### SUMMARY STUFF ######
# this reactive df is the one used for the summary table in the app. 
# using the function above, it gets and then combines the counts of households
# and people/clients
client_count_summary_df <- reactive({
  # this function summarizes a project-specific client_count, returning a dataset with counts by status
  client_count_summary_by <- function(vname, client_counts) {
    df <- client_counts %>%
      fmutate(Status = sub(" \\(.*", "", Status)) %>%
      fselect(vname,'Status') %>% #select(all_of(vname), Status) %>%
      funique() %>%
      group_by(Status)
    return(df)
  }
  
  client_counts <- client_count_data_df() %>%
    fsubset(ProjectName == input$currentProviderList)
  
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
    mutate(n = 1) %>%
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
  
  return(pivoted)
}

get_clientcount_download_info <- function(file) {
  # initial dataset that will make summarizing easier
  validationDF <- client_count_data_df()
  
  ### session$userData$validation DATE RANGE TAB ###
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
    relocate(`Exited Project`, .after = `Currently Moved In`) %>%
    select(-c(`Currently in project`, `Exited project`, ProjectType)) %>%
    arrange(OrganizationName, ProjectName)
  
  ### CURRENT TAB ###
  # counts for each status, by project for just the current date
  validationCurrent <- 
    pivot_and_sum(
      validationDF %>%
        filter(EntryDate <= input$dateRangeCount[2] &
                 (is.na(ExitDate) | ExitDate >= input$dateRangeCount[2]))
    ) %>%
    select(-c(`Currently in project`, ProjectType)) %>%
    arrange(OrganizationName, ProjectName)

  ### DETAIL TAB ###
  validationDetail <- validationDF %>% # full dataset for the detail
    select(!!keepCols, !!clientCountDetailCols) %>%
    arrange(OrganizationName, ProjectName, EntryDate)
  
  exportDFList <- list(
    validationCurrent = validationCurrent %>% nice_names(),
    validationDateRange = validationDateRange %>% nice_names(),
    validationDetail = validationDetail %>% nice_names()
  )
  
  names(exportDFList) = c(
    "validation - Current",
    "validation - Date Range",
    "validation - Detail"
  )
  
  exportTestValues(
    client_count_download_current = summarize_df(validationCurrent %>% nice_names())
  )
  exportTestValues(
    client_count_download_date_range = summarize_df(validationDateRange %>% nice_names())
  )
  exportTestValues(
    client_count_download_detail = validationDetail %>% nice_names()
  )
  
  write_xlsx(exportDFList,
             path = file)
  
  logMetadata(session, paste0("Downloaded Client Counts Report",
                     if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  
}


output$validate_plot <- renderPlot({
  req(session$userData$valid_file() == 1)
  # browser()
  
  detail <- client_count_data_df() %>%
    fsubset(str_detect(Status, "Exit", negate = TRUE)) %>%
    fmutate(Status = factor(
      fcase(
        str_detect(Status, "Currently in"), "Currently in project",
        str_detect(Status, "Currently Moved"), "Currently Moved In",
        default = Status
      ),
      levels = c("Currently in project",
                 "Active No Move-In",
                 "Currently Moved In")
    )) %>% 
    count(ProjectType, Status, name = "Total")
  
  detail_order <- detail %>%
    group_by(ProjectType) %>%
    summarise(InProject = sum(Total, na.rm = FALSE)) %>%
    ungroup()
  
  
  plot_data <- detail %>%
    join(detail_order, on = "ProjectType", how = 'left') %>%
    group_by(ProjectType) %>%
    arrange(ProjectType, desc(Total)) %>%
    fmutate(
      movedin = flag(Total, default = 0),
      text_position = fcase(
        !ProjectType %in% c(ph_project_types), InProject / 2,
        ProjectType %in% c(ph_project_types), 
          Total / 2 + movedin
      )
    )
  
  validate_by_org <-
    ggplot(
      plot_data,
      aes(x = reorder(project_type_abb(ProjectType), InProject),
          y = Total, fill = Status)
    ) +
    geom_col(alpha = .7, position = "stack")  +
    geom_text(aes(label = prettyNum(Total, big.mark = ","),
                  y = text_position),
              color = "gray14")+
    scale_y_continuous(label = comma_format()) +
    scale_colour_manual(
      values = c(
        "Currently in project" = "#71B4CB",
        "Active No Move-In" = "#7F5D9D",
        "Currently Moved In" = "#52BFA5"
      ),
      aesthetics = "fill"
    ) +
    labs(
      title = "Current System-wide Counts",
      x = "",
      y = ""
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title.position = "plot",
      title = element_text(colour = "#73655E"),
      legend.position = "top"
    )
  
  validate_by_org
})

# CLIENT COUNT DETAILS - APP ----------------------------------------------
output$clientCountData <- renderDT({
  req(session$userData$valid_file() == 1)
  req(nrow(session$userData$validation) > 0)
  
  # getting an error sometimes? Warning: Error in filter: â„¹ In argument: `ProjectName == input$currentProviderList`.
  # Caused by error:
  #   ! `..1` must be of size 292 or 1, not size 0.
  
  x <- client_count_data_df() %>%
    fsubset(ProjectName == input$currentProviderList) %>%
    fselect(clientCountDetailCols) %>%
    fmutate(
      days = as.integer(sub(".*\\((\\d+) days\\).*", "\\1", Status)),
      RelationshipToHoH = as.factor(RelationshipToHoH),
      Status = factor(Status, levels = funique(Status[order(days)])),
      days = NULL
    ) %>%
    nice_names()
  
  datatable(
    x,
    rownames = FALSE,
    filter = list(position = 'top', plain = TRUE),
    options = list(dom = 'ltpi'),
    style = "default"
  )
})


# CLIENT COUNT SUMMARY - APP ----------------------------------------------

output$clientCountSummary <- renderDT({
  req(session$userData$valid_file() == 1)
  
  exportTestValues(clientCountSummary = client_count_summary_df())
  
  datatable(
    client_count_summary_df() %>%
      nice_names(),
    rownames = FALSE,
    filter = 'none',
    options = list(dom = 't'),
    style = "default"
  )
})


# TIMELINESS - reactive data frames ---------------------------------------

tl_df_project_start <- reactive({
  ## Time to Entry - Project Start
  left_join(
    client_count_data_df() %>% 
      filter(ProjectName == input$currentProviderList) %>% 
      rename(ProjectStartDate = EntryDate,
                    ProjectExitDate = ExitDate),
    Enrollment %>% select(PersonalID,EnrollmentID,Enrollment.EntryDate = EntryDate, Enrollment.DateCreated = DateCreated)
    
  ) %>% 
    filter(ProjectStartDate >=  input$dateRangeCount[1], ProjectStartDate <=  input$dateRangeCount[2]) %>% 
    mutate(TimeToEntry = as.numeric(as.Date(Enrollment.DateCreated) - ProjectStartDate)) %>% 
    summarize(
      mdn = median(TimeToEntry, na.rm=T),
      nlt0 = sum(TimeToEntry < 0, na.rm=T),
      n0 = sum(TimeToEntry == 0, na.rm=T),
      n1_3 = sum(TimeToEntry >= 1 & TimeToEntry <= 3, na.rm=T),
      n4_6 = sum(TimeToEntry >= 4 & TimeToEntry <= 6, na.rm=T),
      n7_10 = sum(TimeToEntry >= 7 & TimeToEntry <= 10, na.rm=T),
      n11p = sum(TimeToEntry >= 11, na.rm=T)
    )
})

tl_df_project_exit <- reactive({
  ## Time to Entry - Project Exit
  left_join(
    client_count_data_df() %>% 
      filter(ProjectName == input$currentProviderList) %>% 
      rename(ProjectStartDate = EntryDate,
                                                    ProjectExitDate = ExitDate),
    Exit %>% select(PersonalID,EnrollmentID, Exit.ExitDate = ExitDate, Exit.DateCreated = DateCreated)
  ) %>%  
    filter(ProjectExitDate >=  input$dateRangeCount[1], ProjectExitDate <=  input$dateRangeCount[2]) %>% 
    mutate(TimeToEntry = as.numeric(ProjectExitDate - as.Date(Exit.DateCreated) )) %>% 
    summarize(
      mdn = median(TimeToEntry,na.rm=T),
      nlt0 = sum(TimeToEntry < 0, na.rm=T),
      n0 = sum(TimeToEntry == 0, na.rm=T),
      n1_3 = sum(TimeToEntry >= 1 & TimeToEntry <= 3, na.rm=T),
      n4_6 = sum(TimeToEntry >= 4 & TimeToEntry <= 6, na.rm=T),
      n7_10 = sum(TimeToEntry >= 7 & TimeToEntry <= 10, na.rm=T),
      n11p = sum(TimeToEntry >= 11, na.rm=T)
    ) 
})

tl_df_nbn <- reactive({
  ## Time to Entry - Night by Night
  left_join(
    client_count_data_df() %>% 
      filter(ProjectName == input$currentProviderList) %>% 
      rename(ProjectStartDate = EntryDate,
                    ProjectExitDate = ExitDate),
    Services %>% rename(Services.DateCreated = DateCreated, Services.DateProvided = DateProvided)
  ) %>% 
    #filter(!is.na(Services.DateCreated)) %>% 
    filter(Services.DateProvided >=  input$dateRangeCount[1], Services.DateProvided <=  input$dateRangeCount[2]) %>% 
    mutate(TimeToEntry = as.numeric(as.Date(Services.DateCreated) - as.Date(Services.DateProvided)),
           diff = as.numeric(difftime(Services.DateCreated, Services.DateProvided, units="hours"))) %>% 
    summarize(
      pct_lt24 = mean(diff < 24, na.rm=T),
      pct_lt48 = mean(diff < 48, na.rm=T),
      nlt0 = sum(TimeToEntry < 0, na.rm=T),
      n0 = sum(TimeToEntry == 0, na.rm=T),
      n1_3 = sum(TimeToEntry >= 1 & TimeToEntry <= 3, na.rm=T),
      n4_6 = sum(TimeToEntry >= 4 & TimeToEntry <= 6, na.rm=T),
      n7_10 = sum(TimeToEntry >= 7 & TimeToEntry <= 10, na.rm=T),
      n11p = sum(TimeToEntry >= 11, na.rm=T)
    )
})

tl_df_cls <- reactive({
  ## Time to Entry - CLS
  left_join(
    client_count_data_df() %>% 
      filter(ProjectName == input$currentProviderList) %>% 
      rename(ProjectStartDate = EntryDate,
                    ProjectExitDate = ExitDate),
    CurrentLivingSituation %>% 
      select(PersonalID, EnrollmentID, CurrentLivingSituation.DateCreated = DateCreated, CurrentLivingSituation.InformationDate = InformationDate)
    
  ) %>% filter(!is.na(CurrentLivingSituation.DateCreated)) %>% 
    filter(CurrentLivingSituation.InformationDate >=  input$dateRangeCount[1], CurrentLivingSituation.InformationDate <=  input$dateRangeCount[2]) %>% 
    mutate(TimeToEntry = as.numeric(as.Date(CurrentLivingSituation.DateCreated) - as.Date(CurrentLivingSituation.InformationDate)),
           diff = as.numeric(difftime(CurrentLivingSituation.DateCreated, CurrentLivingSituation.InformationDate, units="hours"))) %>% 
    summarize(
      pct_lt24 = mean(diff < 24, na.rm=T),
      pct_lt48 = mean(diff < 48, na.rm=T),
      nlt0 = sum(TimeToEntry < 0, na.rm=T),
      n0 = sum(TimeToEntry == 0, na.rm=T),
      n1_3 = sum(TimeToEntry >= 1 & TimeToEntry <= 3, na.rm=T),
      n4_6 = sum(TimeToEntry >= 4 & TimeToEntry <= 6, na.rm=T),
      n7_10 = sum(TimeToEntry >= 7 & TimeToEntry <= 10, na.rm=T),
      n11p = sum(TimeToEntry >= 11, na.rm=T)
    )
  
})
# CLIENT COUNT DOWNLOAD ---------------------------------------------------

output$downloadClientCountsReportButton  <- renderUI({
  req(session$userData$valid_file() == 1)
  downloadButton(outputId = "downloadClientCountsReport",
                 label = "Download System-Wide")
})

# the download basically contains a pivoted and summarized version of the
# two app tables, but for all projects along with a Current tab limited to
# just the current date.
output$downloadClientCountsReport <- downloadHandler(
  filename = date_stamped_filename("Client Counts Report-"),
  content = get_clientcount_download_info
)