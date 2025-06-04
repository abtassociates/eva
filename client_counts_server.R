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
    filter(EntryDate <= ReportEnd &
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
  
  ### session$userData$validation CURRENT TAB ###
  # counts for each status, by project for just the current date
  validationCurrent <- 
    pivot_and_sum(
      validationDF %>%
        filter(EntryDate <= input$dateRangeCount[2] &
                 (is.na(ExitDate) | ExitDate >= input$dateRangeCount[2]))
    ) %>%
    select(-c(`Currently in project`, ProjectType)) %>%
    arrange(OrganizationName, ProjectName)

  ### session$userData$validation DETAIL TAB ###
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
    filter(str_detect(Status, "Exit", negate = TRUE)) %>%
    mutate(Status = factor(
      case_when(
        str_detect(Status, "Currently in") ~ "Currently in project",
        str_detect(Status, "Currently Moved") ~ "Currently Moved In",
        TRUE ~ Status
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
    left_join(detail_order, by = "ProjectType") %>%
    group_by(ProjectType) %>%
    arrange(ProjectType, desc(Total)) %>%
    mutate(
      movedin = lag(Total, default = 0),
      text_position = case_when(
        !ProjectType %in% c(ph_project_types) ~ InProject / 2,
        ProjectType %in% c(ph_project_types) ~ 
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
    filter(ProjectName == input$currentProviderList) %>%
    select(all_of(clientCountDetailCols)) %>%
    nice_names()
  
  datatable(
    x,
    rownames = FALSE,
    filter = 'top',
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