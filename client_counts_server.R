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
      Status = factor(
        fcase(
          ProjectType %in% c(ph_project_types) & is.na(MoveInDateAdjust) & is.na(ExitDate), "Active No Move-In",
          ProjectType %in% c(ph_project_types) & !is.na(MoveInDateAdjust) & is.na(ExitDate), "Currently Moved In",
          ProjectType %in% c(ph_project_types) & is.na(MoveInDateAdjust) & !is.na(ExitDate), "Exited No Move-In",
          ProjectType %in% c(ph_project_types) & !is.na(MoveInDateAdjust) & !is.na(ExitDate), "Exited with Move-In",
          !ProjectType %in% c(ph_project_types) & is.na(ExitDate), "Currently in Project",
          !ProjectType %in% c(ph_project_types) & !is.na(ExitDate), "Exited Project"
        ),
        levels = c("Currently in Project", "Active No Move-In", "Currently Moved In", "Exited Project", "Exited No Move-In", "Exited with Move-In")
      ),
      
      days = fcase(
        Status == "Currently Moved In", ReportEnd - MoveInDateAdjust,
        Status == "Currently in Project", ReportEnd - EntryDate
      )
    ) %>%
    roworder(-days, HouseholdID, PersonalID) %>% 
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
      ProjectType,
      days
    ) %>%
    fsubset(EntryDate <= ReportEnd &
             (is.na(ExitDate) | ExitDate >= ReportStart))
})

##### SUMMARY STUFF ######
# this reactive df is the one used for the summary table in the app. 
# using the function above, it gets and then combines the counts of households
# and people/clients
client_count_summary_df <- reactive({
  client_counts <- client_count_data_df() %>%
    fsubset(ProjectName == input$currentProviderList) %>%
    fgroup_by(Status)
  
  hhs <- client_counts %>% fsummarise(Households = fnunique(HouseholdID))
  clients <- client_counts %>% fsummarise(Clients = fnunique(PersonalID))

  join(clients, hhs, on = "Status", how="full")
})


##### DOWNLOADING STUFF ######
# make sure these columns are there; they wouldn't be after pivoting if nobody had that status
necessaryCols <- c(
  "Currently in Project",
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
    "Exited Project",
    "Exited with Move-In",
    "Exited No Move-In"
  )
  
  pivoted <- df %>%
    fselect(c(keepCols, "Status", "ProjectType", "PersonalID")) %>%
    funique() %>%
    pivot(how="wider", names = "Status", values = "PersonalID", FUN = "count", sort="names", drop=FALSE) %>%
    fmutate(
      across(
        necessaryCols, 
        \(x) fifelse(is.na(x) & .$ProjectType %in% c(psh_project_type, rrh_project_type), 0, x)
      ),
      "Currently in Project" = fifelse(
        ProjectType %in% ph_project_types, 
        rowSums(
          fselect(., `Currently Moved In`, `Active No Move-In`),
          na.rm = TRUE
        ),
        `Currently in Project`
      )
    ) %>%
    roworder(OrganizationName, ProjectID)

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
    fmutate(
      "Exited Project" = fifelse(
        ProjectType %in% ph_project_types, 
        rowSums(
          fselect(., `Exited with Move-In`, `Exited No Move-In`),
          na.rm = TRUE
        ),
        `Exited Project`
      )
    ) %>%
    fselect(-ProjectType) %>%
    roworder(OrganizationName, ProjectName)
  
  ### CURRENT TAB ###
  # counts for each status, by project for just the current date
  validationLatest <- 
    pivot_and_sum(
      validationDF %>%
        fsubset(EntryDate <= input$dateRangeCount[2] &
                 (is.na(ExitDate) | ExitDate >= input$dateRangeCount[2]))
    ) %>%
    fselect(-ProjectType) %>%
    roworder(OrganizationName, ProjectName)

  ### DETAIL TAB ###
  validationDetail <- validationDF %>% # full dataset for the detail
    fmutate(Status = paste0(Status, " (", days, " days)")) %>%
    fselect(c(keepCols, clientCountDetailCols)) %>%
    roworder(OrganizationName, ProjectName, EntryDate)
  
  exportDFList <- list(
    validationLatest = validationLatest %>% nice_names(),
    validationDateRange = validationDateRange %>% nice_names(),
    validationDetail = validationDetail %>% nice_names()
  )
  
  names(exportDFList) = c(
    "validation - Latest in Range",
    "validation - Date Range",
    "validation - Detail"
  )
  
  exportTestValues(
    client_count_download_current = summarize_df(validationLatest %>% nice_names())
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


# output$validate_plot <- renderPlot({
#   req(session$userData$valid_file() == 1)
#   
#   detail <- client_count_data_df() %>%
#     fsubset(str_detect(Status, "Exit", negate = TRUE)) %>%
#     fcount(ProjectType, Status, name = "Total")
#   
#   detail_order <- detail %>%
#     fgroup_by(ProjectType) %>%
#     fsummarise(InProject = fsum(Total, na.rm = FALSE)) %>%
#     fungroup()
#   
#   
#   plot_data <- detail %>%
#     join(detail_order, on = "ProjectType", how = 'left') %>%
#     fgroup_by(ProjectType) %>%
#     roworder(ProjectType, desc(Total)) %>%
#     fmutate(
#       movedin = flag(Total, default = 0),
#       text_position = fcase(
#         !ProjectType %in% c(ph_project_types), InProject / 2,
#         ProjectType %in% c(ph_project_types), 
#           Total / 2 + movedin
#       )
#     )
#   
#   validate_by_org <-
#     ggplot(
#       plot_data,
#       aes(x = reorder(project_type_abb(ProjectType), InProject),
#           y = Total, fill = Status)
#     ) +
#     geom_col(alpha = .7, position = "stack")  +
#     geom_text(aes(label = prettyNum(Total, big.mark = ","),
#                   y = text_position),
#               color = "gray14")+
#     scale_y_continuous(label = comma_format()) +
#     scale_colour_manual(
#       values = c(
#         "Currently in Project" = "#71B4CB",
#         "Active No Move-In" = "#7F5D9D",
#         "Currently Moved In" = "#52BFA5"
#       ),
#       aesthetics = "fill"
#     ) +
#     labs(
#       title = "Current System-wide Counts",
#       x = "",
#       y = ""
#     ) +
#     theme_minimal(base_size = 18) +
#     theme(
#       plot.title.position = "plot",
#       title = element_text(colour = "#73655E"),
#       legend.position = "top"
#     )
#   
#   validate_by_org
# })

# CLIENT COUNT DETAILS - APP ----------------------------------------------
output$clientCountData <- renderDT({
  req(session$userData$valid_file() == 1)
  req(nrow(session$userData$validation) > 0)
  
  # getting an error sometimes? Warning: Error in filter: â„¹ In argument: `ProjectName == input$currentProviderList`.
  # Caused by error:
  #   ! `..1` must be of size 292 or 1, not size 0.
  
  x <- client_count_data_df() %>%
    fsubset(ProjectName == input$currentProviderList) %>%
    fselect(c(clientCountDetailCols, "days")) %>%
    fmutate(
      Status = fifelse(!is.na(days), paste0(Status, " (", days, " days)"), as.character(Status)),
      Status = factor(
        Status,
        levels = funique(Status[order(days)])
      ),
      RelationshipToHoH = as.factor(RelationshipToHoH),
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