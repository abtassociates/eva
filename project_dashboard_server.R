##### REPORTING COLUMNS ######
# these are the main columns that we will report out in the app and exports
clientCountDetailCols <- c("PersonalID",
                           "EnrollmentID",
                           "RelationshipToHoH",
                           "EntryDate",
                           "MoveInDateAdjust",
                           "ExitDate",
                           "Status")

time_cols <- c("nlt0","n0","n1_3","n4_6","n7_10","n11p")

##### MAIN DATAFRAME ######
# this is the primary client count dataset, calculating
# their status and number of days enrolled. 
# This will be used to create the summary and detail datasets used in the
# app, as well as the datasets used in the export
client_count_data_df <- reactive({
  ReportStart <- input$dateRangeCount[1]
  ReportEnd <- input$dateRangeCount[2]
  
  validate(
    need(
      !is.na(ReportStart) && !is.na(ReportEnd) && ReportEnd > ReportStart,
      message = "Please input a valid date range. The date range you set is outside the date range of your current file."
    )
  )
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
  req(session$userData$valid_file() == 1)
  req(!is.null(input$currentProviderList))
  
  client_counts <- client_count_data_df() %>%
    fsubset(ProjectID == input$currentProviderList) %>%
    fgroup_by(Status)
  if(nrow(client_counts) == 0){
    return(NULL)
  }
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
  "ProjectName",
  "ProjectType"
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
    fselect(c(keepCols, "Status", "PersonalID")) %>%
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
    )

  return(pivoted)
}

clean_timeliness_df <- function(tl_df, record_type, orgList = unique(client_count_data_df()$OrganizationName)){

  validationDF <- tl_df %>% 
    fsubset(OrganizationName %in% orgList) %>% 
    fselect(c(keepCols, time_cols, "mdn")) %>%  
    fmutate(ProjectType = project_type(ProjectType)) %>% 
    roworder(OrganizationName, ProjectName) %>% 
    nice_names_timeliness(record_type = record_type)
  
  return(validationDF)
}

get_project_dashboard_download_info <- function(orgList = unique(client_count_data_df()$OrganizationName),
                                          dateRangeEnd = input$dateRangeCount[2]) {
  logToConsole(session, "in get_project_dashboard_download_info")
   client_counts_metadata <- data.table(
    Chart = c(
      "Export Date",
      "Export Start",
      "Export End",
      "Start Date",
      "End Date",
      "Timeliness: Max Record Entry Days"
    ),
    Value = c(
      strftime(session$userData$meta_HUDCSV_Export_Date,"%m/%d/%y"),
      strftime(session$userData$meta_HUDCSV_Export_Start,"%m/%d/%y"),
      strftime(session$userData$meta_HUDCSV_Export_End,"%m/%d/%y"),
      strftime(input$dateRangeCount[1], "%m/%d/%y"),
      strftime(input$dateRangeCount[2], "%m/%d/%y"),
      input$timeliness_metric
    )
  )
  
  # initial dataset that will make summarizing easier
  validationDF <- client_count_data_df() %>% 
    fsubset(OrganizationName %in% orgList)

  ### session$userData$validation DATE RANGE TAB ###
  # counts for each status, by project, across the date range provided
  if(!is.null(validationDF) & fnrow(validationDF) > 0){
    pivot_att <- tryCatch(
      pivot_and_sum(
        validationDF, isDateRange = TRUE
      ), 
      error = function(e){e}
    )
    
    if(inherits(pivot_att, 'simpleError')){
      logToConsole(session, 'Project Dashboard error: pivot for validationFullExportRange has no rows of data to use.')
      validationFullExportRange <- NULL
    } else {
      validationFullExportRange <- pivot_att %>%
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
        fmutate(ProjectType = project_type(ProjectType)) %>% 
        roworder(OrganizationName, ProjectName)
    }
    
  } else {
    logToConsole(session, "validationDF is NULL or has 0 rows. validationFullExportRange set to NULL.")
    
    validationFullExportRange <- NULL
  }

  ### CURRENT TAB ###
  # counts for each status, by project for just the current date
  if(!is.null(validationDF) & fnrow(validationDF) > 0){
    
    validation_filt <- validationDF %>%
      fsubset(EntryDate <= dateRangeEnd &
                (is.na(ExitDate) | ExitDate >= dateRangeEnd))
    if(is.null(validation_filt) | fnrow(validation_filt) == 0){
      logToConsole(session, 'Project Dashboard error: pivot for validationDateRange has no rows of data to use.')
      validationDateRange <- NULL
     
    } else {
      pivot_att <- tryCatch(
        pivot_and_sum(
         validation_filt, 
        ), error = function(e){e}
      )
      
      if(inherits(pivot_att, 'simpleError')){
        logToConsole(session, 'Project Dashboard error: pivot for validationDateRange has no rows of data to use.')
        validationDateRange <- NULL
      } else {
        validationDateRange <- pivot_att %>%
          fmutate(ProjectType = project_type(ProjectType)) %>% 
          roworder(OrganizationName, ProjectName)
      }
    }
   
  } else {
    logToConsole(session, "validationDF is NULL or has 0 rows. validationDateRange set to NULL.")
    validationDateRange <- NULL
  }

  ### DETAIL TAB ###
  if(!is.null(validationDF) & fnrow(validationDF) > 0){
    validationDetail <- validationDF %>% # full dataset for the detail
      fmutate(
        Status = fifelse(
          Status %in% c("Currently Moved In", "Currently in Project"), 
          paste0(Status, " (", days, " days)"),
          as.character(Status)
        ),
        ProjectType = project_type(ProjectType)
      ) %>%
      fselect(c(keepCols, clientCountDetailCols)) %>%
      roworder(OrganizationName, ProjectName, EntryDate)
  } else {
    logToConsole(session, "validationDF is NULL or has 0 rows. validationDetail set to NULL.")
    validationDetail <- NULL
  }

  if(!is.null(tl_df_project_start())){
    validationStart <- clean_timeliness_df(tl_df_project_start(), record_type = 'start')
  } else {
    validationStart <- NULL
  }

  if(!is.null(tl_df_project_exit())){
    validationExit <- clean_timeliness_df(tl_df_project_exit(), record_type = 'exit')
  } else {
    validationExit <- NULL
  }
 
  if(!is.null(tl_df_cls())){
    validationCLS <- clean_timeliness_df(tl_df_cls(), record_type = 'cls')
  } else {
    validationCLS <- NULL
  }

  if(!is.null(tl_df_nbn())){
    validationNbN <- clean_timeliness_df(tl_df_nbn(), record_type = 'nbn')
    
  } else {
    validationNbN <- NULL
  }
  
  if(!is.null(tl_df_ce_assess())){
    validationCEAssess <- clean_timeliness_df(tl_df_ce_assess(), record_type = 'ce_assess')
    
  } else {
    validationCEAssess <- NULL
  }
  
  if(!is.null(tl_df_ce_event())){
    validationCEEvent <- clean_timeliness_df(tl_df_ce_event(), record_type = 'ce_event')
    
  } else {
    validationCEEvent <- NULL
  }
  
  exportDFList <- list(
    Metadata = client_counts_metadata,
    validationDateRange = validationDateRange %>% nice_names(),
    validationFullExportRange = validationFullExportRange %>% nice_names(),
    validationDetail = validationDetail %>% nice_names(),
    validationStart = validationStart,
    validationExit = validationExit
  )
  
  names(exportDFList) = c(
    "Metadata", 
    "ClientCounts - Date Range",
    "ClientCounts-Full Export Range",
    "ClientCounts - Detail",
    "Timeliness - Project Start",
    "Timeliness - Project Exit"
  )

  exportTestValues(
    client_count_download_date_range = summarize_df(validationDateRange %>% nice_names())
  )
  exportTestValues(
    client_count_download_full_export_range = summarize_df(validationFullExportRange %>% nice_names())
  )
  exportTestValues(
    client_count_download_detail = validationDetail %>% nice_names()
  )
  
  exportTestValues(
    client_count_download_timeliness_start = summarize_df(validationStart %>% nice_names_timeliness(record_type = 'start'))
  )
  
  exportTestValues(
    client_count_download_timeliness_exit = summarize_df(validationExit %>% nice_names_timeliness(record_type = 'exit'))
  )

  if(!is.null(validationCLS)){
    exportDFList[[length(exportDFList) + 1]] <- validationCLS
    names(exportDFList)[[length(exportDFList)]] <- "Timeliness - CLS"
    exportTestValues(
      client_count_download_timeliness_cls = summarize_df(validationCLS %>% nice_names_timeliness(record_type = 'cls'))
    )
  }
  
  if(!is.null(validationNbN)){
    exportDFList[[length(exportDFList) + 1]] <- validationNbN
    names(exportDFList)[[length(exportDFList)]] <- "Timeliness - Bed-Night Dates"
    exportTestValues(
      client_count_download_timeliness_nbn = summarize_df(validationNbN %>% nice_names_timeliness(record_type = 'nbn'))
    )
  }
  
  if(!is.null(validationCEAssess)){
    exportDFList[[length(exportDFList) + 1]] <- validationCEAssess
    names(exportDFList)[[length(exportDFList)]] <- "Timeliness - CE Assessment"
    exportTestValues(
      client_count_download_timeliness_ce_assess = summarize_df(validationCEAssess %>% nice_names_timeliness(record_type = 'ce_assess'))
    )
  }
  
  if(!is.null(validationCEEvent)){
    exportDFList[[length(exportDFList) + 1]] <- validationCEEvent
    names(exportDFList)[[length(exportDFList)]] <- "Timeliness - CE Event"
    exportTestValues(
      client_count_download_timeliness_ce_event = summarize_df(validationCEEvent %>% nice_names_timeliness(record_type = 'ce_event'))
    )
  }
  logToConsole(session, "returning from get_project_dashboard_download_info")
  
  
  return(exportDFList[lengths(exportDFList) > 0])
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
#         "Currently in Project" = get_brand_color('blue'),
#         "Active No Move-In" = get_brand_color('light_purple'),
#         "Currently Moved In" = get_brand_color('sea_green')
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
#       title = element_text(colour = get_brand_color('dark_grey')),
#       legend.position = "top"
#     )
#   
#   validate_by_org
# })

# CLIENT COUNT DETAILS - APP ----------------------------------------------
output$clientCountData <- renderDT({
  req(session$userData$valid_file() == 1)
  req(nrow(session$userData$validation) > 0)
  
  # getting an error sometimes? Warning: Error in filter: â„¹ In argument: `ProjectID == input$currentProviderList`.
  # Caused by error:
  #   ! `..1` must be of size 292 or 1, not size 0.
  x <- client_count_data_df() %>%
    fsubset(ProjectID == input$currentProviderList) %>%
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
  
  validate(
    need(
      nrow(client_count_summary_df()) > 0,
      message = no_data_msg
    )
  )
  
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

calc_time_to_entry <- function(df){
  df %>% 
    fgroup_by(ProjectID) %>% 
    fsummarize(
      OrganizationName = ffirst(OrganizationName),
      ProjectName = ffirst(ProjectName),
      ProjectType = ffirst(ProjectType),
      n_records = GRPN(),
      n_lt_metric = fsum(DaysToEntry <= input$timeliness_metric),
      mdn = fmedian(DaysToEntry),
      nlt0 = fsum(DaysToEntry < 0),
      n0 = fsum(DaysToEntry == 0),
      n1_3 = fsum(DaysToEntry >= 1 & DaysToEntry <= 3),
      n4_6 = fsum(DaysToEntry >= 4 & DaysToEntry <= 6),
      n7_10 = fsum(DaysToEntry >= 7 & DaysToEntry <= 10),
      n11p = fsum(DaysToEntry >= 11)
    ) %>% 
    fungroup()
}

## create rows of zeros for any projects without Project Start records 
pad_missing_projects <- function(df, base_df) {
  diff_ids <- setdiff(base_df$ProjectID, df$ProjectID)
  if (length(diff_ids) == 0) return(df)
  
  zeros <- base_df %>% 
    fsubset(ProjectID %in% diff_ids) %>% 
    gby(ProjectID) %>% 
    ffirst() %>% 
    fungroup() %>% 
    fselect(ProjectID, OrganizationName, ProjectName, ProjectType) %>% 
    fmutate(
      n_records = 0, n_lt_metric = 0, mdn = NA, nlt0 = 0, 
      n0 = 0, n1_3 = 0, n4_6 = 0, n7_10 = 0, n11p = 0
    )
  df <- rowbind(df, zeros)
}

process_timeliness_df <- function(join_df, date_col_name, date_range, how, type) {
  res_df <- join(
    client_count_data_df(), 
    join_df, 
    how = how
  ) %>% 
    fsubset(get(date_col_name) %between% date_range) %>% 
    fmutate(
      DaysToEntry = as.numeric(as.Date(DateCreated) - as.Date(get(date_col_name)))
    )
  
  if(fnrow(res_df) == 0) return(NULL) else calc_time_to_entry(res_df)
}

# Shared processing function for non-project timelines
make_timeliness_reactive <- function(
    join_df,
    date_col_name,
    how = "inner",
    type = ""
) {
  
  reactive({
    req(session$userData$valid_file() == 1)
    
    df <- process_timeliness_df(
      join_df = join_df,
      date_col_name = date_col_name,
      date_range = input$dateRangeCount,
      how = how,
      type = type
    )
    
    if(type %in% c("start", "exit"))
      df <- df |> pad_missing_projects(client_count_data_df())
    
    return(df)
  })
}

tl_df_project_start <- make_timeliness_reactive(
  join_df = session$userData$Enrollment %>% fselect(PersonalID, EnrollmentID, DateCreated),
  date_col_name = "EntryDate",
  how = "left",
  type = "start"
)

tl_df_project_exit <- make_timeliness_reactive(
  join_df = session$userData$Exit %>% fselect(PersonalID, EnrollmentID, DateCreated),
  date_col_name = "ExitDate",
  how = "left",
  type = "exit"
)

tl_df_nbn <- make_timeliness_reactive(
  session$userData$Services,
  "DateProvided",
  "left"
)

tl_df_cls <- make_timeliness_reactive(
  session$userData$CurrentLivingSituation %>%
    fselect(PersonalID, EnrollmentID, DateCreated, InformationDate),
  "InformationDate"
)

tl_df_ce_assess <- make_timeliness_reactive(
  session$userData$Assessment %>% 
    fselect(PersonalID, EnrollmentID, DateCreated, AssessmentDate),
  "AssessmentDate",
  "inner"
)

## Timeliness - Coordinated Entry (CE) Event Records
tl_df_ce_event <- make_timeliness_reactive(
  session$userData$Event %>% 
    fselect(EventID, EnrollmentID, DateCreated, EventDate),
  "EventDate",
  "inner"
)


# TIMELINESS - value boxes ------------------------------------------------
cc_project_type <- reactive({
  req(session$userData$valid_file() == 1)
  
  cc_filt <- (client_count_data_df() %>% 
    fsubset(ProjectID == input$currentProviderList) %>% pull(ProjectType))
  
  if(length(cc_filt) > 0){
    cc_filt[1]
  } else {
    NULL
  }
})

output$timeliness_vb1_val <- renderText({
  req(session$userData$valid_file() == 1)
  
  if(!is.null(tl_df_project_start()) && input$currentProviderList %in% tl_df_project_start()$ProjectID){
    tl_df_project_start() %>%  
      fsubset(ProjectID == input$currentProviderList) %>% 
      pull(mdn)
  } else {
    '-'
  }
  
})

output$timeliness_vb2_val <- renderText({
  req(session$userData$valid_file() == 1)
  
  if(!is.null(tl_df_project_exit()) && input$currentProviderList %in% tl_df_project_exit()$ProjectID){
    tl_df_project_exit() %>% 
      fsubset(ProjectID == input$currentProviderList) %>% 
      pull(mdn)
  } else {
    '-'
  }
 
})

output$timeliness_vb3 <- renderUI({
  req(session$userData$valid_file() == 1)
  
  # Group reactives in a list to calculate programmatically
  reactives_list <- list(
    tl_df_project_start(), 
    tl_df_project_exit(), 
    tl_df_nbn(), 
    tl_df_cls(), 
    tl_df_ce_assess(), 
    tl_df_ce_event()
  )
  
  sums <- lapply(reactives_list, function(df) {
    if (is.null(df)) return(c(num = 0, den = 0))
    sub_df <- fsubset(df, ProjectID == input$currentProviderList)
    if (nrow(sub_df) == 0) return(c(num = 0, den = 0))
    
    c(num = sum(sub_df$n_lt_metric, na.rm = TRUE), 
      den = sum(sub_df$n_records, na.rm = TRUE))
  })
  
  total_num <- sum(sapply(sums, `[[`, "num"))
  total_den <- sum(sapply(sums, `[[`, "den"))
  
  val <- if (total_den == 0) "-" else scales::percent(total_num / total_den, accuracy = 1)
  
  value_box(
    title = paste0("Percent of Records Entered within ", input$timeliness_metric, " Days"),
    value = val,
    showcase = bs_icon("clock"),
    theme = "text-primary",
    class = "border-primary"
  )
})


# TIMELINESS DT table ----------------------------------------------
pull_time_cols <- function(cond, df, set_zero = TRUE) {
  # If the column is not relevant to this project type, omit it entirely
  if (!cond) return(NULL)
  
  x <- df %>%
    fsubset(ProjectID == input$currentProviderList) %>%
    fselect(time_cols)
    
  
  if (fnrow(x) == 0)
    return(if (set_zero) 0 else NULL)
  
  # Select and unlist the timeliness columns
  return(unlist(x))
}

output$timelinessTable <- renderDT({
  req(session$userData$valid_file() == 1)

  validate(
    need(
      length(cc_project_type()) > 0 && !is.na(cc_project_type()),
      message = no_data_msg
    )
  )
  
  dat <-  data.frame(
    time_period = c("< 0 days", "0 days", "1-3 days", "4-6 days", "7-10 days", "11+ days")
  )

  # 1. Project Start & Exit: Always displayed
  dat$proj_start <- pull_time_cols(
    cond = TRUE,
    df = tl_df_project_start(),
    set_zero = TRUE
  )
  
  dat$proj_exit <- pull_time_cols(
    cond = TRUE,
    df = tl_df_project_exit(),
    set_zero = TRUE
  )
  
  # 2. Bed Night: Displayed only for Emergency Shelter - Night-by-Night projects
  #    If it matches the type but has no records, we omit the column
  dat$nbn <- pull_time_cols(
    cond = cc_project_type() == es_nbn_project_type, 
    df = tl_df_nbn(), 
    set_zero = FALSE
  )
  
  # 3. Current Living Situation (CLS):
  #    - Always display for ES-NbN, Street Outreach, and Coordinated Entry (even if 0 records).
  #    - For other project types, display only if they have records.
  is_always_cls <- cc_project_type() %in% c(es_nbn_project_type, out_project_type, ce_project_type)
  dat$cls <- pull_time_cols(
    cond = is_always_cls || input$currentProviderList %in% tl_df_cls()$ProjectID, 
    df = tl_df_cls(),
    set_zero = is_always_cls
  )
  
  # 4. CE Assessment:
  #    - Always display for CE projects (even if 0 records).
  #    - For non-CE projects, display only if they have records.
  is_ce_project <- cc_project_type() == ce_project_type
  dat$ce_assess <- pull_time_cols(
    cond = is_ce_project || input$currentProviderList %in% tl_df_ce_assess()$ProjectID, 
    df = tl_df_ce_assess(),
    set_zero = is_ce_project
  )
  
  # 5. CE Event:
  #    - Always display for CE projects (even if 0 records).
  #    - For non-CE projects, display only if they have records.
  dat$ce_event <- pull_time_cols(
    cond = is_ce_project || input$currentProviderList %in% tl_df_ce_event()$ProjectID, 
    df = tl_df_ce_event(),
    set_zero = is_ce_project
  )
  
  tbl_names <- c(
    "Time for Record Entry" = "time_period", 
    "Number of Project Start Records" = "proj_start", 
    "Number of Project Exit Records" = "proj_exit", 
    "Number of Bed Night Records" = "nbn", 
    "Number of Current Living Situation Records" = "cls",
    "Number of CE Assessment Records" = "ce_assess", 
    "Number of CE Event Records" = "ce_event"
  )
  
  dat <- dat %>% rename(any_of(tbl_names))
  exportTestValues(timelinessTable = dat)
  
  datatable(
    dat,
    rownames = FALSE,
    filter = "none",
    selection = "none",
    options = list(dom = 't', ordering = FALSE),
    style = "default"
  )
})

# CLIENT COUNT DOWNLOAD ---------------------------------------------------

output$downloadProjectDashboardReportButton  <- renderUI({
  req(session$userData$valid_file() == 1)
  downloadButton(outputId = "downloadProjectDashboardReport",
                 label = "Download System-Wide")
})

# the download basically contains a pivoted and summarized version of the
# two app tables, but for all projects along with a Current tab limited to
# just the current date.
output$downloadProjectDashboardReport <- downloadHandler(
  filename = date_stamped_filename("System-level Project Dashboard Report-"),
  content = function(file){
    logMetadata(session, paste0("Downloaded Project Dashboard Report with Date Range = [",
                                paste0(input$dateRangeCount, collapse=', '),']',
                                if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    df_xl <- get_project_dashboard_download_info()
   
    write_xlsx(df_xl,
               path = file)
  }
)
