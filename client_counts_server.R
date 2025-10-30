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
      "Currently in project" = case_when(
        ProjectType %in% c(ph_project_types)  ~ 
          rowSums(select(., `Currently Moved In`, `Active No Move-In`),
                  na.rm = TRUE),
        TRUE ~ replace_na(`Currently in project`, 0)
      )
    ) %>% 
    relocate(`Currently in project`, .after = ProjectName)
  
  return(pivoted)
}

get_clientcount_download_info <- function(file, orgList = unique(client_count_data_df()$OrganizationName),
                                          dateRangeEnd = input$dateRangeCount[2]) {
  # initial dataset that will make summarizing easier
  validationDF <- client_count_data_df() %>% 
    fsubset(OrganizationName %in% orgList)
  
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
    mutate(ProjectType = project_type(ProjectType)) %>% 
    select(-c(`Currently in project`, `Exited project`)) %>%
    arrange(OrganizationName, ProjectName)
  
  ### CURRENT TAB ###
  # counts for each status, by project for just the current date
  validationLatest <- 
    pivot_and_sum(
      validationDF %>%
        filter(EntryDate <= dateRangeEnd &
                 (is.na(ExitDate) | ExitDate >= dateRangeEnd))
    ) %>%
    select(-c(`Currently in project`)) %>%
    mutate(ProjectType = project_type(ProjectType)) %>% 
    arrange(OrganizationName, ProjectName)

  ### DETAIL TAB ###
  validationDetail <- validationDF %>% # full dataset for the detail
    select(!!keepCols, ProjectType, !!clientCountDetailCols) %>%
    mutate(ProjectType = project_type(ProjectType)) %>% 
    arrange(OrganizationName, ProjectName, EntryDate)
  
  validationStart <- tl_df_project_start() %>% 
    fsubset(OrganizationName %in% orgList) %>% 
    select(!!keepCols, ProjectType, nlt0, n0, n1_3, n4_6, n7_10, n11p, mdn) %>%  
    mutate(ProjectType = project_type(ProjectType)) %>% 
    arrange(OrganizationName, ProjectName) %>% 
    nice_names_timeliness(record_type = 'start')
  
  validationExit <- tl_df_project_exit() %>% 
    fsubset(OrganizationName %in% orgList) %>% 
    select(!!keepCols, ProjectType, nlt0, n0, n1_3, n4_6, n7_10, n11p, mdn) %>%
    mutate(ProjectType = project_type(ProjectType)) %>% 
    arrange(OrganizationName, ProjectName) %>% 
    nice_names_timeliness(record_type = 'exit')
 
  if(!is.null(tl_df_cls())){
    validationCLS <- tl_df_cls() %>% 
      fsubset(OrganizationName %in% orgList) %>% 
      select(!!keepCols, ProjectType, nlt0, n0, n1_3, n4_6, n7_10, n11p, mdn) %>%
      mutate(ProjectType = project_type(ProjectType)) %>% 
      arrange(OrganizationName, ProjectName) %>% 
      nice_names_timeliness(record_type = 'cls')
  } else {
    validationCLS <- NULL
  }
  
  if(!is.null(tl_df_nbn())){
    validationNbN <- tl_df_nbn() %>% 
      fsubset(OrganizationName %in% orgList) %>% 
      select(!!keepCols, ProjectType, nlt0, n0, n1_3, n4_6, n7_10, n11p, mdn) %>%
      mutate(ProjectType = project_type(ProjectType)) %>% 
      arrange(OrganizationName, ProjectName) %>% 
      nice_names_timeliness(record_type = 'nbn')
  } else {
    validationNbN <- NULL
  }
 
  
  exportDFList <- list(
    validationLatest = validationLatest %>% nice_names(),
    validationDateRange = validationDateRange %>% nice_names(),
    validationDetail = validationDetail %>% nice_names(),
    validationStart = validationStart,
    validationExit = validationExit
  )
  
  names(exportDFList) = c(
    "validation - Latest in Range",
    "validation - Date Range",
    "validation - Detail",
    "validation - Timeliness Start",
    "validation - Timeliness Exit"
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
  
  exportTestValues(
    client_count_download_timeliness_start = summarize_df(validationStart %>% nice_names_timeliness(record_type = 'start'))
  )
  
  exportTestValues(
    client_count_download_timeliness_exit = summarize_df(validationExit %>% nice_names_timeliness(record_type = 'exit'))
  )
  
  if(!is.null(validationCLS)){
    exportDFList[[length(exportDFList) + 1]] <- validationCLS
    names(exportDFList)[[length(exportDFList)]] <- "validation - Timeliness CLS"
    exportTestValues(
      client_count_download_timeliness_cls = summarize_df(validationCLS %>% nice_names_timeliness(record_type = 'cls'))
    )
  }
  
  if(!is.null(validationNbN)){
    exportDFList[[length(exportDFList) + 1]] <- validationNbN
    names(exportDFList)[[length(exportDFList)]] <- "validation - Timeliness NbN"
    exportTestValues(
      client_count_download_timeliness_nbn = summarize_df(validationNbN %>% nice_names_timeliness(record_type = 'nbn'))
    )
  }
  
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

calc_time_to_entry <- function(df){
  df %>% 
  fgroup_by(ProjectID) %>% 
    fsummarize(
      OrganizationName = ffirst(OrganizationName),
      ProjectName = ffirst(ProjectName),
      ProjectType = ffirst(ProjectType),
      n_records = GRPN(),
      #n_lt24 = fsum(HoursToEntry < 24),
      #n_lt48 = fsum(HoursToEntry < 48),
      n_lt_metric = fsum(HoursToEntry < (24 * input$timeliness_metric)),
      mdn = fmedian(DaysToEntry,na.rm=T),
      nlt0 = fsum(DaysToEntry < 0, na.rm=T),
      n0 = fsum(DaysToEntry == 0, na.rm=T),
      n1_3 = fsum(DaysToEntry >= 1 & DaysToEntry <= 3, na.rm=T),
      n4_6 = fsum(DaysToEntry >= 4 & DaysToEntry <= 6, na.rm=T),
      n7_10 = fsum(DaysToEntry >= 7 & DaysToEntry <= 10, na.rm=T),
      n11p = fsum(DaysToEntry >= 11, na.rm=T)
    ) %>% 
    fungroup()
}

tl_df_project_start <- reactive({
  req(session$userData$valid_file() == 1)
  
  ## Time to Entry - Project Start
  df_start <- join(
    client_count_data_df() %>% 
     #filter(ProjectName == input$currentProviderList) %>% 
      rename(ProjectStartDate = EntryDate),
    session$userData$Enrollment %>% fselect(PersonalID,EnrollmentID, Enrollment.DateCreated = DateCreated),
    how = "left"
  ) %>% 
    fsubset(between(ProjectStartDate, input$dateRangeCount[1], input$dateRangeCount[2])) %>% 
    fmutate(DaysToEntry = as.numeric(as.Date(Enrollment.DateCreated) - ProjectStartDate),
            HoursToEntry = as.numeric(difftime(Enrollment.DateCreated, ProjectStartDate, units="hours"))) %>% 
    calc_time_to_entry()

  ## create rows of zeros for any projects without Project Start records  
  diff_ids <- setdiff(client_count_data_df()$ProjectID,  df_start$ProjectID)
  if(length(diff_ids) > 0){
    df_start <- rowbind(
      df_start,
      client_count_data_df() %>% 
        fsubset(ProjectID %in% diff_ids) %>% 
        gby(ProjectID) %>% ffirst() %>% fungroup() %>% 
        fselect(ProjectID, OrganizationName, ProjectName, ProjectType) %>% 
        fmutate(n_records = 0, n_lt_metric = 0, mdn = NA, nlt0 = 0, n0 = 0, n1_3 = 0, n4_6 = 0, n7_10 = 0, n11p = 0)
    )
  }
  df_start
})

tl_df_project_exit <- reactive({
  req(session$userData$valid_file() == 1)
  
  ## Time to Entry - Project Exit
  df_exit <- join(
    client_count_data_df() %>% 
      #filter(ProjectName == input$currentProviderList) %>% 
      rename(ProjectExitDate = ExitDate),
    session$userData$Exit %>% fselect(PersonalID, EnrollmentID, Exit.DateCreated = DateCreated),
    how = "left"
  ) %>%  
    fsubset(between(ProjectExitDate, input$dateRangeCount[1], input$dateRangeCount[2])) %>% 
    fmutate(DaysToEntry = as.numeric(ProjectExitDate - as.Date(Exit.DateCreated) ),
            HoursToEntry = as.numeric(difftime(ProjectExitDate, Exit.DateCreated, units="hours"))) %>% 
    calc_time_to_entry()
  
  ## create rows of zeros for any projects without Project Start records  
  diff_ids <- setdiff(client_count_data_df()$ProjectID,  df_exit$ProjectID)
  if(length(diff_ids) > 0){
    df_exit <- rowbind(
      df_exit,
      client_count_data_df() %>% 
        fsubset(ProjectID %in% diff_ids) %>% 
        gby(ProjectID) %>% ffirst() %>% fungroup() %>% 
        fselect(ProjectID, OrganizationName, ProjectName, ProjectType) %>% 
        fmutate(n_records = 0, n_lt_metric = 0, mdn = NA, nlt0 = 0, n0 = 0, n1_3 = 0, n4_6 = 0, n7_10 = 0, n11p = 0)
    )
  }
  df_exit
})

tl_df_nbn <- reactive({
  req(session$userData$valid_file() == 1)
  ## Time to Entry - Night by Night
  nbn_df <- join(
    client_count_data_df(),
    #filter(ProjectName == input$currentProviderList) %>% 
    session$userData$Services %>% rename(Services.DateCreated = DateCreated, Services.DateProvided = DateProvided),
    how = "left"
  ) %>% 
    #filter(!is.na(Services.DateCreated)) %>% 
    fsubset(between(Services.DateProvided, input$dateRangeCount[1], input$dateRangeCount[2])) %>% 
    fmutate(DaysToEntry = as.numeric(as.Date(Services.DateCreated) - as.Date(Services.DateProvided)),
           HoursToEntry = as.numeric(difftime(Services.DateCreated, Services.DateProvided, units="hours"))) 
  
  if(nrow(nbn_df) > 0){
    calc_time_to_entry(nbn_df) 
  } else {
     NULL
  }
  
})

tl_df_cls <- reactive({
  req(session$userData$valid_file() == 1)
  ## Time to Entry - CLS
  cls_df <- join(
    client_count_data_df(), 
      #fsubset(ProjectName == input$currentProviderList) %>% 
    session$userData$CurrentLivingSituation %>% 
      fselect(PersonalID, EnrollmentID, CurrentLivingSituation.DateCreated = DateCreated, CurrentLivingSituation.InformationDate = InformationDate),
    how = "left"
  ) %>% 
    fsubset(!is.na(CurrentLivingSituation.DateCreated)) %>% 
    fsubset(between(CurrentLivingSituation.InformationDate, input$dateRangeCount[1], input$dateRangeCount[2])) %>% 
    fmutate(DaysToEntry = as.numeric(as.Date(CurrentLivingSituation.DateCreated) - as.Date(CurrentLivingSituation.InformationDate)),
           HoursToEntry = as.numeric(difftime(CurrentLivingSituation.DateCreated, CurrentLivingSituation.InformationDate, units="hours"))) 
    
    if(nrow(cls_df) > 0){
      calc_time_to_entry(cls_df) 
    } else {
      NULL
    }
})

# TIMELINESS - value boxes ------------------------------------------------
cc_project_type <- reactive({
  req(session$userData$valid_file() == 1)
  (client_count_data_df() %>% 
    fsubset(ProjectName == input$currentProviderList) %>% pull(ProjectType))[1]
})

output$timeliness_vb1_val <- renderText({
  req(session$userData$valid_file() == 1)

  if(input$currentProviderList %in% tl_df_project_start()$ProjectName){
    tl_df_project_start() %>%  
      fsubset(ProjectName == input$currentProviderList) %>% 
      pull(mdn)
  } else {
    '-'
  }
  
})

output$timeliness_vb2_val <- renderText({
  req(session$userData$valid_file() == 1)
  
  if(input$currentProviderList %in% tl_df_project_exit()$ProjectName){
    tl_df_project_exit() %>% 
      fsubset(ProjectName == input$currentProviderList) %>% 
      pull(mdn)
  } else {
    '-'
  }
 
})

output$timeliness_vb3 <- renderUI({
  req(session$userData$valid_file() == 1)
 
  num_hours <- 24 * input$timeliness_metric
  num_hours_var <- "n_lt_metric"
  
  if(cc_project_type() == 1 & !is.null(tl_df_nbn())){
    num_nbn <- tl_df_nbn() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(num_hours_var)
    den_nbn <- tl_df_nbn() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(n_records)
  } else {
    num_nbn <- 0
    den_nbn <- 0
  }
  
  if(cc_project_type() %in% c(0,1,6,14) & !is.null(tl_df_cls())){
    num_cls <- tl_df_cls() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(num_hours_var)
    den_cls <- tl_df_cls() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(n_records)
  } else {
    num_cls <- 0
    den_cls <- 0
  }
    
    num <- sum(
      c(
      tl_df_project_start() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(num_hours_var),
      tl_df_project_exit() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(num_hours_var),
      num_nbn,
      num_cls
      ), 
      na.rm = TRUE
    )
    den <-  sum(
      c(
      tl_df_project_start() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(n_records),
      tl_df_project_exit() %>% fsubset(ProjectName == input$currentProviderList) %>% pull(n_records),
      den_nbn,
      den_cls
      ), 
      na.rm = TRUE
    )
    val <- ifelse(den == 0, 0, num / den)
 
  if(is.nan(val) | is.na(val)){
    val <- "-"
  } else {
    val <- scales::percent(val,accuracy = 1)
  }
 
  value_box(
    title = paste0("Percent of Records Entered within ",input$timeliness_metric," Days"),
    value = val,
    showcase = bs_icon("clock"),
    theme = "text-primary",
    class = "border-primary"
  )
})

# TIMELINESS DT table ----------------------------------------------

output$timelinessTable <- renderDT({
  req(session$userData$valid_file() == 1)

  
  time_cols <- c("nlt0","n0","n1_3","n4_6","n7_10","n11p")
  
  dat <-  data.frame(
    time_period = c("< 0 days", "0 days", "1-3 days", "4-6 days", "7-10 days", "11+ days")
    )
  
  if(input$currentProviderList %in% tl_df_project_start()$ProjectName){
    dat$proj_start <- tl_df_project_start() %>% fsubset(ProjectName == input$currentProviderList) %>% fselect(time_cols) %>% unlist
  } else {
    dat$proj_start <- 0
  }
  
  if(input$currentProviderList %in% tl_df_project_exit()$ProjectName){
    dat$proj_exit <- tl_df_project_exit() %>% fsubset(ProjectName == input$currentProviderList) %>% fselect(time_cols) %>% unlist
  } else {
    dat$proj_exit <- 0
  }
  
  if(cc_project_type() == 1 & input$currentProviderList %in% tl_df_nbn()$ProjectName){
    dat$nbn = tl_df_nbn() %>% fsubset(ProjectName == input$currentProviderList) %>%  fselect(time_cols) %>% unlist
  } else {
    dat$nbn <- NULL
  }
  
  if(cc_project_type() %in% c(0,1,6,14) & input$currentProviderList %in% tl_df_cls()$ProjectName){
    dat$cls = tl_df_cls() %>% fsubset(ProjectName == input$currentProviderList) %>% fselect(time_cols) %>% unlist
  } else {
    dat$cls <- NULL
  }
  
  tbl_names <- c("Time for Record Entry" = "time_period", "Number of Project Start Records" = "proj_start", 
                 "Number of Project Exit Records" = "proj_exit", 
                     "Number of Bed Night Records" = "nbn", "Number of Current Living Situation Records" = "cls")
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

output$downloadClientCountsReportButton  <- renderUI({
  req(session$userData$valid_file() == 1)
  downloadButton(outputId = "downloadClientCountsReport",
                 label = "Download System-Wide")
})

# the download basically contains a pivoted and summarized version of the
# two app tables, but for all projects along with a Current tab limited to
# just the current date.
output$downloadClientCountsReport <- downloadHandler(
  filename = date_stamped_filename("System-level Project Dashboard Report-"),
  content = get_clientcount_download_info
)