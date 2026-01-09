#   PURPOSE: This script contains functions used by the app
#   for generating the DQ plots and DQ exports

vars_prep <- c(
  "EnrollmentID",
  "HouseholdID",
  "PersonalID",
  "OrganizationName",
  "ProjectID",
  "ProjectName",
  "ProjectType",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate"
)

vars_we_want <- c(vars_prep,
                  "Issue",
                  "Type",
                  "Guidance")

# PDDE Checker ------------------------------------------------------------
# PDDE Download Button ----------------------------------------------------
output$downloadPDDEReportButton  <- renderUI({
  req(session$userData$dq_pdde_mirai_complete() == 1)
  req(nrow(session$userData$pdde_main) > 0)
  
  downloadButton(outputId = "downloadPDDEReport",
                 label = "Download")
})


# Download Button Handler -------------------------------------------------

output$downloadPDDEReport <- downloadHandler(
  
  filename = date_stamped_filename("PDDE Report-"),
  content = function(file) {
    req(session$userData$valid_file() == 1)
    summary_df <- session$userData$pdde_main %>% 
      fgroup_by(Issue, Type) %>%
      fsummarise(Count = GRPN()) %>%
      fungroup() %>% 
      roworder(Type, Issue) 
    
    data_df <-session$userData$pdde_main %>% 
      join(session$userData$Project0 %>% fselect(ProjectID, ProjectType), on="ProjectID", how = "left") %>%
      colorder(ProjectName, ProjectType, pos="after") %>%
      fmutate(ProjectType = project_type(ProjectType)) %>% # get strings rather than codes
      roworder(Type, Issue) %>% 
      nice_names()
    
    write_xlsx(
      list("Summary" = summary_df,
           "Data" = data_df),
      path = file)
    
    logMetadata(session, paste0("Downloaded PDDE Report",
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    
    exportTestValues(pdde_download_summary = summary_df)
    exportTestValues(pdde_main = data_df)
  }
)

# summary table
output$pdde_summary_table <- renderDT({
  req(session$userData$valid_file() == 1)
  

  validate(
    need(
      nrow(session$userData$pdde_main) > 0,
      message = no_data_msg
    )
  )
  
  req(nrow(session$userData$pdde_main) > 0)
  
  a <- session$userData$pdde_main %>%
    fgroup_by(Issue, Type) %>%
    fsummarise(Count = GRPN()) %>%
    fungroup() %>%
    roworder(Type)
  
  exportTestValues(pdde_summary_table = summarize_df(a))
  
  datatable(
    a,
    rownames = FALSE,
    filter = 'none',
    options = list(dom = 't'),
    style = "default"
  )
})

# PDDE Guidance -----------------------------------------------------------

output$pdde_guidance_summary <- renderDT({
  req(session$userData$valid_file() == 1)
  
  validate(
    need(
      nrow(session$userData$pdde_main) > 0,
      message = no_data_msg
    )
  )
  
  req(nrow(session$userData$pdde_main) > 0)
  guidance <- session$userData$pdde_main %>%
    fselect(Type, Issue, Guidance) %>%
    roworder(Type, Issue) %>%
    funique()
  
  exportTestValues(pdde_guidance_summary = summarize_df(guidance))
  
  datatable(
    guidance, 
    rownames = FALSE,
    escape = FALSE,
    filter = list(position = 'top', plain = TRUE),
    options = list(dom = 'ltpi'),
    style = "default"
  )
})


# DQ Org Summary -------------------------------------------------------

output$dq_organization_summary_table <- renderDT({
  req(session$userData$valid_file() == 1)

  validate(
    need(
      nrow(session$userData$dq_main) > 0,
      message = no_data_msg
    )
  )

  req(nrow(session$userData$dq_main) > 0)
  
    
  a <- session$userData$dq_main %>%
    fsubset(OrganizationName %in% c(input$orgList)) %>%
    fselect(ProjectName, 
           Type, 
           Issue, 
           PersonalID) %>%
    fgroup_by(ProjectName, 
             Type, 
             Issue) %>%
    fsummarise(Clients = GRPN()) %>%
    roworder(Type, -Clients) %>%
    fselect("Project Name" = ProjectName, 
           Type, 
           Issue, 
           Clients)
  
  exportTestValues(dq_organization_summary_table = summarize_df(a))
  
  datatable(
    a,
    rownames = FALSE,
    filter = list(position = 'top', plain = TRUE),
    options = list(dom = 'ltpi'),
    style = "default"
  )
})

# DQ Org Guidance -------------------------------------------------------

output$dq_org_guidance_summary <- renderDT({
  req(session$userData$valid_file() == 1)

  validate(
    need(
      nrow(session$userData$dq_main) > 0,
      message = no_data_msg
    )
  )
  
  guidance <- session$userData$dq_main %>%
    fsubset(OrganizationName %in% c(input$orgList)) %>%
    fselect(Type, Issue, Guidance) %>%
    roworder(Type, Issue) %>%
    funique()
  
  exportTestValues(dq_org_guidance_summary = summarize_df(guidance))
  
  datatable(
    guidance, 
    rownames = FALSE,
    escape = FALSE,
    filter = list(position = 'top', plain = TRUE),
    options = list(dom = 'ltpi'),
    style = "default"
  )
})

# Download Org DQ Report --------------------------------------------------

output$downloadOrgDQReportButton  <- renderUI({
  req(session$userData$dq_pdde_mirai_complete() == 1)
  req(
    nrow(session$userData$dq_main) > 0 || 
    nrow(session$userData$long_stayers) > 0 || 
    nrow(session$userData$outstanding_referrals) > 0
  )
  req(length(dqDownloadInfo()$orgDQData) > 1)
  downloadButton(outputId = "downloadOrgDQReport",
                 label = "Download")
})

output$downloadOrgDQReport <- downloadHandler(
  filename = reactive(date_stamped_filename(
    str_glue("{input$orgList} Data Quality Report-"))),
  content = function(file) {
    if(length(dqDownloadInfo()$orgDQData) <= 1){
      showNotification("No DQ issues to report for this Organization.")
      
    } else {
      write_xlsx(dqDownloadInfo()$orgDQData, path = file)
      logMetadata(session, paste0("Downloaded Org-level DQ Report",
                                  if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      exportTestValues(orgDQ_download = summarize_df(dqDownloadInfo()$orgDQData))
    }
   
  }
)

# Download System DQ Report -----------------------------------------------
# button
output$downloadSystemDQReportButton  <- renderUI({
  req(session$userData$dq_pdde_mirai_complete() == 1)
  req(
    nrow(session$userData$dq_main) > 0 || 
    nrow(session$userData$long_stayers) > 0 || 
    nrow(session$userData$outstanding_referrals) > 0
  )
  
  downloadButton(outputId = "downloadSystemDQReport",
                 label = "Download") %>% withSpinner()
})

output$downloadSystemDQReport <- downloadHandler(
  filename = date_stamped_filename("Full Data Quality Report-"),
  content = function(file) {
    write_xlsx(dqDownloadInfo()$systemDQData, path = file)
    logMetadata(session, paste0("Downloaded System-level DQ Report",
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    exportTestValues(systemDQ_download = summarize_df(dqDownloadInfo()$systemDQData))
  }
)

# SYSTEM AND ORG-LEVEL DQ TAB PLOTS -----------------------------------------------
# By-org shows organizations containing highest number of HP/errors/warnings
# By-issue shows issues that are the most common of that type

## Get DQ Plot Data Function -----------------
dq_full <- reactive({
  req(session$userData$dq_pdde_mirai_complete() == 1)

  logToConsole(session, "in dq_full")
  
  if(!is.null(session$userData$long_stayers) &&
                     ('DaysSinceLastKnown' %in% names(session$userData$long_stayers))) {
    long_stayers_tc <- tryCatch(
      long_stayers <- session$userData$long_stayers %>%
        fmutate(
          too_many_days = case_match(
            ProjectType,
            es_nbn_project_type ~ input$ESNbNLongStayers,
            out_project_type ~ input$OUTLongStayers,
            sso_project_type ~ input$ServicesOnlyLongStayers,
            other_project_project_type ~ input$OtherLongStayers,
            day_project_type ~ input$DayShelterLongStayers,
            ce_project_type ~ input$CELongStayers
          )
        ) %>% 
        fsubset(DaysSinceLastKnown > too_many_days) %>%
        fselect(vars_we_want) %>%
        fmutate(Type = factor(Type, levels = issue_levels)),
     error = function(e){e}
    )
    
    if(inherits(long_stayers_tc, 'simpleError')){
      logToConsole(session, paste0('Error in long_stayers_tc... colnames: ', paste0(names(session$userData$long_stayers), collapse=',')))

      long_stayers <- data.table()
    } else {
      long_stayers <- long_stayers_tc
    }
  } else {
    long_stayers <- data.table()
  }
  
  if(!is.null(session$userData$outstanding_referrals) > 0) {
    outstanding_referrals <- session$userData$outstanding_referrals %>%
      fsubset(input$CEOutstandingReferrals < Days) %>%
      merge_check_info(checkIDs = 100) %>%
      fselect(vars_we_want) %>%
      fmutate(Type = factor(Type, levels = issue_levels))
  } else {
    outstanding_referrals <- data.table()
  }
  
  bind_rows(
    session$userData$dq_main,
    long_stayers,
    outstanding_referrals
  )
})

get_dq_plot_data <- function(level, issueType, groupVars) {
  # First, need to get long_stayers, if any
  # Unlike other DQ checks, this happens here because it needs to be reactive tp
  # local setting changes
  logToConsole(session, glue::glue("in get_Dq_plot_data, level = {level}, issueType = {issueType}, groupVars = {groupVars}"))
  dq_data <- dq_full()
  
  if(level == "sys") {
    plot_df <- dq_data %>%
      join(
        session$userData$Project0 %>%
          fselect(ProjectID, OrganizationID) %>%
          funique(), 
        on = "ProjectID"
      ) %>%
      fselect(PersonalID,
             OrganizationID,
             OrganizationName,
             HouseholdID,
             Issue,
             Type) %>%
      funique()
  } else {
    plot_df <- dq_data %>%
      fselect(PersonalID,
             ProjectID,
             ProjectName,
             OrganizationName,
             HouseholdID,
             Issue,
             Type) %>%
      funique()
  }
  
  
  # Each project type gets its own Issue text+Guidance etc.
  if(level == "org") {
    plot_df <- plot_df %>% 
      fsubset(OrganizationName %in% input$orgList)
  }
  
  plot_data <- plot_df %>%
    fsubset(Type == issueType) %>% 
    fgroup_by(groupVars) %>%
    fsummarize(countVar = GRPN()) %>%
    fungroup() %>%
    roworder(-countVar)
  
  return(plot_data)
}

## Generate Plots -----------------
# All plots are the same, but with different levels and issue types.
# so we can define a mapping and then loop accordingly


# --- 1. DEFINE YOUR PLOT CONFIGURATIONS IN A TIBBLE ---
# Your original mappings remain useful
dq_issue_type_map <- c(
  "HighPriority" = "High Priority",
  "Warning" = "Warning",
  "Error" = "Error"
)

# Use tidyr::expand_grid to create all possible combinations
plot_configs <- tidyr::expand_grid(
  level = c("sys", "org"),
  issueType = names(dq_issue_type_map),
  byType = c("Org", "Project", "Issue"),
  color = get_brand_color('blue')
) %>%
  # Filter out invalid combinations (e.g., system-level "By Project")
  filter(
    !(level == "sys" & byType == "Project"),
    !(level == "org" & byType == "Org")
  )


# --- 2. CREATE A SINGLE FUNCTION TO GENERATE A PLOT AND ITS UI ---
# This function takes a single row of the config tibble as arguments
renderDQPlot <- function(level, issueType, byType, color) {
  plot_output_id = paste0(level, "DQ", issueType, "By", byType)
  ui_output_id = paste0(plot_output_id, "_ui")
  
  groupVars = fcase(
    level == "sys" & byType == "Org", list(c("OrganizationName", "OrganizationID")),
    level == "sys" & byType == "Issue", list("Issue"),
    level == "org" & byType == "Project", list(c("OrganizationName", "ProjectName", "ProjectID")),
    level == "org" & byType == "Issue", list(c("OrganizationName", "Issue")),
    default = list(NULL)
  )
  
  # Derive the x-axis variable for the plot
  x_group = fcase(
    byType == "Org", "OrganizationName",
    byType == "Project", "ProjectName",
    byType == "Issue", "Issue"
  )
  
    
  # RENDER THE UI (The Plot's Container)
  output[[ui_output_id]] <- renderUI({
    
    cond <- inherits(tryCatch(dq_full(), error = function(e){e}), "simpleError")
    plotOutput(plot_output_id,
               height = if_else(!cond && fnrow(dq_full()) > 0, 400, 50),
               width = ifelse(isTRUE(getOption("shiny.testmode")),
                              "1640",
                              "100%"))
  })
  
  # RENDER THE PLOT (The Plot's Content)
  output[[plot_output_id]] <- renderPlot({
    issueTypeDisplay <- if_else(issueType == "Warning", "warnings", "errors")
    
    validate(
      need(
        !inherits(tryCatch(dq_full(), error = function(e){e}), "simpleError"),
        message = paste0("An error occurred in data quality calculations. This may be an issue in the code. Please reach out to Eva team via GitHub.")
      )
    )

    validate(
      need(
        fnrow(dq_full()) > 0,
        message = paste0("Great job! No ", issueTypeDisplay, " to show.")
      )
    )
    
    plot_data <- get_dq_plot_data(level, dq_issue_type_map[[issueType]], unlist(groupVars))
    
    validate(
      need(
        fnrow(plot_data) > 0,
        message = paste0("Great job! No ", issueTypeDisplay, " to show.")
      )
    )
    
    # Your ggplot code remains identical
    ggplot(head(plot_data, 10L), aes(x = reorder(!!as.name(x_group), countVar), y = countVar)) +
      geom_col(show.legend = FALSE, color = color, fill = color) +
      coord_flip() +
      labs(x = "", y = "Number of Enrollments") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_y_discrete(expand = expansion(mult = c(0, .1))) +
      theme_classic() +
      theme(
        axis.line = element_line(linetype = "blank"),
        axis.text = element_text(size = sys_axis_text_font),
        axis.text.x = element_blank(),
        axis.title = element_text(size = sys_axis_text_font),
        axis.ticks = element_line(linetype = "blank"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      ) +
      geom_text(aes(label = countVar), hjust = -0.5, color = "black", size = sys_chart_text_font)
  },
  alt = case_match(
    plot_output_id,
    "sysDQHighPriorityByIssue" ~ "A bar chart of the top High Priority Errors in the system.",
    "sysDQHighPriorityByOrg" ~ "A bar chart of the top organizations with the most High Priority Errors in the system.",
    "sysDQErrorByIssue" ~ "A bar chart of the top General Errors in the system.",
    "sysDQErrorByOrg" ~ "A bar chart of the top organizations with the most General Errors in the system.",
    "sysDQWarningByIssue" ~ "A bar chart of the top Warnings in the system.",
    "sysDQWarningByOrg" ~ "A bar chart of the top organizations with the most Warnings in the system.",
    "orgDQHighPriorityByIssue" ~ "A bar chart of the top High Priority Errors in the organization.",
    "orgDQHighPriorityProject" ~ "A bar chart of the organization's projects with the most High Priority Errors.",
    "orgDQErrorByIssue" ~ "A bar chart of the top General Errors in the organization.",
    "orgDQErrorByProject" ~ "A bar chart of the organization's projects with the most General Errors.",
    "orgDQWarningByIssue" ~ "A bar chart of the top Warnings in the organization.",
    "orgDQWarningByProject" ~ "A bar chart of the organization's projects with the most Warnings.")
  )
}


# --- 3. ITERATE OVER THE CONFIGS TO CREATE ALL PLOTS ---
# pwalk takes the tibble and applies the function to each row.
# It automatically maps column names to the function's arguments.
purrr::pwalk(plot_configs, renderDQPlot)

getDQReportDataList <- function(
  dqData,
  dqOverlapDetails = NULL,
  bySummaryLevel = NULL,
  dqReferrals = NULL) {
  
  logToConsole(session, "in getDQReportDataList")
  select_list <- c(
    "OrganizationName",
    "ProjectID",
    "ProjectName",
    "ProjectType",
    "Issue",
    "PersonalID",
    "EnrollmentID",
    "HouseholdID",
    "EntryDate"
  )
  
  if(!is.null(dqData) && fnrow(dqData) > 0){
    high_priority <- dqData %>%
      fsubset(Type == "High Priority") %>%
      fmutate(ProjectType = project_type_abb(ProjectType)) %>%
      fselect(select_list)
    
    errors <- dqData %>%
      fsubset(Type == "Error") %>%
      fmutate(ProjectType = project_type_abb(ProjectType)) %>%
      fselect(select_list)
    
    warnings <- dqData %>%
      fsubset(Type == "Warning") %>%
      fmutate(ProjectType = project_type_abb(ProjectType)) %>%
      fselect(select_list)
    
  } else {
    high_priority <- NULL
    errors <- NULL
    warnings <- NULL
  }
 
  if(!is.null(dqReferrrals) && fnrow(dqReferrals) > 0){
    dqReferralDetails <- dqReferrals %>%
      fmutate(ProjectType = project_type_abb(ProjectType)) %>%
      fselect(
        OrganizationName,
        ProjectID,
        ProjectName,
        ProjectType,
        EventID,
        PersonalID,
        EnrollmentID,
        EventDate,
        EventType,
        Days
      )
  }
 
  
  if(!is.null(dqData) && fnrow(dqData) > 0){
    mainsummary <- dqData %>% 
      fselect(Type, Issue, PersonalID) %>%
      # group_by(ProjectName, Type, Issue) %>%
      fgroup_by(Type, Issue) %>%
      fsummarize(Enrollments = GRPN()) %>%
      fungroup() %>%
      fselect(Type, Enrollments, Issue) %>%
      roworder(Type, -Enrollments)
    
    byunitsummary <- dqData %>% 
      fselect(c(bySummaryLevel, "Type", "Issue", "PersonalID")) %>%
      fgroup_by(c(bySummaryLevel, "Type", "Issue")) %>%
      fsummarize(Enrollments = GRPN()) %>%
      fungroup() %>%
      roworderv(cols = c("Type", "Enrollments", bySummaryLevel), decreasing = c(FALSE, TRUE, FALSE))
    
    guidance <- dqData %>%
      fselect(Type, Issue, Guidance) %>%
      funique() %>%
      roworder(Type)
  } else {
    mainsummary <- NULL
    byunitsummary <- NULL
    guidance <- NULL
  }
 
  
  exportDetail <- data.table(
    `Export Field` = c("Export Start", "Export End", "Export Date"),
    Value = c(
      session$userData$meta_HUDCSV_Export_Start,
      session$userData$meta_HUDCSV_Export_End,
      session$userData$meta_HUDCSV_Export_Date
    )
  )

  exportDFList <- list(
    exportDetail = exportDetail %>% nice_names(),
    mainsummary = mainsummary %>% nice_names(),
    byunitsummary = byunitsummary %>% nice_names(),
    guidance = guidance %>% nice_names(),
    high_priority = high_priority %>% nice_names(),
    errors = errors %>% nice_names(),
    warnings = warnings %>% nice_names(),
    overlaps = dqOverlapDetails %>% nice_names(),
    dqReferrals = dqReferralDetails %>% nice_names()
  )
  
  names(exportDFList) <- c(
    "Export Detail",
    paste(
      if_else(bySummaryLevel == "OrganizationName", "System", "Organization"),
      "Summary"
    ),
    paste(
      if_else(
        bySummaryLevel == "OrganizationName",
        "Organization",
        "Project"
      ),
      "Summary"
    ),
    "Guidance",
    "High Priority",
    "Errors",
    "Warnings",
    "Overlap Details",
    "Referral Details"
  )
  
  exportDFList <- exportDFList[lengths(exportDFList) > 0]
  
  return(exportDFList)
}

# list of data frames to include in DQ Org Report
dqDownloadInfo <- reactive({
  logToConsole(session, "in dqDownloadInfo")
  req(session$userData$dq_pdde_mirai_complete() == 1)

  exportTestValues(dq_main = dq_full() %>% nice_names())
  exportTestValues(dq_overlaps = session$userData$overlap_details %>% nice_names())
  
  # org-level data prep (filtering to selected org)
  orgDQData <- dq_full() %>%
    fsubset(OrganizationName %in% input$orgList)
  
  if(!is.null(session$userData$overlap_details)){
    orgDQoverlapDetails <- session$userData$overlap_details %>% 
      fsubset(OrganizationName %in% input$orgList | 
                PreviousOrganizationName %in% input$orgList)
  } else {
    orgDQoverlapDetails <- NULL
  }
  
  
  orgDQReferrals <- session$userData$outstanding_referrals %>%
    fsubset(OrganizationName %in% input$orgList)

  # return a list for reference in downloadHandler
  list(
    orgDQData = 
      getDQReportDataList(orgDQData,
                          orgDQoverlapDetails,
                          "ProjectName",
                          orgDQReferrals
      ),
    
    systemDQData = 
      getDQReportDataList(dq_full(),
                          session$userData$overlap_details,
                          "OrganizationName",
                          session$userData$outstanding_referrals
      )
  )
})


dq_export_date_range_end <- reactive({
  #req(session$userData$dq_pdde_mirai_complete() == 1)
  req(session$userData$valid_file() == 1)
  if(input$dq_export_date_options == 'Date Range'){
    input$dq_export_date_multiple[2]
  } else if(input$dq_export_date_options == 'Single Date'){
    input$dq_export_date_single
  }
})

observeEvent(input$dateRangeCount, {
  req(session$userData$valid_file() == 1)
  #req(session$userData$dq_pdde_mirai_complete() == 1)
  if(input$pageid == 'tabClientCount'){
    
      updateDateRangeInput(session, 'dq_export_date_multiple',
                           start = input$dateRangeCount[1],
                           end = input$dateRangeCount[2])
    
      updateDateInput(session, 'dq_export_date_single',
                      value = input$dateRangeCount[2])
    
  }

}, ignoreInit = TRUE, ignoreNULL = TRUE)

observeEvent({
  c(input$dq_export_date_multiple,
    input$dq_export_date_single)}, {
      
  req(session$userData$valid_file() == 1)
  #req(session$userData$dq_pdde_mirai_complete() == 1)
      
  if(input$pageid == 'tabDQExport'){
    
    if(input$dq_export_date_options == 'Date Range'){
      updateDateRangeInput(session, 'dateRangeCount',
                           start = input$dq_export_date_multiple[1],
                           end = input$dq_export_date_multiple[2])
    } else if(input$dq_export_date_options == 'Single Date'){
      updateDateRangeInput(session, 'dateRangeCount',
                           end = input$dq_export_date_single)
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)



# list of data frames to include in DQ Org Report
get_dqDownloadInfo_export <- function(org_name, value = "org"){
  logToConsole(session, "in dqDownloadInfo_export")
  req(session$userData$dq_pdde_mirai_complete() == 1)
  
  if(inherits(tryCatch(dq_full(), error=function(e){e}), "simpleError")){
    return(NULL)
  }
  
  exportTestValues(dq_main = dq_full() %>% nice_names())
  exportTestValues(dq_overlaps = session$userData$overlap_details %>% nice_names())
  
  if(!is.null(dq_full())){
    orgDQData <- dq_full() %>%
      fsubset(OrganizationName %in% org_name)
  } else {
    orgDQData <- NULL
  }
  
  if(!is.null(session$userData$overlap_details)){
    orgDQoverlapDetails <- session$userData$overlap_details %>% 
      fsubset(OrganizationName %in% org_name | 
                PreviousOrganizationName %in% org_name)
  } else {
    orgDQoverlapDetails <- NULL
  }
  
  if(!is.null(session$userData$outstanding_referrals)){
    orgDQReferrals <- session$userData$outstanding_referrals %>%
      fsubset(OrganizationName %in% org_name)
  } else {
    orgDQReferrals <- NULL
  }
 
  
  # return a list for reference in downloadHandler
  if((is.null(orgDQData) || fnrow(orgDQData) == 0) && 
     (is.null(orgDQOverlapDetails) || fnrow(orgDQoverlapDetails) == 0) && 
     (is.null(orgDQReferrals) || fnrow(orgDQReferrals) == 0) ){
    logToConsole(session, 'no data found for DQ Report generation. exiting from get_dqDownloadInfo')
    return(NULL)
  }
  
  if(value == "org"){
    return(
      getDQReportDataList(orgDQData,
                          orgDQoverlapDetails,
                          "ProjectName",
                          orgDQReferrals
      )
    )
  } else if(value == "system"){
    return(
      getDQReportDataList(dq_full(),
                          session$userData$overlap_details,
                          "OrganizationName",
                          session$userData$outstanding_referrals
      )
    )
  }
  
}

observe({
  
  if('Organization-level (multi-select)' %in% input$dq_export_export_types){
    shinyjs::show(id = 'dq_export_orgList')
  } else {
    shinyjs::hide(id = 'dq_export_orgList')
  }
  
  shinyjs::toggle("dq_export_download_btn", 
                  condition = (session$userData$valid_file() == 1))
                  #condition = (session$userData$dq_pdde_mirai_complete() == 1))
})

observe({
  req(session$userData$valid_file() == 1)
  #req(session$userData$dq_pdde_mirai_complete() == 1)
  ## disable DQ Export button if any of these cases are true
  if(
    ## (1) org-level and system-level are both unchecked
    length(input$dq_export_export_types) == 0 | 
    ## (2) org-level is selected but all orgs are de-selected
     ("Organization-level (multi-select)" %in% input$dq_export_export_types & length(input$dq_export_orgList) == 0) | 
    ## (3) one or both checkboxes are checked, but all report types are unchecked
    (length(input$dq_export_files) == 0 )
  ){
    shinyjs::disable('dq_export_download_btn')
  } else {
    shinyjs::enable('dq_export_download_btn')
  }
})

output$dq_export_report_selections <- renderUI({
  
  ## if an error occurs in dq_full, exclude the DQ report checkbox option
  if(inherits(tryCatch(dq_full(), error = function(e){e}), "simpleError")){
    tree_choices <- create_tree(dq_file_options[1:2,])
  } else {
     tree_choices <- create_tree(dq_file_options)
  }
 
    treeInput(
      inputId = 'dq_export_files',
      label = NULL,
      choices = tree_choices,
      selected = "All Data Quality Reports"
    )
  
})

output$dq_reports_invalid_msg <- renderUI({
  
    if(inherits(tryCatch(dq_full(), error = function(e){e}), "simpleError")){
      return(
        HTML(
        paste0("There is an error in the Data Quality Reports. This may be an issue in the code. Please reach out to Eva team via GitHub. You may still be able to download Project Dashboard and PDDE Reports.")
        )
      )
    } else {
      return(NULL)
    }
})

output$dq_export_download_btn <- downloadHandler(
  filename = date_stamped_filename('Data Quality Exports-', ext='.zip'),
  content = function(file){
    req(session$userData$valid_file() == 1)
    #req(session$userData$dq_pdde_mirai_complete() == 1)
    
    zip_files <- c()
    
    progress <- Progress$new(
      session = session,
      min = 0,
      max = 1
    )
    on.exit(progress$close())
    
    
    ## org-level downloads
    if('Organization-level (multi-select)' %in% input$dq_export_export_types){
      
      if("Data Quality Report" %in% input$dq_export_files){
        req(session$userData$dq_pdde_mirai_complete() == 1)
        req(
          nrow(session$userData$dq_main) > 0 || 
            nrow(session$userData$long_stayers) > 0 || 
            nrow(session$userData$outstanding_referrals) > 0
        )

        progress$set(
          value = 0, 
          message = "Org-Level Data Quality Reports",
          detail = NULL
        )
        
        orgs_to_save <- input$dq_export_orgList
        logMetadata(session, paste0("Attempting to Download Org-Level Data Quality Reports for ",
                                    length(orgs_to_save),' organizations',
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        dq_counter <- 0
        
        for(i in orgs_to_save){
          logToConsole(session, paste0("Checking Org #",which(orgs_to_save == i),", Orgname: ", i))
          dq_export_list <- get_dqDownloadInfo_export(i, value = "org")
          progress$inc(amount = 1 / length(orgs_to_save),
                       detail = paste0('Org ', which(orgs_to_save == i), ' of ', length(orgs_to_save)))          
         
          if(length(dq_export_list) <= 1) {
            logToConsole(session, paste0("Skipping report for Org #",which(orgs_to_save == i),", Orgname: ", i))
            next
          } else {
            dq_counter <- dq_counter + 1 
          }
          org_name_std <- standardize_org_name(i)
          
          path_prefix <- file.path(tempdir(), org_name_std)
          zip_prefix <- str_glue('{org_name_std}/')
          if(!dir.exists(path_prefix)){
            dir.create(path_prefix)
          }
          
          dq_org_filename <- date_stamped_filename(str_glue('{org_name_std} - Data Quality Report-'))
          
          if(length(dq_export_list) > 1){
           
            write_xlsx(dq_export_list, path = file.path(tempdir(), str_glue(zip_prefix, dq_org_filename)))
            zip_files <- c(zip_files, str_glue(zip_prefix, dq_org_filename))
          } else {
            logToConsole(session, paste0("No valid data in DQ report for org #", which(orgs_to_save == i),", so did not write to a file."))
          }
        }
        logMetadata(session, paste0("Downloaded Org-Level Data Quality Reports for ",
                                    dq_counter,' organizations',
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      }
      
      if("PDDE Report" %in% input$dq_export_files){
        
        req(session$userData$valid_file() == 1)
        
        progress$set(
          value = 0, 
          message = "Org-Level PDDE Reports",
          detail = NULL
        )
        
        orgs_to_save <- input$dq_export_orgList
        logMetadata(session, paste0("Attempting to Download Org-Level PDDE Reports for ",
                                    length(orgs_to_save),' organizations',
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        pdde_counter <- 0
        for(i in orgs_to_save){
          
          progress$inc(amount = 1 / length(orgs_to_save),
                       detail = paste0('Org ', which(orgs_to_save == i), ' of ', length(orgs_to_save)))     
          
          summary_df <- session$userData$pdde_main %>% 
            fsubset(OrganizationName == i) %>% 
            fgroup_by(Issue, Type) %>%
            fsummarise(Count = GRPN()) %>%
            fungroup() %>% 
            roworder(Type, Issue)
          
          if(nrow(summary_df) == 0){
            next
          } else {
            pdde_counter <- pdde_counter + 1
          }
          org_name_std <- standardize_org_name(i)
          path_prefix <- file.path(tempdir(), org_name_std)
          zip_prefix <- str_glue('{org_name_std}/')
          if(!dir.exists(path_prefix)){
            dir.create(path_prefix)
          }
          
          pdde_filename <- date_stamped_filename(str_glue("{org_name_std} - PDDE Report-"))
          
          write_xlsx(
            list("Summary" = summary_df,
                 "Data" = session$userData$pdde_main %>% 
                   fsubset(OrganizationName == i) %>% 
                   join(session$userData$Project0 %>% fsubset(OrganizationName == i) %>% fselect(ProjectID, ProjectType), 
                        how = "left", on="ProjectID") %>%
                   roworder(Type, Issue) %>% 
                   nice_names()
            ),
            path = file.path(tempdir(), str_glue(zip_prefix, pdde_filename))
          )
          
          zip_files <- c(zip_files, str_glue(zip_prefix, pdde_filename))
         
        }  
        logMetadata(session, paste0("Downloaded Org-Level PDDE Reports for ",
                                    pdde_counter,' organizations',
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        
      }
      
      if("Project Dashboard Report" %in% input$dq_export_files){
        req(session$userData$valid_file() == 1)
        
        progress$set(
          value = 0, 
          message = "Org-level Project Dashboard Reports",
          detail = NULL
        )
        
        orgs_to_save <- input$dq_export_orgList
        if(input$dq_export_date_options == 'Date Range'){
          logMetadata(session, paste0("Attempting to Download Org-Level Project Dashboard Reports for ",
                                      length(orgs_to_save), ' Organizations',
                                      " with Date Range = [",paste0(input$dq_export_date_multiple, collapse=', '),']',
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        } else {
          logMetadata(session, paste0("Attempting to Download Org-Level Project Dashboard Reports for - ",
                                      length(orgs_to_save), ' Organizations',
                                      " with End Date = ",dq_export_date_range_end(),
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        }
        
        proj_dash_counter <- 0
        
        for(i in orgs_to_save){
          logToConsole(session, paste0("Checking Org #",which(orgs_to_save == i),", Orgname: ", i))
          progress$inc(amount = 1 / length(orgs_to_save),
                       detail = paste0('Org ', which(orgs_to_save == i), ' of ', length(orgs_to_save)))     
          
          validationDF <- client_count_data_df() %>% 
            fsubset(OrganizationName == i)
          
          if(is.null(validationDF) | nrow(validationDF) == 0) {
            logToConsole(session, paste0("Skipping report for Org #",which(orgs_to_save == i),", Orgname: ", i))
            next
          } else {
            proj_dash_counter <- proj_dash_counter + 1
          }
          org_name_std <- standardize_org_name(i)
          logToConsole(session, paste0("standardized org name: ", org_name_std))
          
          path_prefix <- file.path(tempdir(), org_name_std)
          zip_prefix <- str_glue('{org_name_std}/')
          if(!dir.exists(path_prefix)){
            dir.create(path_prefix)
          }
          proj_dash_filename <- date_stamped_filename(str_glue('{org_name_std} - Project Dashboard Report-'))
          pd_org_export <- get_clientcount_download_info(file = file.path(tempdir(), str_glue(zip_prefix, proj_dash_filename)), 
                                        orgList = i, dateRangeEnd = dq_export_date_range_end())
          if(length(pd_org_export) > 1){
            write_xlsx(pd_org_export, path = file.path(tempdir(), str_glue(zip_prefix, proj_dash_filename)))
            zip_files <- c(zip_files, str_glue(zip_prefix, proj_dash_filename))
          } else {
            logToConsole(session, paste0("No valid data in Project Dashboard report for org #", which(orgs_to_save == i),", so did not write to a file."))
          }
          
        }
        if(input$dq_export_date_options == 'Date Range'){
          logMetadata(session, paste0("Downloaded Org-Level Project Dashboard Reports for ",
                                      proj_dash_counter, ' Organizations',
                                      " with Date Range = [",paste0(input$dq_export_date_multiple, collapse=', '),']',
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        } else {
          logMetadata(session, paste0("Downloaded Org-Level Project Dashboard Reports for - ",
                                      proj_dash_counter, ' Organizations',
                                      " with End Date = ",dq_export_date_range_end(),
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        }
      }
     
    }
    
    ## system-level downloads
    if ('System-level' %in% input$dq_export_export_types){
      
      progress$set(
        value = 0, 
        message = "System-level Data Quality Reports",
        detail = NULL
      )
      
      path_prefix <- file.path(tempdir(), 'System-level')
      zip_prefix <- 'System-level/'
      if(!dir.exists(path_prefix)){
        dir.create(path_prefix)
      }
      
      if("Project Dashboard Report" %in% input$dq_export_files){
        req(session$userData$valid_file() == 1)
        
        progress$set(value = 1/3,
                     detail = 'Project Dashboard Report')
        
        proj_dash_filename <- date_stamped_filename('System-level Project Dashboard Report-')
        if(fnrow(client_count_data_df()) > 0){
          pd_sys_export <- get_clientcount_download_info(file = file.path(path_prefix, proj_dash_filename), dateRangeEnd = dq_export_date_range_end())
          if(length(pd_sys_export) > 1){
            write_xlsx(pd_sys_export, path = file.path(path_prefix, proj_dash_filename))
            zip_files <- c(zip_files, str_glue(zip_prefix, proj_dash_filename))
          } else {
            logToConsole(session, paste0("No valid data in System-level Project Dashboard report, so did not write to a file."))
            zip_files <- c(zip_files, paste0(zip_prefix, proj_dash_filename))
            
          }
          
          
          if(input$dq_export_date_options == 'Date Range'){
            logMetadata(session, paste0("Downloaded System-Level Project Dashboard Report with Date Range = [",
                                        paste0(input$dq_export_date_multiple, collapse=', '),']',
                                        if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
          } else {
            logMetadata(session, paste0("Downloaded System-Level Project Dashboard Report with End Date = ",
                                        dq_export_date_range_end(),
                                        if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
          }
        }
       
        
      }
      
      if("PDDE Report" %in% input$dq_export_files){
        
        req(session$userData$valid_file() == 1)
        
        progress$set(value = 2 / 3,
                     detail = 'PDDE Report')
        
        pdde_filename <- date_stamped_filename("System-level PDDE Report-")
        
        summary_df <- session$userData$pdde_main %>% 
          fgroup_by(Issue, Type) %>%
          fsummarise(Count = GRPN()) %>%
          fungroup() %>% 
          roworder(Type, Issue) 
        
        
        if(nrow(summary_df) > 0){
          write_xlsx(
            list("Summary" = summary_df,
                 "Data" = session$userData$pdde_main %>% 
                   join(session$userData$Project0 %>% fselect(ProjectID, ProjectType), how = "left", on = "ProjectID") %>%
                   roworder(Type, Issue) %>% 
                   nice_names()
            ),
            path = file.path(path_prefix,pdde_filename))
          zip_files <- c(zip_files, str_glue(zip_prefix, pdde_filename))
          logMetadata(session, paste0("Downloaded System-level PDDE Report",
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        }
        
      }
      if("Data Quality Report" %in% input$dq_export_files){
        
        req(session$userData$dq_pdde_mirai_complete() == 1)
        req(
          nrow(session$userData$dq_main) > 0 || 
            nrow(session$userData$long_stayers) > 0 || 
            nrow(session$userData$outstanding_referrals) > 0
        )
       
        progress$set(value = 3/3,
                     detail = 'Data Quality Report')
        
        dq_system_filename <- date_stamped_filename("System-level Data Quality Report-")
        if(length(dqDownloadInfo()$systemDQData) > 1) {
          write_xlsx(dqDownloadInfo()$systemDQData, path = file.path(path_prefix,dq_system_filename))
          logMetadata(session, paste0("Downloaded System-level DQ Report",
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
          zip_files <- c(zip_files, str_glue(zip_prefix, dq_system_filename))
        }
      
      } 
      
    }
    
    logToConsole(session, paste0('valid DQ Export files: ', length(zip_files)))  
    
    if(length(zip_files) > 0){
      return(
        zip::zip(file, files = zip_files, root = tempdir())
      )
    } else {
      showNotification('No valid download files available. Please try selecting additional options.', type='error')
      warning('No valid download files available.')
    }
                                                     
  }
)

# # System Data Quality Overview --------------------------------------------
# empty_dq_overview_plot <- function(currPlot) {
#   return(currPlot + 
#     theme(
#       axis.line = element_blank(),
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     ) +
#     annotate(
#       "text",
#       x = 0.5,
#       y = 0.5,
#       label = "No issues!",
#       size = 12,
#       color = "gray50",
#       fontface = "bold"
#     )
#   )
# }

# output$dq_overview_plot <- renderPlot({
#   req(session$userData$valid_file() == 1)
# # browser()
#   detail <- session$userData$dq_main %>%
#     count(Type, name = "Total") %>%
#     mutate(Type = factor(
#       case_when(
#         Type == "High Priority" ~ "High Priority Issues",
#         Type == "Error" ~ "Errors",
#         Type == "Warning" ~ "Warnings"
#       ),
#       levels = c("High Priority Issues",
#                  "Errors",
#                  "Warnings")
#     ))

#   dq_plot_overview <-
#     ggplot(
#       detail,
#       aes(x = Type, y = Total)
#     ) +
#     geom_col(fill = "#71b4cb", alpha = .7, width = .4) +
#     scale_y_continuous(label = comma_format()) +
#     labs(
#       title = "System-wide Data Quality Issues",
#       x = "Data Quality Issue Type",
#       y = "System-wide Issues") +
#     theme_minimal(base_size = 18) +
#     theme(
#       plot.title.position = "plot",
#       title = element_text(colour = "#73655E")
#     ) +
#     geom_text(aes(label = prettyNum(Total, big.mark = ",")),
#                vjust = -.5,
#                color = "gray14")

#   if (nrow(detail) == 0) {
#     dq_plot_overview <- empty_dq_overview_plot(dq_plot_overview)
#   }
#   dq_plot_overview
# })  

# 
#     output$dq_orgs_overview_plot <- renderPlot({
#       req(session$userData$valid_file() == 1)
# # browser()
#       highest_type <- session$userData$dq_main %>%
#         count(Type) %>% 
#         head(1L) %>%
#         mutate(Type = as.character(Type)) %>%
#         pull(Type)
#       
#       highest_type_display <-
#         case_when(
#           highest_type == "High Priority" ~ "High Priority Issues",
#           highest_type == "Error" ~ "Errors",
#           TRUE ~ "Warnings"
#         )
#       
#       detail <- session$userData$dq_main %>%
#         count(OrganizationName, Type, name = "Total") %>%
#         filter(Type == highest_type)
# 
#       dq_plot_overview <-
#         ggplot(
#           detail %>%
#             arrange(desc(Total)) %>%
#             head(5L) %>%
#             mutate(OrganizationName = fct_reorder(OrganizationName, Total)),
#           aes(x = OrganizationName, y = Total)
#         ) +
#         geom_col(fill = "#D5BFE6", alpha = .7)+
#         scale_y_continuous(label = comma_format()) +
#         labs(
#           title = paste("Highest Counts of",
#                         ifelse(is_empty(highest_type_display),
#                                "Issue",
#                                highest_type_display)),
#           x = "Top 5 Organizations",
#           y = ifelse(is_empty(highest_type_display),"Issue",highest_type_display)
#         ) +
#         coord_flip() +
#         theme_minimal(base_size = 18) +
#         theme(
#           plot.title.position = "plot",
#           title = element_text(colour = "#73655E")
#         ) +
#         geom_text(aes(label = prettyNum(Total, big.mark = ",")),
#                   nudge_y = 2,
#                   color = "gray14")
#       
#       if (nrow(detail) == 0) {
#         dq_plot_overview <- empty_dq_overview_plot(dq_plot_overview)
#       }
#       dq_plot_overview
#     })
