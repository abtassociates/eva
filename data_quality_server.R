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
  downloadButton(outputId = "downloadOrgDQReport",
                 label = "Download")
})

output$downloadOrgDQReport <- downloadHandler(
  filename = reactive(date_stamped_filename(
    str_glue("{input$orgList} Data Quality Report-"))),
  content = function(file) {
    write_xlsx(dqDownloadInfo()$orgDQData, path = file)
    logMetadata(session, paste0("Downloaded Org-level DQ Report",
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    exportTestValues(orgDQ_download = summarize_df(dqDownloadInfo()$orgDQData))
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
  long_stayers <- if(!is.null(session$userData$long_stayers) & 
                     ('DaysSinceLastKnown' %in% names(session$userData$long_stayers))) {
    session$userData$long_stayers %>%
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
      fmutate(Type = factor(Type, levels = issue_levels))
  } else data.table()
  
  outstanding_referrals <- if(!is.null(session$userData$outstanding_referrals) > 0) {
    session$userData$outstanding_referrals %>%
      fsubset(input$CEOutstandingReferrals < Days) %>%
      merge_check_info(checkIDs = 100) %>%
      fselect(vars_we_want) %>%
      fmutate(Type = factor(Type, levels = issue_levels))
  } else data.table()

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
    req(nrow(dq_full()) > 0)
    
    plotOutput(plot_output_id,
               height = if_else(fnrow(dq_full()) == 0, 50, 400),
               width = ifelse(isTRUE(getOption("shiny.testmode")),
                              "1640",
                              "100%"))
  })
  
  # RENDER THE PLOT (The Plot's Content)
  output[[plot_output_id]] <- renderPlot({
    req(fnrow(dq_full()) > 0)

    plot_data <- get_dq_plot_data(level, dq_issue_type_map[[issueType]], unlist(groupVars))
    
    issueTypeDisplay <- if_else(issueType == "Warning", "warnings", "errors")
    
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
  
  exportDFList <- exportDFList[sapply(exportDFList, 
                                      function(x) dim(x)[1]) > 0]
  
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
  
  orgDQoverlapDetails <- session$userData$overlap_details %>% 
    fsubset(OrganizationName %in% input$orgList | 
             PreviousOrganizationName %in% input$orgList)
  
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
  req(session$userData$dq_pdde_mirai_complete() == 1)
  
  if(input$dq_export_date_options == 'Date Range'){
    input$dq_export_date_multiple[2]
  } else if(input$dq_export_date_options == 'Single Date'){
    input$dq_export_date_single
  }
})

observeEvent(input$dateRangeCount, {
  req(session$userData$dq_pdde_mirai_complete() == 1)
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
      
  req(session$userData$dq_pdde_mirai_complete() == 1)
      
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

observe({

  if('Organization-level (multi-select)' %in% input$dq_export_export_types){
    shinyjs::show(id = 'dq_export_orgList')
  } else {
    shinyjs::hide(id = 'dq_export_orgList')
  }
  
  shinyjs::toggle("dq_export_download_btn", condition = (session$userData$dq_pdde_mirai_complete() == 1))
})

# list of data frames to include in DQ Org Report
get_dqDownloadInfo_export <- function(org_name, value = "org"){
  logToConsole(session, "in dqDownloadInfo_export")
  req(session$userData$dq_pdde_mirai_complete() == 1)
  
  exportTestValues(dq_main = dq_full() %>% nice_names())
  exportTestValues(dq_overlaps = session$userData$overlap_details %>% nice_names())
  
  orgDQData <- dq_full() %>%
    fsubset(OrganizationName %in% org_name)
  
  orgDQoverlapDetails <- session$userData$overlap_details %>% 
    fsubset(OrganizationName %in% org_name | 
             PreviousOrganizationName %in% org_name)
  
  orgDQReferrals <- session$userData$outstanding_referrals %>%
    fsubset(OrganizationName %in% org_name)
  
  # return a list for reference in downloadHandler
  
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
  
  req(session$userData$dq_pdde_mirai_complete() == 1)
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

output$dq_export_download_btn <- downloadHandler(
  filename = date_stamped_filename('Data Quality Exports-', ext='.zip'),
  content = function(file){
    
    req(session$userData$dq_pdde_mirai_complete() == 1)
    
    zip_files <- c()
    
    ## org-level downloads
    if('Organization-level (multi-select)' %in% input$dq_export_export_types){
      
      if("Data Quality Report" %in% input$dq_export_files){
        req(session$userData$dq_pdde_mirai_complete() == 1)
        req(
          nrow(session$userData$dq_main) > 0 || 
            nrow(session$userData$long_stayers) > 0 || 
            nrow(session$userData$outstanding_referrals) > 0
        )

        orgs_to_save <- input$dq_export_orgList
        logMetadata(session, paste0("Downloaded Org-Level Data Quality Reports for ",
                                    length(orgs_to_save),' organizations',
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        
        for(i in orgs_to_save){
          
          dq_export_list <- get_dqDownloadInfo_export(i, value = "org")
          
          if(length(dq_export_list) <= 1) next
          
          org_name_std <- standardize_org_name(i)
          
          path_prefix <- file.path(tempdir(), org_name_std)
          zip_prefix <- str_glue('{org_name_std}/')
          if(!dir.exists(path_prefix)){
            dir.create(path_prefix)
          }
          
          dq_org_filename <- date_stamped_filename(str_glue('{org_name_std} - Data Quality Report-'))
          write_xlsx(dq_export_list, 
                     path = file.path(tempdir(), str_glue(zip_prefix, dq_org_filename))
                     )
          zip_files <- c(zip_files, str_glue(zip_prefix, dq_org_filename))
          
        }
    
      }
      
      if("PDDE Report" %in% input$dq_export_files){
        
        req(session$userData$valid_file() == 1)
        
        orgs_to_save <- input$dq_export_orgList
        logMetadata(session, paste0("Downloaded Org-Level PDDE Reports for ",
                                    length(orgs_to_save),' organizations',
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        for(i in orgs_to_save){
          
          summary_df <- session$userData$pdde_main %>% 
            fsubset(OrganizationName == i) %>% 
            fgroup_by(Issue, Type) %>%
            fsummarise(Count = GRPN()) %>%
            fungroup() %>% 
            roworder(Type, Issue)
          
          if(nrow(summary_df) == 0) next
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
        
      }
      
      if("Project Dashboard Report" %in% input$dq_export_files){
        req(session$userData$valid_file() == 1)
        
        orgs_to_save <- input$dq_export_orgList
        if(input$dq_export_date_options == 'Date Range'){
          logMetadata(session, paste0("Downloaded Org-Level Project Dashboard Reports for ",
                                      length(orgs_to_save), ' Organizations',
                                      " with Date Range = [",paste0(input$dq_export_date_multiple, collapse=', '),']',
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        } else {
          logMetadata(session, paste0("Downloaded Org-Level Project Dashboard Reports for - ",
                                      length(orgs_to_save), ' Organizations',
                                      " with End Date = ",dq_export_date_range_end(),
                                      if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        }
        
        for(i in orgs_to_save){
          
          validationDF <- client_count_data_df() %>% 
            fsubset(OrganizationName == i)
          
          if(nrow(validationDF) == 0) next
          org_name_std <- standardize_org_name(i)
          path_prefix <- file.path(tempdir(), org_name_std)
          zip_prefix <- str_glue('{org_name_std}/')
          if(!dir.exists(path_prefix)){
            dir.create(path_prefix)
          }
          proj_dash_filename <- date_stamped_filename(str_glue('{org_name_std} - Project Dashboard Report-'))
          get_clientcount_download_info(file = file.path(tempdir(), str_glue(zip_prefix, proj_dash_filename)), 
                                        orgList = i, dateRangeEnd = dq_export_date_range_end())
          zip_files <- c(zip_files, str_glue(zip_prefix, proj_dash_filename))
          
          
        }
        
      }
     
    }
    
    ## system-level downloads
    if ('System-level' %in% input$dq_export_export_types){
      
      path_prefix <- file.path(tempdir(), 'System-level')
      zip_prefix <- 'System-level/'
      if(!dir.exists(path_prefix)){
        dir.create(path_prefix)
      }
      
      if("Project Dashboard Report" %in% input$dq_export_files){
        req(session$userData$valid_file() == 1)
        
        proj_dash_filename <- date_stamped_filename('System-level Project Dashboard Report-')
        
        get_clientcount_download_info(file = file.path(path_prefix, proj_dash_filename), dateRangeEnd = dq_export_date_range_end())
        
        zip_files <- c(zip_files, paste0(zip_prefix, proj_dash_filename))
        
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
      
      if("PDDE Report" %in% input$dq_export_files){
        
        req(session$userData$valid_file() == 1)
        
        pdde_filename <- date_stamped_filename("System-level PDDE Report-")
        
        summary_df <- session$userData$pdde_main %>% 
          fgroup_by(Issue, Type) %>%
          fsummarise(Count = GRPN()) %>%
          fungroup() %>% 
          roworder(Type, Issue) 
        
        if(nrow(summary_df) == 0) break

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
      if("Data Quality Report" %in% input$dq_export_files){
        
        req(session$userData$dq_pdde_mirai_complete() == 1)
        req(
          nrow(session$userData$dq_main) > 0 || 
            nrow(session$userData$long_stayers) > 0 || 
            nrow(session$userData$outstanding_referrals) > 0
        )
       
        dq_system_filename <- date_stamped_filename("System-level Data Quality Report-")
        
        write_xlsx(dqDownloadInfo()$systemDQData, path = file.path(path_prefix,dq_system_filename))
        logMetadata(session, paste0("Downloaded System-level DQ Report",
                                    if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
        zip_files <- c(zip_files, str_glue(zip_prefix, dq_system_filename))
      } 
      
    }
    
    
    
   
      
    
    zip::zip(file, files = zip_files, root = tempdir())
                                                     
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

## Get Bed/Unit Inventory Data Function ------------------------------------------
bed_unit_inv <- reactive({
  req(session$userData$dq_pdde_mirai_complete() == 1)
  
  logToConsole(session, "bed_unit_inv")
  # get the last date in activeInventory
  lastday <- as.Date(fmax(HMIS_projects_w_active_inv$ProjectHMISActiveParticipationEnd))
  y_last <- year(lastday)
  # the quarters end on the last wednedsay of january, april, july & october
  # create a function to get the exact date given a month and year
  last_wednesday <- function(year, month) {
    # Get the last day of the month
    last_day <- ceiling_date(ymd(paste(year, month, "01", sep = "-")), "month") - days(1)
    # Find the weekday of the last day (1 = Sunday, 7 = Saturday)
    weekday <- wday(last_day)
    # Calculate the difference to the last Wednesday (4 = Wednesday)
    diff <- ifelse(weekday >= 4, weekday - 4, weekday + 3)
    # Subtract the difference to get the last Wednesday
    last_day - days(diff)
  }
  
  q1_PIT <- as.Date(fifelse( last_wednesday(y_last, 1) <= lastday, # if lastday is after the current year's 1st quarter,
                    last_wednesday(y_last,1), # use last wedensday of this january
                    last_wednesday(y_last-1,1))) # else use last wednesday of last january
  q2_PIT <- as.Date(fifelse( last_wednesday(y_last, 4) <= lastday, # if lastday is after the current year's 2nd quarter,
                    last_wednesday(y_last,4), # use last wedensday of this april
                    last_wednesday(y_last-1,4))) # else use last wednesday of last april
  q3_PIT <- as.Date(fifelse( last_wednesday(y_last, 7) <= lastday, # if lastday is after the current year's 3rd quarter,
                    last_wednesday(y_last,7), # use last wedensday of this july
                    last_wednesday(y_last-1,7))) # else use last wednesday of last july
  q4_PIT <- as.Date(fifelse( last_wednesday(y_last, 10) <= lastday, # if lastday is after the current year's 4th quarter,
                    last_wednesday(y_last,10), # use last wedensday of this october
                    last_wednesday(y_last-1,10))) # else use last wednesday of last october
  
  # these will probably be the 'default' values in some date selection 
  quarters <- c(q1_PIT, q2_PIT, q3_PIT, q4_PIT) # create vector of quarterly dates
  report_start <- min(quarters) 
  report_end <- report_start + years(1) - days(1)
  
  
  # with these values create functions to count Beds & Units and Served (Enrollments) & HH_Served (HOH Enrollments)
  # use 365 / length of pit_dates to calculate the 'report length' and calculate nightly averages
  count_Beds_Units <- function(pit_dates, nightly_avg = TRUE){
    pit_dates <- data.frame("PIT" = pit_dates) %>% fmutate(temp=1)
    
    Bed_Unit_Count <- HMIS_project_active_inventories %>% 
      fselect(ProjectID, BedInventory, UnitInventory, InventoryStartDate, InventoryEndDate) %>%
      fmutate(temp = 1) %>%
      join( # expand rows for each PIT date
        pit_dates, 
        on="temp", 
        multiple=T
      ) %>%
      fmutate(
        activeInv = InventoryStartDate <= as.Date(PIT) & (is.na(InventoryEndDate) | InventoryEndDate > as.Date(PIT))
      ) %>%
      fgroup_by(PIT, ProjectID) %>%
      fsummarize(
        PIT_Beds = fsum(fifelse(activeInv,BedInventory,0)),
        PIT_Units = fsum(fifelse(activeInv,UnitInventory,0))
      ) %>% fungroup()
    #For each relevant project, count the number of beds and units for the project available for occupancy on each of the 4 PIT Dates.
    #For inventory to be considered "active" on a PIT Date it must meet the following logic: InventoryStartDate <= [PIT Date] and InventoryEndDate > [PIT Date] or NULL
    if(nightly_avg){
      # calculate length of range 
      report_length <- 365 / nrow(pit_dates)
      Bed_Unit_Count <- Bed_Unit_Count %>% # use it to calculate average nightly beds/units
        fmutate(Avg_Nightly_Beds = PIT_Beds / report_length ,
                Avg_Nightly_Units = PIT_Units / report_length )
    }
    return(Bed_Unit_Count)
  }
  count_Enrollments <-function(pit_dates, nightly_avg = TRUE){
    pit_dates <- data.frame("PIT" = pit_dates) %>% fmutate(temp=1)
    #For each relevant project and using the EnrollmentAdjust data frame, count the number of people "served in a bed" on each of the 4 PIT Dates.
    #For an enrollment to be considered "active" on a PIT Date it must meet the following logic: EntryDate <= [PIT Date] and ExitAdjust > [PIT Date] or is NULL
    #Exclude any ES - NbN enrollments where there is no Bed Night record on [PIT Date]
    #Exclude any permanent housing enrollments where MoveInDateAdjust < [PIT Date]
    services_qPIT <- Services %>%
      fselect(EnrollmentID, DateProvided)  %>% 
      join(EnrollmentAdjust %>% fselect(EnrollmentID, ProjectID), on = "EnrollmentID", how = 'full')  %>% 
      fmutate(temp = 1) %>%
      join( # expand rows for each PIT date
        pit_dates %>% fmutate(temp=1), 
        on="temp", 
        multiple=T
      ) %>%
      fmutate(bn_PIT = as.Date(DateProvided) == as.Date(PIT)) %>%  
      fmutate(bn_PIT = fifelse(is.na(bn_PIT),FALSE,bn_PIT)) %>%  
      fgroup_by(EnrollmentID, PIT) %>% # For each enrollment & PIT Date,
      fsummarise( # flag if Enrollment has any Service records where DateProvided == PIT
        has_bn_PIT = any(bn_PIT, na.rm=TRUE)
      )  %>% fungroup()
    Bed_Unit_Util <- EnrollmentAdjust %>%
      join(services_qPIT, on = "EnrollmentID", how = "left", multiple = T) %>%
      fmutate(# Enrollment Active
        activeEnroll = EntryDate <= as.Date(PIT) & (is.na(ExitAdjust) | ExitAdjust > as.Date(PIT)),
        eligProjNBN = ProjectType != es_nbn_project_type | any(has_bn_PIT) ,
        eligProjPerm = !(ProjectType %in% c(3,9,10,13)) | fifelse(is.na(MoveInDateAdjust), 
                                                                  FALSE, # if MoveInDateAdjust is missing, use FALSE to count zero days
                                                                  MoveInDateAdjust >= as.Date(PIT))
      ) %>% 
      fmutate(Served = fifelse(activeEnroll & eligProjNBN & eligProjPerm, 1, 0), # flag active & eligible enrollments
              HH_Served = fifelse(activeEnroll & eligProjNBN & eligProjPerm & RelationshipToHoH==1, # count households by just flagging active/elig enrollments that are head of household
                                 1, 0)) %>%
      fgroup_by(ProjectID, PIT) %>%
      fsummarise(
        PIT_Served = fsum(Served),
        PIT_HH_Served = fsum(HH_Served))%>%
      fungroup()  %>% fsubset(!is.na(PIT_Served))
    
    if(nightly_avg){
      # calculate length of range 
      report_length <- 365 / nrow(pit_dates)
      Bed_Unit_Util <- Bed_Unit_Util %>% # use it to calculate average nightly beds/units
        fmutate(Avg_Nightly_Served = PIT_Served / report_length ,
                Avg_Nightly_HH_Served = PIT_HH_Served / report_length )
    }
    return(Bed_Unit_Util)
  } 
  
  # create versions that sum counts over a range of dates
  # use difference in dates to calculate the 'report length' and calculate nightly averages
  count_Beds_Units_rng <- function(range_start,range_end){
    
    Bed_Unit_Count <- count_Beds_Units(seq(from = report_start, to = report_end, by = 1), nightly_avg = FALSE) 
    
    Bed_Unit_Count <- Bed_Unit_Count %>%  fgroup_by(ProjectID) %>%
      fsummarize(
        Total_Beds = fsum(PIT_Beds), # sum active beds over all days in range
        Total_Units = fsum(PIT_Units) # sum active units over all days in range
      )
    # calculate length of range 
    report_length <- as.numeric(range_end - range_start)
    Bed_Unit_Count <- Bed_Unit_Count %>% # use it to calculate average nightly beds/units
      fmutate(Avg_Nightly_Beds = Total_Beds / report_length ,
              Avg_Nightly_Units = Total_Units / report_length )

    return(Bed_Unit_Count)
  }
  count_Enrollments_rng <-function(range_start,range_end){
    
    Bed_Unit_Util <- count_Enrollments(seq(from = report_start, to = report_end, by = 1), nightly_avg = FALSE) 
    Bed_Unit_Util <- Bed_Unit_Util %>% fgroup_by(ProjectID) %>%
      fsummarise(
        Total_Served = fsum(PIT_Served), # sum enrollments over all days in range
        Total_HH_Served = fsum(PIT_HH_Served) # sum HOH enrollments over all days in range
      )
    
    # calculate length of range 
    report_length <- as.numeric(range_end - range_start)
    Bed_Unit_Util <- Bed_Unit_Util %>% # use it to calculate average served beds/units
      fmutate(Avg_Nightly_Served = Total_Served / report_length ,
              Avg_Nightly_HH_Served = Total_HH_Served / report_length )
    
    return(Bed_Unit_Util)
  } 
  
  # full join the results of passing quarters through each function
  project_level_util <- count_Beds_Units(quarters) %>%
    join(count_Enrollments(quarters), how = "left")
  
  # calculate project level quarterly utilization
   project_level_util <- project_level_util %>%
     fmutate(Bed_Utilization = PIT_Served / PIT_Beds,
            Unit_Utilization = PIT_HH_Served / PIT_Units,
            Avg_Nightly_Bed_Util = Avg_Nightly_Served / Avg_Nightly_Beds,
            Avg_Nightly_Unit_Util = Avg_Nightly_HH_Served / Avg_Nightly_Units)
   
   # get projects with at least one  active period
   any_active <- HMIS_projects_w_active_inv %>% filter(HMISParticipationType == 1)
   any_active <- unique(any_active$ProjectID)
   project_level_util <- project_level_util %>% filter(ProjectID %in% any_active)
   
  # calculate system level totals
  system_level_util <- project_level_util %>% fungroup %>%
    fgroup_by(PIT) %>% # for each PIT Date,
    fsummarise(  # sum all projects 
      Total_Beds = fsum(PIT_Beds),
      Total_Units = fsum(PIT_Units),
      Total_Served = fsum(PIT_Served),
      Total_HH_Served = fsum(PIT_HH_Served),
      Avg_Nightly_Beds = fsum(Avg_Nightly_Beds),
      Avg_Nightly_Units = fsum(Avg_Nightly_Units),
      Avg_Nightly_Served = fsum(Avg_Nightly_Served),
      Avg_Nightly_HH_Served = fsum(Avg_Nightly_HH_Served)
    ) %>% fungroup()
  
  # calculate system level quarterly utilization
  system_level_util <- system_level_util %>%
    fmutate(Bed_Utilization = Total_Served / Total_Beds,
           Unit_Utilization = Total_HH_Served / Total_Units,
           Avg_Nightly_Bed_Util = Avg_Nightly_Served / Avg_Nightly_Beds,
           Avg_Nightly_Unit_Util = Avg_Nightly_HH_Served / Avg_Nightly_Units)
  
  
})
