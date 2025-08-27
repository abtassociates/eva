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
      group_by(Issue, Type) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    write_xlsx(
      list("Summary" = summary_df,
           "Data" = session$userData$pdde_main %>% 
             left_join(session$userData$Project0 %>% select(ProjectID, ProjectType), by="ProjectID") %>%
             nice_names()
      ),
      path = file)
    
    logMetadata(session, paste0("Downloaded PDDE Report",
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    
    exportTestValues(pdde_download_summary = summary_df)
    exportTestValues(pdde_main = session$userData$pdde_main %>% nice_names())
  }
)

# summary table
output$pdde_summary_table <- renderDT({
  req(session$userData$valid_file() == 1)
  req(nrow(session$userData$pdde_main) > 0)

  a <- session$userData$pdde_main %>%
    group_by(Issue, Type) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    arrange(Type)
  
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
    select(Type, Issue, Guidance) %>%
    arrange(Type, Issue) %>%
    unique()
  
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
    filter(OrganizationName %in% c(input$orgList)) %>%
    select(ProjectName, 
           Type, 
           Issue, 
           PersonalID) %>%
    group_by(ProjectName, 
             Type, 
             Issue) %>%
    summarise(Clients = n()) %>%
    arrange(Type, desc(Clients)) %>%
    select("Project Name" = ProjectName, 
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
    fmutate(Type = factor(Type, levels = c("High Priority",
                                          "Error",
                                          "Warning"))) %>%
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
  long_stayers <- if(!is.null(session$userData$long_stayers) > 0) {
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
      left_join(session$userData$Project0 %>%
                  select(ProjectID, OrganizationID) %>%
                  unique(), by = "ProjectID") %>%
      select(PersonalID,
             OrganizationID,
             OrganizationName,
             HouseholdID,
             Issue,
             Type) %>%
      unique()
  } else {
    plot_df <- dq_data %>%
      select(PersonalID,
             ProjectID,
             ProjectName,
             OrganizationName,
             HouseholdID,
             Issue,
             Type) %>%
      unique()
  }
  
  
  # Each project type gets its own Issue text+Guidance etc.
  if(level == "org") {
    plot_df <- plot_df %>% 
      filter(OrganizationName %in% c(input$orgList))
  }
  
  plot_data <- plot_df %>%
    filter(Type == issueType) %>% 
    group_by(across(all_of(groupVars))) %>%
    summarise(countVar = n()) %>%
    ungroup() %>%
    arrange(desc(countVar))
  
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
  color = "#71B4CB"
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
  
  groupVars = case_when(
    level == "sys" & byType == "Org" ~ list(c("OrganizationName", "OrganizationID")),
    level == "sys" & byType == "Issue" ~ list("Issue"),
    level == "org" & byType == "Project" ~ list(c("OrganizationName", "ProjectName", "ProjectID")),
    level == "org" & byType == "Issue" ~ list(c("OrganizationName", "Issue")),
    TRUE ~ list(NULL)
  )
  
  # Derive the x-axis variable for the plot
  x_group = case_when(
    byType == "Org" ~ "OrganizationName",
    byType == "Project" ~ "ProjectName",
    byType == "Issue" ~ "Issue"
  )
  
  # RENDER THE UI (The Plot's Container)
  output[[ui_output_id]] <- renderUI({
    req(nrow(dq_full()) > 0)
    
    plotOutput(plot_output_id,
               height = if_else(nrow(dq_full()) == 0, 50, 400),
               width = ifelse(isTRUE(getOption("shiny.testmode")),
                              "1640",
                              "100%"))
  })
  
  # RENDER THE PLOT (The Plot's Content)
  output[[plot_output_id]] <- renderPlot({
    req(nrow(dq_full()) > 0)

    plot_data <- get_dq_plot_data(level, dq_issue_type_map[[issueType]], unlist(groupVars))
    
    issueTypeDisplay <- if_else(issueType == "Warning", "warnings", "errors")
    
    validate(
      need(
        nrow(plot_data) > 0,
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
    filter(Type == "High Priority") %>%
    mutate(ProjectType = project_type_abb(ProjectType)) %>%
    select(all_of(select_list))
  
  errors <- dqData %>%
    filter(Type == "Error") %>%
    mutate(ProjectType = project_type_abb(ProjectType)) %>%
    select(all_of(select_list))
  
  warnings <- dqData %>%
    filter(Type == "Warning") %>%
    mutate(ProjectType = project_type_abb(ProjectType)) %>%
    select(all_of(select_list))
  
  dqReferralDetails <- dqReferrals %>%
    mutate(ProjectType = project_type_abb(ProjectType)) %>%
    select(
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
    select(Type, Issue, PersonalID) %>%
    # group_by(ProjectName, Type, Issue) %>%
    group_by(Type, Issue) %>%
    summarise(Enrollments = n()) %>%
    ungroup() %>%
    select(Type, Enrollments, Issue) %>%
    arrange(Type, desc(Enrollments))

  bySummaryLevel2 <- rlang::sym(bySummaryLevel)
  byunitsummary <- dqData %>% 
    select(!!bySummaryLevel2, Type, Issue, PersonalID) %>%
    group_by(!!bySummaryLevel2, Type, Issue) %>%
    summarise(Enrollments = n()) %>%
    ungroup() %>%
    select(!!bySummaryLevel2, Type, Enrollments, Issue) %>%
    arrange(Type, desc(Enrollments), !!bySummaryLevel2)
  
  
  guidance <- dqData %>%
    select(Type, Issue, Guidance) %>%
    unique() %>%
    mutate(Type = factor(Type, levels = c("High Priority", "Error", "Warning"))) %>%
    arrange(Type)
  
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
    filter(OrganizationName %in% c(input$orgList))
  
  orgDQoverlapDetails <- session$userData$overlap_details %>% 
    filter(OrganizationName %in% c(input$orgList) | 
             PreviousOrganizationName %in% c(input$orgList))
  
  orgDQReferrals <- 
    session$userData$outstanding_referrals %>%
    filter(OrganizationName %in% c(input$orgList))

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

output$dq_export_date_selection <- renderUI({
  req(session$userData$dq_pdde_mirai_complete() == 1)
  
  if(input$dq_export_date_options == 'Date Range'){
    dateRangeInput(
      inputId = 'dq_export_date_multiple',
      label = NULL,
      start = session$userData$meta_HUDCSV_Export_Start,
      end = session$userData$meta_HUDCSV_Export_End,
      min = session$userData$meta_HUDCSV_Export_Start,
      max = session$userData$meta_HUDCSV_Export_End
    )
  } else if(input$dq_export_date_options == 'Single Date'){
    dateInput(
      inputId = 'dq_export_date_single',
      label = NULL,
      value = session$userData$meta_HUDCSV_Export_Start,
      min = session$userData$meta_HUDCSV_Export_Start,
      max = session$userData$meta_HUDCSV_Export_End
      
    )
  }
 
})

observe({

  if('Organization-level (multi-select)' %in% input$dq_export_export_types){
    shinyjs::show(id = 'dq_export_orgList')
  } else {
    shinyjs::hide(id = 'dq_export_orgList')
  }
})

output$dq_export_download_btn <- downloadHandler(
  filename = date_stamped_filename('Data Quality Exports', ext='.zip'),
  content = function(file){
                                                    
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
