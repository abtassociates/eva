# PDDE Checker ------------------------------------------------------------
# PDDE Download Button ----------------------------------------------------
output$downloadPDDEReportButton  <- renderUI({
  req(session$userData$valid_file() == 1)
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
    options = list(dom = 't')
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
    filter = 'top',
    options = list(dom = 'ltpi')
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
    filter = 'top',
    options = list(dom = 'ltpi')
  )
})

# DQ Org Guidance -------------------------------------------------------

output$dq_org_guidance_summary <- renderDT({
  req(session$userData$valid_file() == 1)
  guidance <- session$userData$dq_main %>%
    filter(OrganizationName %in% c(input$orgList)) %>%
    select(Type, Issue, Guidance) %>%
    mutate(Type = factor(Type, levels = c("High Priority",
                                          "Error",
                                          "Warning"))) %>%
    arrange(Type, Issue) %>%
    unique()
  
  exportTestValues(dq_org_guidance_summary = summarize_df(guidance))
  
  datatable(
    guidance, 
    rownames = FALSE,
    escape = FALSE,
    filter = 'top',
    options = list(dom = 'ltpi')
  )
})

# Download Org DQ Report --------------------------------------------------

output$downloadOrgDQReportButton  <- renderUI({
  req(session$userData$valid_file() == 1)
  req(length(dqDownloadInfo()$orgDQData) > 0)
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
  req(session$userData$valid_file() == 1)
  req(length(dqDownloadInfo()$systemDQData) > 0)
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

# SYSTEM-LEVEL DQ TAB PLOTS -----------------------------------------------
# By-org shows organizations containing highest number of HP/errors/warnings
# By-issue shows issues that are the most common of that type
output$systemDQHighPriorityErrorsByOrg_ui <- renderUI({
  renderDQPlot("sys", "High Priority", "Org", "#71B4CB")
})

output$systemDQHighPriorityErrorsByIssue_ui <- renderUI({
  renderDQPlot("sys", "High Priority", "Issue", "#71B4CB")
})

output$systemDQErrorsByOrg_ui <- renderUI({
  renderDQPlot("sys", "Error", "Org", "#71B4CB")
})

output$systemDQErrorsByIssue_ui <- renderUI({
  renderDQPlot("sys", "Error", "Issue", "#71B4CB")
})

output$systemDQWarningsByOrg_ui <- renderUI({
  renderDQPlot("sys", "Warning", "Org", "#71B4CB")
})

output$systemDQWarningsByIssue_ui <- renderUI({
  renderDQPlot("sys", "Warning", "Issue", "#71B4CB")
})


# ORG-LEVEL TAB PLOTS -----------------------------------------------------
# By-project shows projects, within the selected org, containing highest 
# number of HP errors/errors/warnings
# By-issue shows issues, within the selected org, that are the most common 
# of that type (HP errors/errors/warnings)
output$orgDQHighPriorityErrorsByProject_ui <- renderUI({
  renderDQPlot("org", "High Priority", "Project", "#71B4CB")
})

output$orgDQHighPriorityErrorByIssue_ui <- renderUI({
  renderDQPlot("org", "High Priority", "Issue", "#71B4CB")
})

output$orgDQErrorsByProject_ui <- renderUI({
  renderDQPlot("org", "Error", "Project", "#71B4CB")
})

output$orgDQErrorByIssue_ui <- renderUI({
  renderDQPlot("org", "Error", "Issue", "#71B4CB")
})

output$orgDQWarningsByProject_ui <- renderUI({
  renderDQPlot("org", "Warning", "Project", "#71B4CB")
})

output$orgDQWarningsByIssue_ui <- renderUI({
  renderDQPlot("org", "Warning", "Issue", "#71B4CB")
})

output$orgDQWarningsByIssue_ui <- renderUI({
  renderDQPlot("org", "Warning", "Issue", "#71B4CB")
})
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

getDQReportDataList <-
  function(dqData,
           dqOverlapDetails = NULL,
           bySummaryLevel = NULL,
           dqReferrals = NULL) {
    
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
      filter(Issue == "Days Referral Active Exceeds Local Settings") %>%
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
    
    exportDetail <- data.frame(c("Export Start", "Export End", "Export Date"),
                               c(session$userData$meta_HUDCSV_Export_Start,
                                 session$userData$meta_HUDCSV_Export_End,
                                 session$userData$meta_HUDCSV_Export_Date))
    
    colnames(exportDetail) <- c("Export Field", "Value")
    
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

renderDQPlot <- function(level, issueType, group, color) {
  req(nrow(session$userData$dq_main) > 0 & session$userData$valid_file() == 1)
  # groupVars is the variable(s) used to summarise/count rows
  # x_group is the x variable used to in the ggplot reordering
  if(group == "Org") {
    groupVars <- c("OrganizationName", "OrganizationID")
    x_group <- "OrganizationName"
  } else if(group == "Project") {
    groupVars <- c("OrganizationName", "ProjectName", "ProjectID")
    x_group <- "ProjectName"
  } else if(group == "Issue" && level == "org") {
    groupVars <- c("OrganizationName", "Issue")
    x_group <- "Issue"
  } else if(group == "Issue" && level == "sys") {
    groupVars <- "Issue"
    x_group <- "Issue"
  } 

  
  # Plots for System-Level DQ Tab -------------------------------------------
  # determine which data.frame we start with
  if(level == "sys") {
    plot_df <- session$userData$dq_main %>%
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
    plot_df <- session$userData$dq_main %>%
      select(PersonalID,
             ProjectID,
             ProjectName,
             OrganizationName,
             HouseholdID,
             Issue,
             Type) %>%
      unique()
  }

  plot_data <- plot_df %>%
    filter(Type == issueType) %>% 
    group_by(across(all_of(groupVars))) %>%
    summarise(countVar = n()) %>%
    ungroup() %>%
    arrange(desc(countVar))
  
  if(level == "org") {
    plot_data <- plot_data %>% 
      filter(OrganizationName %in% c(input$orgList))
  }

  # dynamically refer to the UI element ID
  outputId <- paste0(
    if_else(level == 'sys', 'system', 'org'),
    "DQ",
    if_else(issueType == 'High Priority', 'HighPriorityErrors', issueType),
    "By",
    group
  )
  
  # generate the plot
  # note there's no ui.R element with this ID, but it's, necessary to have an 
  # output element to refer to in the plotOutput statement below)
  output[[outputId]] <- renderPlot({
    req(session$userData$valid_file() == 1)
  
    issueTypeDisplay = if_else(issueType == "Warning", 
                               "warnings", 
                               "errors"
                               )
    
    validate(need(nrow(plot_data) > 0, 
                  message = paste0("Great job! No ",
                                   issueTypeDisplay,
                                   " to show.")
                  )
             )

    ggplot(head(plot_data, 10L),
           aes(
             x = reorder(!!as.name(x_group), countVar),
             y = countVar
           )) +
      geom_col(show.legend = FALSE,
               color = color,
               fill = color) +
      coord_flip() +
      labs(x = "",
           y = "Number of Enrollments") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
      scale_y_discrete(expand = expansion(mult = c(0, .1))) +
      theme_classic() +
      theme(axis.line = element_line(linetype = "blank"),
            axis.text = element_text(size = sys_axis_text_font),
            axis.text.x = element_blank(),
            axis.title = element_text(size = sys_axis_text_font),
            axis.ticks = element_line(linetype = "blank"),
            plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
      geom_text(aes(label = countVar), hjust = -0.5, color = "black",
                size = sys_chart_text_font)
  },
  alt = case_when(outputId == "systemDQHighPriorityErrorsByIssue" ~ "A bar chart of the top High Priority Errors in the system.",
                  outputId == "systemDQHighPriorityErrorsByOrg" ~ "A bar chart of the top organizations with the most High Priority Errors in the system.",
                  outputId == "systemDQErrorByIssue" ~ "A bar chart of the top General Errors in the system.",
                  outputId == "systemDQErrorByOrg" ~ "A bar chart of the top organizations with the most General Errors in the system.",
                  outputId == "systemDQWarningByIssue" ~ "A bar chart of the top Warnings in the system.",
                  outputId == "systemDQWarningByOrg" ~ "A bar chart of the top organizations with the most Warnings in the system.",
                  outputId == "orgDQHighPriorityErrorsByIssue" ~ "A bar chart of the top High Priority Errors in the organization.",
                  outputId == "orgDQHighPriorityErrorsByProject" ~ "A bar chart of the organization's projects with the most High Priority Errors.",
                  outputId == "orgDQErrorByIssue" ~ "A bar chart of the top General Errors in the organization.",
                  outputId == "orgDQErrorByProject" ~ "A bar chart of the organization's projects with the most General Errors.",
                  outputId == "orgDQWarningByIssue" ~ "A bar chart of the top Warnings in the organization.",
                  TRUE ~ "A bar chart of the organization's projects with the most Warnings.")
  )
  
  # this effectively collapses the plot if there are no rows
  plot_height = if_else(nrow(plot_data) == 0, 50, 400)
  
  # finally, render the plot
  return(plotOutput(outputId,
                    height = plot_height,
                    width = ifelse(isTRUE(getOption("shiny.testmode")),
                                    "1640",
                                    "100%")))
}

# list of data frames to include in DQ Org Report
dqDownloadInfo <- reactive({
  logToConsole(session, "in dqDownloadInfo")
  req(session$userData$valid_file() == 1)

  exportTestValues(dq_main = session$userData$dq_main %>% nice_names())
  exportTestValues(dq_overlaps = session$userData$overlap_details %>% nice_names())
  
  # org-level data prep (filtering to selected org)
  orgDQData <- session$userData$dq_main %>%
    filter(OrganizationName %in% c(input$orgList))
  
  orgDQoverlapDetails <- session$userData$overlap_details %>% 
    filter(OrganizationName %in% c(input$orgList) | 
             PreviousOrganizationName %in% c(input$orgList))
  
  browser()
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
      getDQReportDataList(session$userData$dq_main,
                          session$userData$overlap_details,
                          "OrganizationName",
                          session$userData$outstanding_referrals
      )
  )
})








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
