
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

dq_main_reactive <- reactive({
  ESNbN <- calculate_long_stayers_local_settings_dt(input$ESNbNLongStayers, 0)
  Outreach <- calculate_long_stayers_local_settings_dt(input$OUTLongStayers, 4)
  CoordinatedEntry <- calculate_long_stayers_local_settings_dt(input$CELongStayers, 14)
  ServicesOnly <- calculate_long_stayers_local_settings_dt(input$ServicesOnlyLongStayers, 6)
  Other <- calculate_long_stayers_local_settings_dt(input$OtherLongStayers, 7)
  DayShelter <- calculate_long_stayers_local_settings_dt(input$DayShelterLongStayers, 11)
  
  
  #Calculating potential old referrals based on Local settings
  CE_Event <- calculate_outstanding_referrals(input$CEOutstandingReferrals) %>%
    select(all_of(vars_we_want))
  
  rbind(dq_main_df() %>%
          filter(
            str_detect(tolower(Issue), "local settings", negate = TRUE) == TRUE
          ),
        ESNbN,
        Outreach,
        DayShelter,
        ServicesOnly,
        Other,
        CoordinatedEntry,
        CE_Event)
})

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
                               c(meta_HUDCSV_Export_Start(),
                                 meta_HUDCSV_Export_End(),
                                 meta_HUDCSV_Export_Date()))
    
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

# Non-Residential Long Stayers --------------------------------------------

cls_df <- reactive({
  validation() %>%
  filter(is.na(ExitDate)) %>% # less data to deal w/
  left_join(CurrentLivingSituation() %>%
              select(CurrentLivingSitID,
                     EnrollmentID,
                     InformationDate), by = "EnrollmentID") %>%
  group_by(EnrollmentID) %>%
  slice_max(InformationDate) %>%
  slice(1L) %>%
  ungroup() %>%
  select(EnrollmentID, "MaxCLSInformationDate" = InformationDate)
})

cls_project_types <- reactive({
  validation() %>%
  left_join(cls_df(), by = "EnrollmentID") %>%
  select(all_of(vars_prep), ProjectID, MaxCLSInformationDate) %>%
  mutate(
    Days = 
      as.numeric(difftime(
        as.Date(meta_HUDCSV_Export_Date()),
        if_else(!is.na(MaxCLSInformationDate),
                MaxCLSInformationDate, # most recent CLS
                EntryDate), # project entry
        units = "days"
      ))
  )
})

calculate_long_stayers_local_settings <- function(too_many_days, projecttype){
  
  entryexit_project_types <- validation() %>%
    filter(is.na(ExitDate) &
             !ProjectType %in% c(project_types_w_cls) &
             ProjectType == projecttype
    ) %>%
    mutate(
      Days =
        as.numeric(difftime(
          as.Date(meta_HUDCSV_Export_Date()), EntryDate, 
          units = "days"
        ))
    ) %>%
    filter(too_many_days < Days) %>%
    merge_check_info(checkIDs = 102) %>%
    select(all_of(vars_we_want))
  
  if (projecttype %in% c(project_types_w_cls)) {
    cls_project_types() %>%
      filter(is.na(ExitDate) &
       ProjectType %in% c(project_types_w_cls) &
       ProjectType == projecttype & 
       too_many_days < Days) %>%
      merge_check_info(checkIDs = 103) %>%
      select(all_of(vars_we_want))
  } else{
    entryexit_project_types
  } 
  
}

calculate_long_stayers_local_settings_dt <- function(too_many_days, projecttype){
  if (projecttype %in% c(project_types_w_cls)) {
    merge_check_info_dt(
      as.data.table(cls_project_types())[
        is.na(ExitDate) &
          ProjectType %in% project_types_w_cls &
          ProjectType == projecttype &
          too_many_days < Days,
      ],
      103
    )[, ..vars_we_want]
  } else{
    entryexit_project_types <- as.data.table(validation())
    entryexit_project_types[, Days := as.numeric(difftime(
      as.Date(meta_HUDCSV_Export_Date()), EntryDate, units = "days"
    ))]
    merge_check_info_dt(entryexit_project_types[
      is.na(ExitDate) &
        !ProjectType %in% project_types_w_cls &
        ProjectType == projecttype &
        too_many_days < Days],
      102
    )[, ..vars_we_want]
  } 
  
}
# Outstanding Referrals --------------------------------------------

calculate_outstanding_referrals <- function(too_many_days){

  base_dq_data_func() %>%
    left_join(Event() %>% select(EnrollmentID,
                               EventID,
                               EventDate,
                               Event,
                               ProbSolDivRRResult,
                               ReferralCaseManageAfter,
                               LocationCrisisOrPHHousing,
                               ReferralResult,
                               ResultDate),
              by = "EnrollmentID") %>%
    select(all_of(vars_prep), ProjectID, EventID, EventDate, ResultDate, Event) %>%
    mutate(
      Days = 
        as.numeric(
          difftime(as.Date(meta_HUDCSV_Export_Date()), EventDate, units = "days")),
      EventType = case_when(
        Event == 10 ~ "Referral to Emergency Shelter bed opening",
        Event == 11 ~ "Referral to Transitional Housing bed/unit opening",
        Event == 12 ~ "Referral to Joint TH-RRH project/unit/resource opening",
        Event == 13 ~ "Referral to RRH project resource opening",
        Event == 14 ~ "Referral to PSH project resource opening",
        Event == 15 ~ "Referral to Other PH project/unit/resource opening",
        Event == 17 ~ "Referral to Emergency Housing Voucher (EHV)",
        Event == 18 ~ "Referral to a Housing Stability Voucher"
      )
    ) %>%
    filter(Event %in% c(10:15, 17:18) &
             is.na(ResultDate) &
             too_many_days < Days) %>%
    merge_check_info(checkIDs = 100)
}

renderDQPlot <- function(level, issueType, group, color) {
  req(nrow(dq_main_df()) > 0 & valid_file() == 1)
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
    plot_df <- dq_main_df() %>%
      left_join(Project0() %>%
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
    plot_df <- dq_main_df() %>%
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
    req(valid_file() == 1)
  
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
  ,
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
  req(valid_file() == 1)
  
  exportTestValues(dq_main_reactive =  dq_main_reactive() %>% nice_names())
  exportTestValues(dq_overlaps = overlap_details() %>% nice_names())
  
  # org-level data prep (filtering to selected org)
  orgDQData <- dq_main_reactive() %>%
    filter(OrganizationName %in% c(input$orgList))
  
  orgDQoverlapDetails <- overlap_details() %>% 
    filter(OrganizationName %in% c(input$orgList) | 
             PreviousOrganizationName %in% c(input$orgList))
  
  orgDQReferrals <- 
    calculate_outstanding_referrals(input$CEOutstandingReferrals) %>%
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
      getDQReportDataList(dq_main_reactive(),
                          overlap_details(),
                          "OrganizationName",
                          calculate_outstanding_referrals(input$CEOutstandingReferrals)
      )
  )
})
