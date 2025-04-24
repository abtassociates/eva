
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
  # Long Stayers -------------------------------------------------------------
  # The goal is here to flag "stays" that go beyond the local setting 
  # (that defines a "long" stay), and is set by the user
  # A "stay" is the time between when we last "heard" from an enrollment and Export Date
  # How we determine the last time we heard from an enrollment differs by Project Type
  
  ## ES NbN --------------------
  ESNbN <- calculate_long_stayers_local_settings_dt(input$ESNbNLongStayers, es_nbn_project_type) #1
  
  ## Non-Residential Projects (other than HP projects) --------
  Outreach <- calculate_long_stayers_local_settings_dt(input$OUTLongStayers, out_project_type) #4
  ServicesOnly <- calculate_long_stayers_local_settings_dt(input$ServicesOnlyLongStayers, sso_project_type) #6
  Other <- calculate_long_stayers_local_settings_dt(input$OtherLongStayers, other_project_project_type) #7
  DayShelter <- calculate_long_stayers_local_settings_dt(input$DayShelterLongStayers, day_project_type) #11
  CoordinatedEntry <- calculate_long_stayers_local_settings_dt(input$CELongStayers, ce_project_type) #14
  
  # Calculating potential old referrals based on Local settings ---------
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

calculate_long_stayers_local_settings_dt <- function(too_many_days, projecttype){
  # get non-exited enrollments for projecttype
  non_exits <- validation() %>%
    fsubset(ProjectType == projecttype & 
             (ExitDate >= meta_HUDCSV_Export_End() | is.na(ExitDate))
    ) %>%
    fselect(vars_prep)
  
  # only proceed if there are any non-exited enrollments
  if(nrow(non_exits) == 0) return(NULL)
  
  # data with last-known dates
  # we're going to later compute the LAST Known Date to determine when we last heard from them
  # this starts the clock of how long their stay is.
  data_w_dates <- if(projecttype %in% c(out_project_type, sso_project_type, ce_project_type)) {
    # This will be merged back into non_exits
    CurrentLivingSituation() %>% fselect(EnrollmentID, KnownDate = InformationDate)
  } else if(projecttype == es_nbn_project_type) {
    # This will be merged back into non_exits
    services() %>% fselect(EnrollmentID, KnownDate = DateProvided)
  } else {
    # If a different project type, we'll just use their EntryDate as the KnownDate
    non_exits %>% fselect(vars_prep, KnownDate = EntryDate)
  }
  
  # calculate last-known date (differs by project type)
  if(projecttype %in% c(other_project_project_type, day_project_type)) {
    # LastKnown = KnownDate (not fmax) because it's per enrollment, and EntryDate (now KnownDate) is at Enrollment level
    non_exits_w_lastknown_date <- data_w_dates %>%
      fselect(vars_prep, LastKnown = KnownDate)
  } else {
    non_exits_w_lastknown_date <- 
      join(non_exits, data_w_dates, on = "EnrollmentID", how="left") %>%
      fgroup_by(EnrollmentID) %>%
      # Take EntryDate if there's no Information or DateProvided
      fmutate(LastKnown = fcoalesce(fmax(KnownDate), EntryDate))
  }
  
  # calculate days since last known
  long_stayers <- qDT(non_exits_w_lastknown_date) %>%
    fmutate(
      DaysSinceLastKnown = as.numeric(difftime(
        as.Date(meta_HUDCSV_Export_Date()), LastKnown, units = "days"
      ))
    ) %>%
    # NOW FILTER DOWN TO THE PROBLEM CASES
    fsubset(DaysSinceLastKnown > too_many_days)
    
  # Each project type gets its own Issue text+Guidance etc.
  merge_check_info_dt(
    long_stayers,
    case_when(
      projecttype %in% c(out_project_type, sso_project_type, ce_project_type) ~ 103,
      projecttype == es_nbn_project_type ~ 142,
      projecttype %in% c(other_project_project_type, day_project_type) ~ 102
    )
  )[, ..vars_we_want]
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
