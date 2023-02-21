dq_main_reactive <- reactive({
  req(valid_file()== 1)
  # browser()
  ESNbN <- calculate_long_stayers(input$ESNbNLongStayers, 0)
  Other <- calculate_long_stayers(input$OtherLongStayers, 7)
  Outreach <- calculate_long_stayers(input$OUTLongStayers, 4)
  DayShelter <- calculate_long_stayers(input$DayShelterLongStayers, 11)
  ServicesOnly <- calculate_long_stayers(input$ServicesOnlyLongStayers, 6)
  
  #Calculating potential old referrals based on Local settings
  CE_Event <- calculate_outstanding_referrals(input$CEOutstandingReferrals) %>%
    select(all_of(vars_we_want))
  
  x <- dq_main %>%
    filter(!Issue %in% c("Days Enrollment Active Exceeds Local settings", 
                         "Days Referral Active Exceeds Local settings"))
  
  rbind(x, ESNbN, Outreach, DayShelter, ServicesOnly, Other, CE_Event)
  
})

getDQReportDataList <-
  function(dqData,
           dqOverlaps = NULL,
           bySummaryLevel = NULL,
           dqReferrals = NULL) {
    
    select_list <- c(
      "Organization Name" = "OrganizationName",
      "Project ID" = "ProjectID",
      "Project Name" = "ProjectName",
      "Issue" = "Issue",
      "Personal ID" = "PersonalID",
      "Household ID" = "HouseholdID",
      "Entry Date" = "EntryDate"
    )
    
    high_priority <- dqData %>%
      filter(Type == "High Priority") %>%
      select(all_of(select_list))
    
    errors <- dqData %>%
      filter(Type == "Error") %>%
      select(all_of(select_list))
    
    warnings <- dqData %>%
      filter(Type == "Warning") %>%
      select(all_of(select_list))
    
    dqOverlapDetails <- dqOverlaps %>%
      select(-c(Issue, Type, Guidance, PreviousIssue)) %>%
      relocate(
        OrganizationName,
        ProjectID,
        ProjectName,
        ProjectType,
        EnrollmentID,
        HouseholdID,
        PersonalID,
        EntryDate,
        FirstDateProvided,
        "MoveInDate" = MoveInDateAdjust,
        ExitDate,
        PreviousOrganizationName,
        PreviousProjectID,
        PreviousProjectName,
        PreviousProjectType,
        PreviousEnrollmentID,
        PreviousHouseholdID,
        PreviousPersonalID,
        PreviousEntryDate,
        PreviousFirstDateProvided,
        "PreviousMoveInDate" = PreviousMoveInDateAdjust,
        PreviousExitDate
      )
    
    dqReferralDetails <- dqReferrals %>%
      filter(Issue == "Days Referral Active Exceeds Local Settings") %>%
      select(
        OrganizationName,
        ProjectID,
        ProjectName,
        EventID,
        PersonalID,
        EventDate,
        EventType,
        Days
      )
    
    mainsummary <- rbind(
      dqData %>% select(Type, Issue, PersonalID),
      dqOverlaps %>% select(Type, Issue, PersonalID)
    ) %>%
      # group_by(ProjectName, Type, Issue) %>%
      group_by(Type, Issue) %>%
      summarise(Enrollments = n()) %>%
      ungroup() %>%
      select(Type, Enrollments, Issue) %>%
      arrange(Type, desc(Enrollments))
    
    bySummaryLevel2 <- rlang::sym(bySummaryLevel)
    byunitsummary <- rbind(
      dqData %>% select(!!bySummaryLevel2, Type, Issue, PersonalID),
      dqOverlaps %>% select(!!bySummaryLevel2, Type, Issue, PersonalID)
    ) %>%
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
                               c(meta_HUDCSV_Export_Start,
                                 meta_HUDCSV_Export_End,
                                 meta_HUDCSV_Export_Date))
    
    colnames(exportDetail) <- c("Export Field", "Value")
    
    exportDFList <- list(
      exportDetail = exportDetail,
      mainsummary = mainsummary,
      byunisummary = byunitsummary,
      guidance = guidance,
      high_priority = high_priority,
      errors = errors,
      warnings = warnings,
      overlaps = dqOverlapDetails,
      dqReferrals = dqReferralDetails
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

calculate_long_stayers <- function(input, projecttype){
  
  served_in_date_range %>%
    select(all_of(vars_prep), ProjectID) %>%
    mutate(
      Days = 
        as.numeric(
          difftime(as.Date(meta_HUDCSV_Export_Date), EntryDate, units = "days")),
      Issue = "Days Enrollment Active Exceeds Local Settings",
      Type = "Warning",
      Guidance = str_squish("You have at least one active enrollment that has been
         active for longer than the days set for this Project Type in your
         Referral settings on the Edit Local Settings tab.")
    ) %>%
    filter(is.na(ExitDate) &
             ProjectType == projecttype &
             input < Days) %>% 
    select(all_of(vars_we_want))
  
}

# Outstanding Referrals --------------------------------------------

calculate_outstanding_referrals <- function(input){
  
  served_in_date_range %>%
    left_join(Event %>% select(EnrollmentID,
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
          difftime(as.Date(meta_HUDCSV_Export_Date), EventDate, units = "days")),
      Issue = "Days Referral Active Exceeds Local Settings",
      Type = "Warning",
      Guidance = str_squish("You have at least one active referral that has been
         active without a Result Date for longer than the days set in your
         Local Settings on the Home tab."),
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
    filter(Event %in% c(10:15,17:18) &
             is.na(ResultDate) &
             input < Days)
  
}

renderDQPlot <- function(inDS, x_group, y_field, color) {
  renderPlot({
    req(valid_file() == 1)
  
    validate(need(nrow(inDS) > 0, 
                  message = "Great job! No errors to show."))

    ggplot(head(inDS, 10L),
           aes(
             x = reorder(!!as.name(x_group), !!as.name(y_field)),
             y = !!as.name(y_field)
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
            axis.text = element_text(size = 12),
            axis.text.x = element_blank(),
            axis.title = element_text(size = 12),
            axis.ticks = element_line(linetype = "blank"),
            plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
      geom_text(aes(label = !!as.name(y_field)), hjust = -0.5, color = "black")
  })
}

