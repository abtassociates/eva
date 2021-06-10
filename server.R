# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

function(input, output, session) {
  output$headerHome <- renderUI({
    box(
      title = "Welcome",
      width = 12,
      HTML(
        "<p>R minor elevated is intended for use by the Ohio Balance of State CoC
        and the Mahoning County CoC HMIS users. This site requires a login 
        because client-level data is shown (without Personally Identifying 
        Information). Please use this site to verify that your HMIS data is 
        accurate and complete.
        <p><a href=\"https://ohiobalanceofstatecoc.shinyapps.io/Rminor\"
        target=\"_blank\">R minor</a> is a separate COHHIO site used for 
        performance reporting. Visitors to R minor will include HMIS users, 
        program executives, funders, government representatives, advocates, and 
        other interested parties. R minor contains no client-level data.<br>
        <p>We're glad you're here! Please select a report in the left sidebar."
      )
    )
    
  })
  
  pass_icon <- '<span style="color: teal; font-size: 150%;">
            <i class="fas fa-check"></i>
            </span>'
  fail_icon <- '<span style="color: tomato; font-size: 150%;">
            <i class="fas fa-times"></i>
            </span>'
  unknown_icon <- '<span style="color: grey; font-size: 150%;">
            <i class="fas fa-question-circle"></i>
            </span>'
  alert_icon <- '<span style="color: goldenrod; font-size: 150%;">
            <i class="fas fa-exclamation-triangle"></i>
            </span>'
  
  output$headerPrioritization <- renderUI({
    list(h2("Prioritization Report"),
         h4("Literally Homeless Clients as of", meta_HUDCSV_Export_End))
  })
  
  output$headerCurrent <- renderUI({
    list(h2("Client Counts Report"),
         h4(input$currentProviderList))
  })
  
  output$headerVaccine <- renderUI({
    list(h2("COVID-19 Vaccine Distribution"),
         h3("Second Dose Logistics"),
         h4(paste("Data Last Refreshed:", meta_HUDCSV_Export_Date)))
  })
  
  output$headerVaccineStatus <- renderUI({
    reportstart <- input$vaccine_status_daterange[1]
    reportend <- input$vaccine_status_daterange[2]

    list(h2("COVID-19 Vaccine Status"),
         h3(paste("Date Range:", reportstart, "to", reportend)))
  })
  
  output$headerUtilization <- renderUI({
    list(h2("Bed and Unit Utilization"),
         h4(input$providerListUtilization),
         h4(format(ymd(
           input$utilizationDate
         ), "%B %Y"))
         )
  })
  
  output$headerCoCCompetitionProjectLevel <- renderUI({
    
    next_thing_due <- tribble(
      ~ DueDate, ~ Event,
      ymd(hc_project_eval_docs_due), "Projects submit program documents to evidence 
      best practices and CE Prioritization compliance",
      ymd("20210506"), "All HMIS data corrections must be complete by 11:59pm",
      ymd("20210507"), "Project Evaluation data is saved as final data for scoring",
      ymd("20210527"), "CoC staff post online preliminary renewal project ranking",
      ymd("20210604"), "Recipients submit appeals of project evaluation results and 
      ranking to ohioboscoc@cohhio.org.",
      ymd("20210625"), "Final CoC project ranking released"
    ) %>%
      mutate(
        ShowStart = lag(ymd(DueDate), n = 1L, order_by = DueDate),
        ShowStart = if_else(is.na(ShowStart), today(), ShowStart + days(1)),
        ShowEnd = ymd(DueDate),
        DateRange = interval(ShowStart, ShowEnd)
      ) %>%
      filter(today() %within% DateRange) %>%
      select(Event, DueDate)
    
    list(
      h2("2021 BoS CoC Competition: Project Evaluation"), 
      h4(paste("Fixed Date Range:", 
               format.Date(hc_project_eval_start, "%B %d, %Y"), 
               "to",
               format.Date(hc_project_eval_end, "%B %d, %Y"))),
      h4(strong("THE DATA ON THIS TAB DOES NOT SHOW CHANGES MADE ON OR AFTER
                5-7-2021.")),
      h4(input$pe_provider),
      hr(),
      h5(strong("Next Due Date:"),
         format(ymd(next_thing_due$DueDate), "%A %b %e, %Y"),
         "| ",
         next_thing_due$Event),
      p("Please consult the", 
            a("CoC Program",
        href = "https://cohhio.org/boscoc/coc-program/", target="_blank"), 
        "page for complete specifications and timeline.")
    )
  })
  
  output$headerDataQuality <- renderUI({
    list(h2("Data Quality"),
         h4(paste(
           format(input$dq_startdate, "%m-%d-%Y"),
           "to",
           format(meta_HUDCSV_Export_Date, "%m-%d-%Y")
         )))
  })
  
  output$headerDeskTime <- renderUI({
    list(h2("Data Entry Timeliness"),
         h4(input$providersDeskTime),
         h4(paste("Fixed Date Range:",
                  format(today() - years(1), "%m-%d-%Y"),
                  "to",
                  format(today(), "%m-%d-%Y"))))
  })
  
  output$deskTimeNote <-
    renderUI(
      HTML(
        "<h4>HUD and Data Quality</h4>
        <p>HUD defines \"Data Quality\" as having three elements:
    1. Accuracy, 2. Completeness, and 3. Timeliness. Data Entry Delay (aka
    \"Desk Time\") refers to how long it is taking to enter a client into HMIS
    from the day they enter your project.
    <h4>Ohio Balance of State CoC Data Standards</h4>
    <p>According to the Data Quality Standards for the Ohio Balance of State
    CoC, all clients should be entered within 5 days of their entry into your
    project.
    <h4>How Do We Fix This?</h4>
    <p><strong>There is nothing a user can do</strong> to \"correct\" a client
    entered into the system outside the window. We can only resolve to enter
    clients within the 5-day range going forward. As you catch up on data entry,
    you may see your median get worse at first, but this data looks back exactly
    one year, so any clients with an Entry Date over a year ago will fall off
    of this plot and your median will change accordingly.
    <h4>Interpretation</h4>
    <p>Green dots here represent clients entered within the range and orange
    dots represent clients entered outside the range. The darker the dot, the
    more clients entered your project on that day. (Likely a household.)
    <p>The metric COHHIO looks at here is the Median, so if you have orange dots
    but your Median is within the 5 day range, that is great!
    <p>If you have orange dots BELOW the 0 mark, that means you entered Entry
    Dates into the future, which means there is potentially a mis-keyed date or
    the user needs technical assistance about how to know what date to enter for
    the Entry Date. If this is the case, please email the HMIS team.
            <h4>Is it possible there's a mistake?</h4>
    It's rare that this occurs, but if an Entry Exit has been created, deleted,
    and then recreated, the Entry Exit's \"Date Created\" date is reset,
    thus inflating the number of days between the Date Created and the Entry Date.
    If you need us to check if this was the case for a particular dot on the
    plot, please email us with the provider and number of days it is
    displaying that you think may be incorrect so we can verify if this is the
        issue."
      )
    )
  
  output$headerUnshDataQuality <- renderUI({
    list(h2("Unsheltered Data Quality"),
         h4("Entered into the Unsheltered Provider by a User whose Default 
            Provider is", input$unshDefaultProvidersList),
         h4(paste(
           format(input$unsh_dq_startdate, "%m-%d-%Y"),
           "to",
           format(meta_HUDCSV_Export_Date, "%m-%d-%Y")
         )))
  })
  
  output$headerCocDQ <- renderUI({
    list(h2("System-wide Data Quality"),
         h4(
           paste(format(hc_check_dq_back_to, "%m-%d-%Y"),
                 "through",
                 format(meta_HUDCSV_Export_End, "%m-%d-%Y"))
         ))
  })
  
  output$headerRegionDataQuality <- renderUI({
    list(h2("Regional Data Quality"),
         h4(input$regionList3),
         h4(paste(
           format(input$dq_region_startdate, "%m-%d-%Y"),
           "to",
           format(meta_HUDCSV_Export_Date, "%m-%d-%Y")
         )))
  })
  
  output$headerExitsToPH <- renderUI({
    ReportStart <- format.Date(input$ExitsToPHDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$ExitsToPHDateRange[2], "%B %d, %Y")
    
    list(h2("Successful Placement Detail"),
         h4(input$ExitsToPHProjectList),
         h4(paste(
           ReportStart,
           "to",
           ReportEnd
         )))
  })
  
  output$DeskTimePlotDetail <- renderPlot({
  provider <- input$providerDeskTime
  
  ReportStart <- format.Date(ymd(today() - years(1)), "%m-%d-%Y")
  ReportEnd <- format.Date(ymd(today()), "%m-%d-%Y")
  
  desk_time <- validation() %>%
    filter(ProjectName == provider &
             entered_between(., ReportStart, ReportEnd) &
             ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
    select(ProjectName, PersonalID, HouseholdID, EntryDate, DateCreated) %>%
    mutate(
      DeskTime = difftime(floor_date(DateCreated, unit = "day"),
                          EntryDate,
                          units = "days"),
      DeskTime = as.integer(floor(DeskTime)),
      GoalMet = if_else(DeskTime > 5 |
                          DeskTime < 0,
                        "chocolate2",
                        "forestgreen")
    ) %>%
    select(HouseholdID,
           PersonalID,
           ProjectName,
           EntryDate,
           DateCreated,
           DeskTime,
           GoalMet) 
  
  desk_time_medians <- desk_time %>%
    group_by(ProjectName) %>%
    summarise(MedianDeskTime = median(DeskTime),
              TotalEntered = n()) %>%
    ungroup()
  
  dq_plot_desk_time <-
    ggplot(
      desk_time,
      aes(x = EntryDate, y = DeskTime)
    ) +
    geom_point(aes(color = GoalMet, size = 8, alpha = .2),
               show.legend = FALSE)+
    scale_color_identity() +
    geom_hline(yintercept = 5, color = "forestgreen") +
    geom_hline(yintercept = 0, color = "forestgreen") +
    geom_hline(
      data = desk_time_medians,
      aes(yintercept = MedianDeskTime),
      color = "black"
    ) +
    xlim(today() - years(1), today()) +
    geom_label(x = today() - days(180),
               y = desk_time_medians %>%
                 pull(MedianDeskTime),
               label = paste("Median:", 
                             desk_time_medians %>%
                               pull(MedianDeskTime),
                             "days | Total Clients:",
                             desk_time_medians %>%
                               pull(TotalEntered))) +
    geom_label(x = today() - days(300),
               y = 5,
               label = "DQ Standards (5 days or less)") +
    labs(x = "Entry Date",
         y = "Data Entry Delay (in days)") +
    theme_minimal(base_size = 18)
  
  dq_plot_desk_time
})
  
  output$DeskTimePlotCoC <- renderPlot({
  provider <- input$providerDeskTimeCoC

  ReportStart <- format.Date(ymd(today() - years(1)), "%m-%d-%Y")
  ReportEnd <- format.Date(ymd(today()), "%m-%d-%Y")
  
  desk_time <- validation() %>%
    filter(entered_between(., ReportStart, ReportEnd) &
             ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
    select(ProjectName, PersonalID, HouseholdID, EntryDate, DateCreated) %>%
    mutate(
      DeskTime = difftime(floor_date(DateCreated, unit = "day"),
                          EntryDate,
                          units = "days"),
      DeskTime = as.integer(floor(DeskTime))
    ) %>%
    select(HouseholdID,
           PersonalID,
           ProjectName,
           EntryDate,
           DateCreated,
           DeskTime) 
  
  desk_time_medians <- desk_time %>%
    group_by(ProjectName) %>%
    summarise(MedianDeskTime = median(DeskTime)) %>%
    ungroup() %>%
    arrange(desc(MedianDeskTime))
  
  ggplot(
    head(desk_time_medians, 10L),
    aes(
      x = reorder(ProjectName, MedianDeskTime),
      y = MedianDeskTime,
      fill = MedianDeskTime
    )
  ) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = "",
         y = "Median Days") +
    scale_fill_viridis_c(direction = -1) +
    theme_minimal(base_size = 18)
  
})
  
  output$ExitsToPHSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$ExitsToPHDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$ExitsToPHDateRange[2], "%m-%d-%Y")
      
      SuccessfullyPlaced <- qpr_leavers() %>%
        filter(ProjectName == input$ExitsToPHProjectList &
                 ((ProjectType %in% c(3, 9, 13) &
                   !is.na(MoveInDateAdjust)) |
                  ProjectType %in% c(1, 2, 4, 8, 12)
        ) &
          # excluding non-mover-inners
          (((DestinationGroup == "Permanent" |
               #exited to ph or still in PSH/HP
               is.na(ExitDate)) &
              ProjectType %in% c(3, 9, 12) &
              served_between(., ReportStart, ReportEnd)# PSH & HP
          ) |
            (
              DestinationGroup == "Permanent" & # exited to ph
                ProjectType %in% c(1, 2, 4, 8, 13) &
                exited_between(., ReportStart, ReportEnd)
            )
          )) %>%
        group_by(ProjectName) %>%
        count()
      
      # calculating the total households to compare successful placements to
      TotalHHsSuccessfulPlacement <- qpr_leavers() %>%
        filter(ProjectName == input$ExitsToPHProjectList &
                 ((
                   served_between(., ReportStart, ReportEnd) &
                     ProjectType %in% c(3, 9, 12) # PSH & HP
                 ) |
                   (
                     exited_between(., ReportStart, ReportEnd) &
                       ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
                   )
                 )) %>%
        group_by(ProjectName) %>%
        count()
      
      infoBox(
        title = "Successfully Placed",
        color = "blue",
        icon = icon("key"),
        value = paste(SuccessfullyPlaced$n,
                      "/",
                      TotalHHsSuccessfulPlacement$n,
                      "households")
      )
      
    })
  
  
  
  output$headerIncomeIncrease <- renderUI({
    ReportStart <- format.Date(input$IncomeDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$IncomeDateRange[2], "%B %d, %Y")
    
    list(h2("Income Increase"),
         h4(input$incomeProjectList),
         h4(paste(
           ReportStart,
           "to",
           ReportEnd
         )))
  })
  
  output$headerRRHSpending <- renderUI({
    ReportStart <- format.Date(input$RRHSpendingDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$RRHSpendingDateRange[2], "%B %d, %Y")
    list(h2("Quarterly Performance Report"),
         h3("Rapid Rehousing Spending Goals"),
         # h4(input$RRHRegion),
         h4(ReportStart, "-", ReportEnd))
  })
  
  output$headerDaysToHouse <- renderUI({
    ReportStart <- format.Date(input$LoSDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$LoSDateRange[2], "%B %d, %Y")
    
    list(h1("Days to House"),
         h4(input$RapidRRHProviderList),
         h4(paste(
           ReportStart,
           "to",
           ReportEnd
         )))
  })
  
  output$vaccineStatusDataTable <- DT::renderDataTable({
    reportstart <- input$vaccine_status_daterange[1]
    reportend <- input$vaccine_status_daterange[2]
    
    x <- vaccine_status() %>%
      mutate(
        HH_status = case_when(
          str_starts(HouseholdID, "s") ~ "Single",
          str_starts(HouseholdID, "h") & RelationshipToHoH == 1 ~ "Head of Household",
          str_starts(HouseholdID, "h") & RelationshipToHoH != 1 ~ "Household member"
        ),
        PersonalID = paste(PersonalID, "<br><small>", HH_status, "</small>"),
        VeteranStatus = enhanced_yes_no_translator(VeteranStatus)
      ) %>%
      filter(CountyServed %in% c(input$vaccineStatusCounty) &
               served_between(., reportstart,
                              reportend)) %>%
      arrange(HouseholdID) %>%
      select(
        "Client ID" = PersonalID,
        "County" = CountyServed,
        "Provider Name" = ProjectName,
        "Age at Entry" = AgeAtEntry,
        "Veteran" = VeteranStatus,
        "Entry Date" = EntryDate,
        "Move In Date" = MoveInDateAdjust,
        "Exit Date" = ExitDate,
        "Vaccine Status" = VaccineStatus
      )
    
    datatable(x,
              rownames = FALSE,
              escape = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
  })
  
  output$prioritizationData <- DT::renderDataTable({
    
    active <- active_list() %>%
      filter(CountyServed %in% c(input$prioritizationCounty) |
               is.na(CountyServed)) %>%
      arrange(COVID19Priority) %>%
      mutate(EntryDate = format.Date(ymd(EntryDate), "%m-%d-%Y")) %>%
      select(
        "HoH Client ID" = PersonalID,
        "Project Name" = ProjectName,
        "Entry Date" = EntryDate,
        "County" = CountyServed,
        "Current Situation (Entry, Referral, Perm Housing Track)" = Situation,
        "COVID-19: Priority for Immediate Non-congregate Housing" = COVID19Priority,
        "Veteran" = VeteranStatus,
        "Fleeing DV" = CurrentlyFleeing,
        "Transition Aged Youth" = TAY,
        "Chronic Status" = ChronicStatus,
        "Eligible for PSH (Disability in Household)" = DisabilityInHH,
        Score,
        "Household Size" = HouseholdSize,
        "Income" = IncomeFromAnySource,
        HH_DQ_Issue,
        CountyGuessed
      )
    
    datatable(
      active,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi',
                     columnDefs = list(list(
                       visible = FALSE, 
                       targets = c(14:15)
                     )), 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$('th').css({'text-align': 'center'});",
                       "$('td').css({'text-align': 'center'});",
                       "}"))
    ) %>%
      formatStyle(
        columns = 1, # HoH Client ID indices
        valueColumns = 15, # HH_DQ_issue indices
        color = styleEqual(c(1),
                           c("white")),
        backgroundColor = styleEqual(c(1),
                                     c("#7d7d8d"))
      ) %>%
      formatStyle(
        columns = 4, # County
        valueColumns = 16, # CountyGuessed indices
        color = styleEqual(c(1),
                           c("white")),
        backgroundColor = styleEqual(c(1),
                                     c("#7d7d8d"))
      )
    
  })
  
  output$downloadActiveList <- downloadHandler(
    filename = function() {
      "active_list.xlsx"
    },
    content = function(file) {
      write_xlsx(active_list() %>%
                   filter(
                     CountyServed %in% c(input$prioritizationCounty) |
                       is.na(CountyServed)
                   ), path = file)
    }
  )
  
  output$clientCountData <- DT::renderDataTable({
    ReportStart <- format.Date(input$dateRangeCount[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$dateRangeCount[2], "%m-%d-%Y")
    
    datatable(
      validation() %>%
        filter(served_between(., ReportStart, ReportEnd) &
                 ProjectName == input$currentProviderList) %>%
        mutate(
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
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ paste0("Currently Awaiting Housing (", 
                                       today() - EntryDate,
                                       " days)"),
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ paste0("Currently Moved In (",
                                      today() - MoveInDateAdjust,
                                      " days)"),
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ paste0("Currently in project (",
                                       today() - EntryDate, 
                                       " days)"),
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited project",
          ),
          sort = today() - EntryDate
        ) %>%
        mutate(PersonalID = as.character(PersonalID)) %>%
        arrange(desc(sort), HouseholdID, PersonalID) %>%
        select(
          "County" = CountyServed,
          "Client ID" = PersonalID,
          "Relationship to Head of Household" = RelationshipToHoH,
          "Entry Date" = EntryDate,
          "Move In Date (RRH/PSH Only)" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          Status
        ),
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    )
  })
  
  output$clientCountSummary <- DT::renderDataTable({
    ReportStart <- format.Date(input$dateRangeCount[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$dateRangeCount[2], "%m-%d-%Y")
    
    hhs <- validation() %>%
      filter(served_between(., ReportStart, ReportEnd) &
               ProjectName == input$currentProviderList) %>%
      select(HouseholdID,
             ProjectType,
             EntryDate,
             MoveInDateAdjust,
             ExitDate) %>%
      unique() %>%
      mutate(
        # Entered = if_else(between(EntryDate, ReportStart, ReportEnd),
        #                   "Entered in date range", "Entered outside date range"),
        # Leaver = if_else(!is.na(ExitDate), "Leaver", "Stayer"),
        Status = case_when(
          ProjectType %in% c(3, 13) &
            is.na(MoveInDateAdjust) &
            is.na(ExitDate) ~ "Currently Awaiting Housing",
          ProjectType %in% c(3, 13) &
            !is.na(MoveInDateAdjust) &
            is.na(ExitDate) ~ "Currently Moved In",
          ProjectType %in% c(3, 13) &
            is.na(MoveInDateAdjust) &
            !is.na(ExitDate) ~ "Exited No Move-In",
          ProjectType %in% c(3, 13) &
            !is.na(MoveInDateAdjust) &
            !is.na(ExitDate) ~ "Exited with Move-In",
          !ProjectType %in% c(3, 13) &
            is.na(ExitDate) ~ "Currently in project",
          !ProjectType %in% c(3, 13) &
            !is.na(ExitDate) ~ "Exited project",
        )
      ) %>%
      group_by(Status) %>%
      summarise(Households = n())
    
    clients <- validation() %>%
      filter(served_between(., ReportStart, ReportEnd) &
               ProjectName == input$currentProviderList) %>%
      select(PersonalID,
             ProjectType,
             EntryDate,
             MoveInDateAdjust,
             ExitDate) %>%
      unique() %>%
      mutate(
        Status = case_when(
          ProjectType %in% c(3, 13) &
            is.na(MoveInDateAdjust) &
            is.na(ExitDate) ~ "Currently Awaiting Housing",
          ProjectType %in% c(3, 13) &
            !is.na(MoveInDateAdjust) &
            is.na(ExitDate) ~ "Currently Moved In",
          ProjectType %in% c(3, 13) &
            is.na(MoveInDateAdjust) &
            !is.na(ExitDate) ~ "Exited No Move-In",
          ProjectType %in% c(3, 13) &
            !is.na(MoveInDateAdjust) &
            !is.na(ExitDate) ~ "Exited with Move-In",
          !ProjectType %in% c(3, 13) &
            is.na(ExitDate) ~ "Currently in project",
          !ProjectType %in% c(3, 13) &
            !is.na(ExitDate) ~ "Exited project",
        )
      ) %>%
      group_by(Status) %>%
      summarise(Clients = n())
    
    final <- full_join(clients, hhs, by = "Status")
    
    datatable(
      final,
      rownames = FALSE,
      filter = 'none',
      options = list(dom = 't')
    )
  })
  
  output$vaccineSecondDoseOverdue <- DT::renderDataTable({
    
    needs_2nd <- vaccine_needs_second_dose() %>%
      filter(CountyServed %in% c(input$vaccineCounty) &
               HowSoon == "Overdue") %>%
      arrange(NextDoseNeededDate, HouseholdID) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      select(
        "Client ID" = PersonalID,
        "County" = CountyServed,
        "Vaccine Manufacturer" = COVID19VaccineManufacturer,
        "Age Range" = AgeAtEntry,
        "Veteran Status" = VeteranStatus,
        "Date Next Dose Needed" = NextDoseNeededDate,
        "Current Location and Client Contact Info" = CurrentLocation
      )
    
    datatable(
      needs_2nd,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    ) 
  })
  
  output$vaccineSecondDose3Days <- DT::renderDataTable({
    
    needs_2nd <- vaccine_needs_second_dose() %>%
      filter(CountyServed %in% c(input$vaccineCounty) &
               HowSoon == "3 days") %>%
      arrange(NextDoseNeededDate, HouseholdID) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      select(
        "Client ID" = PersonalID,
        "County" = CountyServed,
        "Vaccine Manufacturer" = COVID19VaccineManufacturer,
        "Age Range" = AgeAtEntry,
        "Veteran Status" = VeteranStatus,
        "Date Next Dose Needed" = NextDoseNeededDate,
        "Current Location and Client Contact Info" = CurrentLocation
      )
    
    datatable(
      needs_2nd,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    ) 
  })
  
  output$vaccineSecondDose7Days <- DT::renderDataTable({
    
    needs_2nd <- vaccine_needs_second_dose() %>%
      filter(CountyServed %in% c(input$vaccineCounty) &
               HowSoon == "7 days") %>%
      arrange(NextDoseNeededDate, HouseholdID) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      select(
        "Client ID" = PersonalID,
        "County" = CountyServed,
        "Vaccine Manufacturer" = COVID19VaccineManufacturer,
        "Age Range" = AgeAtEntry,
        "Veteran Status" = VeteranStatus,
        "Date Next Dose Needed" = NextDoseNeededDate,
        "Current Location and Client Contact Info" = CurrentLocation
      )
    
    datatable(
      needs_2nd,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    ) 
  })
  
  output$vaccineSecondDoseNextWeek <- DT::renderDataTable({
    
    needs_2nd <- vaccine_needs_second_dose() %>%
      filter(CountyServed %in% c(input$vaccineCounty) &
               HowSoon == "Next Week") %>%
      arrange(NextDoseNeededDate, HouseholdID) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      select(
        "Client ID" = PersonalID,
        "County" = CountyServed,
        "Vaccine Manufacturer" = COVID19VaccineManufacturer,
        "Age Range" = AgeAtEntry,
        "Veteran Status" = VeteranStatus,
        "Date Next Dose Needed" = NextDoseNeededDate,
        "Current Location and Client Contact Info" = CurrentLocation
      )
    
    datatable(
      needs_2nd,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    ) 
  })
  
  output$utilizationDetail <-
    DT::renderDataTable({
      ReportStart <-
        format(floor_date(ymd(input$utilizationDate),
                          unit = "month"), "%m-%d-%Y")
      ReportEnd <-
        format(floor_date(ymd(input$utilizationDate) + days(31),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      z <-
        paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
      # input <- list(providerListUtilization = sample(c(sort(utilization_bed()$ProjectName)), 1))
      a <- utilizers_clients() %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate),
               PersonalID = as.character(PersonalID)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", z)
      
      datatable(a,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'))
      
    })
  
  output$utilizationSummary0 <-
    renderInfoBox({
      ReportStart <-
        format(floor_date(ymd(input$utilizationDate),
                          unit = "month"), "%m-%d-%Y")
      ReportEnd <-
        format(floor_date(ymd(input$utilizationDate) + days(31),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      a <- utilizers_clients() %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- Beds() %>%
        filter(ProjectName == input$providerListUtilization &
                 beds_available_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectID) %>%
        summarise(BedCount = sum(BedInventory)) %>%
        ungroup() %>%
        pull(BedCount)
      
      daysInMonth <- days_in_month(ymd(input$utilizationDate))
      
      infoBox(
        title = "Total Bed Nights Served",
        color = "purple",
        icon = icon("bed"),
        value = sum(a$BNs),
        subtitle = "See table below for detail."
      )
    })
  
  output$utilizationSummary1 <-
    renderInfoBox({
      ReportStart <-
        format(floor_date(ymd(input$utilizationDate),
                          unit = "month"), "%m-%d-%Y")
      ReportEnd <-
        format(floor_date(ymd(input$utilizationDate) + days(31),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      a <- utilizers_clients() %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- Beds() %>%
        filter(ProjectName == input$providerListUtilization &
                 beds_available_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectID) %>%
        summarise(BedCount = sum(BedInventory)) %>%
        ungroup() %>%
        pull(BedCount)
      
      # units <- Utilization %>%
      #   filter(ProjectName == input$providerListUtilization) %>%
      #   select(UnitCount)
      
      daysInMonth <- days_in_month(ymd(input$utilizationDate))
      
      infoBox(
        title = "Possible Bed Nights",
        color = "purple",
        icon = icon("bed"),
        value = beds * daysInMonth,
        subtitle = paste(
          "Bed Count:",
          beds,
          "beds ×",
          daysInMonth,
          "days in",
          format(ymd(input$utilizationDate), "%B"),
          "=",
          beds * daysInMonth
        )
      )
    })
  
  output$utilizationSummary2 <-
    renderInfoBox({
      ReportStart <-
        format(floor_date(ymd(input$utilizationDate),
                          unit = "month"), "%m-%d-%Y")
      ReportEnd <-
        format(floor_date(ymd(input$utilizationDate) + days(31),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      a <- utilizers_clients() %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- Beds() %>%
        filter(ProjectName == input$providerListUtilization &
                 beds_available_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectID) %>%
        summarise(BedCount = sum(BedInventory)) %>%
        ungroup() %>%
        pull(BedCount)
      
      daysInMonth <-
        as.numeric(days_in_month(ymd(input$utilizationDate)))
      
      bedUtilization <- percent(sum(a$BNs) / (beds * daysInMonth))
      
      infoBox(
        title = "Bed Utilization",
        color = "teal",
        icon = icon("bed"),
        value = bedUtilization,
        subtitle = paste(sum(a$BNs),
                         "÷",
                         beds * daysInMonth,
                         "=",
                         bedUtilization)
      )
    })
  
  output$dq_provider_summary_table <- DT::renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    guidance <- dq_main() %>%
      filter(ProjectName %in% c(input$providerListDQ) &
               served_between(., ReportStart, ReportEnd)) %>%
      group_by(Type, Issue, Guidance) %>%
      ungroup() %>%
      select(Type, Issue, Guidance) %>%
      mutate(Type = factor(Type, levels = c("High Priority",
                                            "Error",
                                            "Warning"))) %>%
      arrange(Type) %>%
      unique()

      datatable(guidance, 
              rownames = FALSE,
              escape = FALSE,
              options = list(dom = 't',
                             paging = FALSE))
  })

  output$dq_unsheltered_summary_table <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    dq_unsheltered() %>%
      filter(DefaultProvider == input$unshDefaultProvidersList &
               served_between(., ReportStart, ReportEnd)) %>%
      group_by(Type, Issue, Guidance) %>%
      ungroup() %>%
      select(Type, Issue, Guidance) %>%
      arrange(Type) %>%
      unique()
  })
  
  output$dq_unsheltered_summary_box <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    x <- dq_unsheltered() %>%
      filter(DefaultProvider == input$unshDefaultProvidersList &
               served_between(., ReportStart, ReportEnd))
    if (nrow(x) > 0) {
      box(
        id = "DQSummaryUnsheltered",
        title = "Data Quality Guidance",
        status = "info",
        solidHeader = TRUE,
        tableOutput("dq_unsheltered_summary_table"),
        width = 12
      )
    }
    else {
      
    }
  })
  
  output$dq_region_summary_table <- DT::renderDataTable({
    ReportStart <- format.Date(input$dq_region_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    a <- dq_main() %>%
      filter(ProjectRegion == input$regionList3 &
               served_between(., ReportStart, ReportEnd)) %>%
      select(ProjectName, Type, Issue, PersonalID)
    
    b <- dq_unsheltered() %>%
      filter(UserRegion == input$regionList3 &
               served_between(., ReportStart, ReportEnd)) %>%
      mutate(ProjectName = paste("Unsheltered Provider, entered by a user from", 
                                 DefaultProvider))%>%
      select(ProjectName, Type, Issue, PersonalID)
    
    c <- rbind(a, b) %>%
      group_by(ProjectName, Type, Issue) %>%
      summarise(Clients = n()) %>%
      select("Provider Name" = ProjectName, Type, Issue, Clients) %>%
      arrange(Type, desc(Clients))
    
    datatable(c, 
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
  })
  
  output$DuplicateEEs <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    DuplicateEEs <- dq_main() %>%
      filter(
        Issue == "Duplicate Entry Exits" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate
      )
    DuplicateEEs
  })
  
  output$DQDuplicateEEs <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    DuplicateEEs <- dq_main() %>%
      filter(
        Issue == "Duplicate Entry Exits" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate
      )
    if (nrow(DuplicateEEs) > 0) {
      box(
        id = "dup_ees",
        title = "Duplicate Entry Exits",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "Please correct this issue before moving on to your other errors.<br>
         Duplicate Entry Exits are created when the user clicks \"Add Entry Exit\"
         instead of clicking the Entry pencil to get back into an assessment.
         These must be deleted for each member of the household. Please take
         care to not delete Entry Exits with valid Interims attached."
        ),
        tableOutput("DuplicateEEs")
      )
    }
    else {
      
    }
  })
  
  output$HouseholdIssues <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    HHIssues <- dq_main() %>%
      filter(
        Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household"
        ) &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      arrange(PersonalID) %>%
      select("A Client ID in the Household" = PersonalID,
             Issue,
             "Entry Date" = EntryDate)
    
    HHIssues
  })
  
  output$DQHHIssues <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    HHIssues <- dq_main() %>%
      filter(
        Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household"
        ) &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      )
    if (nrow(HHIssues) > 0) {
      box(
        id = "hhs",
        title = "Household Issues",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "Please correct your Household Issues before moving on to make other
          Data Quality corrections."
        ),
        tableOutput("HouseholdIssues")
      )
    }
    else {
      
    }
  })
  
  output$DQMissingLocation <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    HHIssues <- dq_main() %>%
      filter(
        Issue == "Missing Client Location" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      )
    if (nrow(HHIssues) > 0) {
      box(
        id = "location",
        title = "Missing Client Location",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "Households with a missing Client Location (the data element just 
          after the Relationship to Head of Household) will be completely
          excluded from ALL HUD reporting."
        ),
        tableOutput("ClientLocation")
      )
    }
    else {
      
    }
  })
  
  output$ClientLocation <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    HHIssues <- dq_main() %>%
      filter(
        Issue == "Missing Client Location" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      arrange(PersonalID) %>%
      select("Client ID" = PersonalID,
             "Entry Date" = EntryDate)
    
    HHIssues
  })
  
  output$DQPATHMissingContact <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    no_contact <- dq_main() %>%
      filter(
        Issue == "Missing PATH Contact" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      )
    if (nrow(no_contact) > 0) {
      box(
        id = "location",
        title = "Missing Contact (PATH)",
        status = "warning",
        solidHeader = TRUE,
        dq_main() %>%
          filter(Issue == "Missing PATH Contact") %>%
          select(Guidance) %>%
          unique(),
        tableOutput("MissingPATHContact")
      )
    }
    else {
      
    }
  })
  
  output$MissingPATHContact <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    x <- dq_main() %>%
      filter(
        Issue == "Missing PATH Contact" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      arrange(PersonalID) %>%
      select("Client ID" = PersonalID,
             "Entry Date" = EntryDate)
    
    x
  })
  
  output$APs_with_EEs <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    APs_w_EEs <- dq_main() %>%
      filter(
        Issue == "Access Point with Entry Exits" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y")
      ) %>%
      arrange(PersonalID) %>%
      select("Client ID" = PersonalID,
             "Entry Date" = EntryDate)
    
    APs_w_EEs
  })
  
  output$DQ_APs_w_EEs <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    APs_w_EEs <- dq_main() %>%
      filter(
        Issue == "Access Point with Entry Exits" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      )
    if (nrow(APs_w_EEs) > 0) {
      box(
        id = "ees_on_ap",
        title = "Access Points Do Not Create Entry Exits",
        status = "danger",
        solidHeader = TRUE,
        HTML(
          "Please consult the Coordinated Entry workflow. Access Point providers
          should not have any Entry Exits. These Entry Exits should be deleted."
        ),
        tableOutput("APs_with_EEs")
      )
    }
    else {
      
    }
  })
  
  output$Overlaps <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    OverlappingEEs <- dq_overlaps() %>%
      filter(
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Move In Date" = MoveInDateAdjust,
        "Exit Date" = ExitDate,
        "Overlaps With This Provider's Stay" = PreviousProject
      )
    OverlappingEEs
  })
  
  output$DQOverlappingEEs <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    # FIXME Repetition of filter/mutate (use eventReactive)
    OverlappingEEs <- dq_overlaps() %>%
      filter(
        Issue == "Overlapping Project Stays" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        ProjectName,
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate
      )
    if (nrow(OverlappingEEs) > 0) {
      box(
        id = "overlappers",
        title = "Overlapping Entry Exits",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        HTML(
          "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor
        can they have a Move-In Date into a PSH or RRH project while they are
        still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's
        or any two PSH's simultaneously, housed or not.<br>
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the \"Previous 
        Provider's\" mistake, but if you are seeing clients here, it means your
        project stay was entered last. <br>
        If the overlap is not your project's mistake, please work with the project 
        that has the incorrect Entry/Move-In/or Exit Date to get this corrected 
        or send an email to hmis@cohhio.org if you cannot get it resolved. These 
        clients will NOT show on their Data Quality app. <br>
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
        ),
        tableOutput("Overlaps")
      )
    }
    else {
      
    }
  })
  
  output$DQAPsNoReferrals <- renderUI({
    AP_not_doing_referrals <- aps_no_referrals() %>%
      filter(ProviderCreating %in% c(input$providerListDQ))
    
    if (nrow(AP_not_doing_referrals) > 0) {
      box(
        id = "noreferrals",
        title = "Access Point Has No Outgoing Referrals",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        HTML(
          "Access Points should be creating Referrals in HMIS so that households
          can be connected to housing. Please 
<a href=\"http://hmis.cohhio.org/index.php?pg=kb.page&id=186\">click here</a>
 for more information."
        )
      )
    }
    else {
      
    }
  })
  
  output$cocAPsNoReferralsList <-
    DT::renderDataTable({
      a <- aps_no_referrals() %>% arrange(ProviderCreating)
      
      datatable(a, rownames = FALSE)
    })
  
  output$cocOverlap <- DT::renderDataTable({
    ReportStart <- format.Date(hc_check_dq_back_to, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    a <- dq_overlaps() %>%
      filter(served_between(., ReportStart, ReportEnd)) %>%
      group_by(ProjectName) %>%
      summarise(Clients = n()) %>%
      arrange(desc(Clients)) %>%
      top_n(20L, wt = Clients) %>%
      select("Project Name" = ProjectName,
             "Clients with Overlapping Entry Exits" = Clients)
    datatable(a,
              rownames = FALSE)
  })
  
  output$cocUnshelteredEntriesByMonth <- renderPlotly({
    ReportStart <-  format.Date(input$unshEntriesByMonth_ReportStart, "%m-%d-%Y")
    ReportEnd <-  format.Date(meta_HUDCSV_Export_Date, "%m-%d-%Y")
    
    monthyears <- unsheltered_by_month() %>%
      arrange(EntryDate) %>%
      pull(EntryDateDisplay) %>%
      unique()   
    
    unsheltered_by_month <- unsheltered_by_month() %>%
      filter(entered_between(., ReportStart, ReportEnd),
             County %in% c(input$unshEntriesByMonth_County)) %>%
      select(HouseholdID, EntryDateDisplay, County) %>%
      group_by(EntryDateDisplay, County) %>%
      summarise(Entries = n()) %>%
      ungroup() %>%
      mutate(
        Entries = as.numeric(Entries),
        EntryDateDisplay = factor(EntryDateDisplay, levels = c(monthyears)),
        County = factor(County)
      ) %>%
      pivot_wider(names_from = County, values_from = Entries)
    
    unsheltered_by_month[is.na(unsheltered_by_month)] <- 0
    
    unsheltered_by_month <- unsheltered_by_month %>%
      pivot_longer(cols = !all_of("EntryDateDisplay"), 
                   names_to = "County", 
                   values_to = "Entries") %>%
      mutate(hover = paste(County, 
                           "County:\n", 
                           Entries,
                           "clients entered during",
                           EntryDateDisplay))
    
    plot_ly(unsheltered_by_month %>% 
              arrange(EntryDateDisplay, County) %>%
              group_by(County), 
            x = ~EntryDateDisplay, 
            y = ~Entries, 
            type = 'scatter', 
            mode = 'lines', 
            text = ~hover,
            hoverinfo = 'text',
            color = ~County,
            colors = colorRampPalette(c("black",
                                        "purple",
                                        "blue",
                                        "green"))(80)
    ) %>%
      layout(xaxis = list(title = "Month/Year"))
  })
  
  output$cocLongStayers <- DT::renderDataTable({
    ReportStart <- format.Date(hc_check_dq_back_to, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    a <- dq_main() %>%
      filter(served_between(., ReportStart, ReportEnd) &
               Issue == "Extremely Long Stayer") %>%
      group_by(ProjectName) %>%
      summarise(Clients = n()) %>%
      arrange(desc(Clients)) %>%
      top_n(20L, wt = Clients) %>%
      select("Project Name" = ProjectName,
             "Extremely Long Stayers" = Clients)
    datatable(a,
              rownames = FALSE)
    
  })
  
  output$cocRRHDestination <- DT::renderDataTable({
    ReportStart <- format.Date(hc_check_dq_back_to, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    a <- dq_main() %>%
      filter(served_between(., ReportStart, ReportEnd) &
               Issue %in% c(
               "Incorrect Exit Destination (should be \"Rental by client, with RRH...\")",
               "Missing RRH Project Stay or Incorrect Destination")) %>%
      group_by(ProjectName, Issue) %>%
      summarise(Clients = n()) %>%
      arrange(desc(Clients)) %>%
      select("Project Name" = ProjectName,
             Issue,
             Clients)
    
    datatable(head(a, 20),
              rownames = FALSE)
    
  })
  
  output$cocWidespreadIssues <- DT::renderDataTable({
    a <- dq_past_year() %>%
      select(Issue, ProjectName, Type) %>%
      unique() %>%
      group_by(Issue, Type) %>%
      summarise(HowManyProjects = n()) %>%
      arrange(desc(HowManyProjects)) %>%
      head(10L) %>%
      select(Issue, Type, "How Many Providers" = HowManyProjects)
    
    datatable(a,
              rownames = FALSE)
  })
 
  # purrr::walk(gg_nms, ~{
  #   output[[.x]] <<- renderImage({
  #     # Return a list containing the filename and alt text
  #     list(src = get0(.x), width = "100%", height = "auto")
  #   }, deleteFile = FALSE)
  # })
  
  
  output$cocAPsNoReferrals <- renderPlot({
    
    ggplot(data_APs(), aes(fill = category, x = providertype, y = percent)) +
      geom_bar(position = "fill",
                stat = "identity",
               width = .1) +
      geom_label(
        aes(label = paste(
          category,
          "\n",
          prettypercent
        )),
        position = position_stack(),
        vjust = 2,
        fill = "white",
        colour = "black",
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("#00952e", "#a11207"),
                        guide = FALSE) +
      theme_void()
  })
  
  output$Ineligible <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    Ineligible <- detail_eligibility() %>%
      filter(ProjectName %in% c(input$providerListDQ) &
               served_between(., ReportStart, ReportEnd)) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        PreviousStreetESSH = if_else(PreviousStreetESSH == 1, "Yes", "No")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Residence Prior" = ResidencePrior,
        "Length of Stay" = LengthOfStay,
        "Literally Homeless Prior" = PreviousStreetESSH
      )
    Ineligible
  })
  
  output$DQIneligible <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    Ineligible <- detail_eligibility() %>%
      filter(ProjectName %in% c(input$providerListDQ) &
               served_between(., ReportStart, ReportEnd))
    
    if (nrow(Ineligible) > 0) {
      box(
        id = "eligibility",
        title = "Check Eligibility",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        HTML(
          "<p>Your Residence Prior data suggests that this project is either serving
          ineligible households, the household was entered into the wrong project,
          or the Residence Prior data at Entry is incorrect. Please check the
          terms of your grant or speak with the CoC team at COHHIO if you are
          unsure of eligibility criteria for your project type."
        ),
        tableOutput("Ineligible")
      )
    }
    else {
      
    }
  })
  
  output$DQIncorrectEETypeTable <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    EEType <- dq_main() %>%
      filter(
        Issue == "Incorrect Entry Exit Type" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate
      )
    EEType
  }) 
  
  output$DQIncorrectEEType <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    
    EEType <- dq_main() %>%
      filter(
        Issue == "Incorrect Entry Exit Type" &
          ProjectName %in% c(input$providerListDQ) &
          served_between(., ReportStart, ReportEnd)
      ) 
    
    if (nrow(EEType) > 0) {
      box(
        id = "DQEEType",
        title = "Incorrect Entry Exit Type",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "If you are not sure which Entry Exit Type you should be using for 
          your provider, please contact the HMIS team."
        ),
        tableOutput("DQIncorrectEETypeTable")
      )
    }
    else {
      
    }
  })
  
  output$DQErrors <- DT::renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(meta_HUDCSV_Export_Date, "%m-%d-%Y")
    
    DQErrors <- dq_main() %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits",
          "Access Point with Entry Exits"
        ) & # because these are all in the boxes already
          served_between(., ReportStart, ReportEnd) &
          ProjectName %in% c(input$providerListDQ) &
          Type == "Error"
      ) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      arrange(HouseholdID, PersonalID) %>%
      select("Client ID" = PersonalID,
             "Error" = Issue,
             "Entry Date" =  EntryDate)
    
    datatable(
      DQErrors,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    )
  })
  
  output$veteranActiveListEligibilityLegend<-
    renderUI(
      HTML(
        paste0("<p>", pass_icon, " = Veteran eligible for all VA homeless services</p>",
                  "<p>", fail_icon, " = Veteran not eligible for VA services</p>",
                  "<p>", alert_icon, " = Veteran eligible for SSVF/GPD only</p>",
                  "<p>", unknown_icon, " = VA eligibility unknown</p>"
        )
      )
    )
  
  output$VeteranActiveList <- DT::renderDataTable({

    vet_active_list <- veteran_active_list() %>%
      filter(County %in% c(input$vetCounty)) %>%
      arrange(PersonalID) %>%
      mutate(PersonalID = if_else(
        is.na(HOMESID),
        as.character(PersonalID),
        paste(PersonalID,
              "<br>HOMES:",
              HOMESID)
      ),
      HousingPlan = case_when(
        ExpectedPHDate < today() ~ paste0(
          "<span style='color:tomato;'>", HousingPlan, "</span>"),
        ExpectedPHDate >= today() ~ paste0(
          "<span style='color:seagreen;'>", HousingPlan, "</span>"),
        TRUE ~ HousingPlan),
      VAEligibilityIcon = paste(
        case_when(
          VAEligible == "Veteran eligible for all VA homeless services" ~ pass_icon,
          VAEligible == "Veteran not eligible for VA services" ~ fail_icon,
          VAEligible == "Veteran eligible for SSVF/GPD only" ~ alert_icon,
          VAEligible == "VA eligibility unknown" |
            is.na(VAEligible) ~ unknown_icon
        ),
        case_when(
          !is.na(SSVFIneligible) &
            SSVFIneligible != "NA" ~ paste("<br><br>", SSVFIneligible),
          TRUE ~ ""
        )
      )) %>%
      group_by(PersonalID) %>%
      mutate(
        Enrollments = paste0(
          if_else(
            !is.na(PH_ProjectName), paste0(
              "<span style='background-color:lavenderblush;'>", PH_ProjectName, "<br>", PH_TimeInProject), "", "</span>"), 
          if_else(
            !is.na(PH_ProjectName) & 
              (!is.na(LH_ProjectName) | !is.na(O_ProjectName)), "<br><br>", ""), 
          if_else(
            !is.na(LH_ProjectName), paste0(
              "<span style='background-color:lightgoldenrodyellow;'>", LH_ProjectName, "<br>", LH_TimeInProject), "", "</span>"),
          if_else(
            !is.na(LH_ProjectName) & !is.na(O_ProjectName), "<br><br>", ""), 
          if_else(
            !is.na(O_ProjectName), paste0(
              "<span style='background-color:paleturquoise;'>", O_ProjectName, "<br>", O_TimeInProject), "", "</span>")
        )
      ) %>%
      select(
        "SSVF Responsible Provider" = SSVFServiceArea,
        "Client ID" = PersonalID,
        "Active Date" =  ActiveDateDisplay,
        "Enrollments" = Enrollments,
        "Eligibility" = VAEligibilityIcon,
        "Most Recent Offer" = MostRecentOffer,
        "List Status" = ListStatus, 
        "Housing Track & Notes" = HousingPlan
      )
    
    datatable(
      vet_active_list,
      rownames = FALSE,
      escape = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi', 
                     initComplete = JS(
                       "function(settings, json) {",
                       "$('th').css({'text-align': 'center'});",
                       "$('td').css({'text-align': 'center'});",
                       "}"))
      )
    # %>% formatStyle(
    #   "Housing Track & Notes", "DatePast",
    #   color = styleEqual(c(0, 1), c('black', 'darkred'))
     # %>% formatStyle("Eligibility", textAlign = "center")
    
  })
  
  output$downloadVeteranActiveList <- downloadHandler(
    filename = function() {
      "veteran_active_list.xlsx"
    },
    content = function(file) {
      write_xlsx(veteran_active_list() %>%
                   filter(
                     County %in% c(input$vetCounty) |
                       is.na(County)
                   ) %>%
                   mutate(ProjectType = project_type(ProjectType),
                          LivingSituation = living_situation(LivingSituation),
                          Destination = living_situation(Destination),
                          VeteranStatus = enhanced_yes_no_translator(VeteranStatus),
                          DisablingCondition = enhanced_yes_no_translator(DisablingCondition)) %>%
                   select(
                     SSVFServiceArea,
                     County,
                     PersonalID, 
                     HOMESID,
                     ProjectType,
                     ProjectName,
                     DateVeteranIdentified,
                     EntryDate,
                     MoveInDateAdjust,
                     ExitDate,
                     ListStatus,
                     VAEligible,
                     SSVFIneligible,
                     DisablingCondition,
                     VAMCStation,
                     PHTrack,
                     ExpectedPHDate,
                     Destination,
                     OtherDestination,
                     ClientLocation,
                     AgeAtEntry,
                     VeteranStatus,
                     HouseholdSize,
                     Notes,
                     ActiveDate,
                     DaysActive,
                     Eligibility,
                     MostRecentOffer,
                     HousingPlan
                   ) %>% unique(), path = file)
    }
  )
  
  output$DQWarnings <- DT::renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(meta_HUDCSV_Export_Date, "%m-%d-%Y")
    
    DQWarnings <- dq_main() %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits",
          "Check Eligibility"
        ) &
          served_between(., ReportStart, ReportEnd) &
          ProjectName %in% c(input$providerListDQ) &
          Type == "Warning"
      ) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      arrange(HouseholdID, PersonalID) %>%
      select(
        "Client ID" = PersonalID,
        "Warning" = Issue,
        "Entry Date" =  EntryDate
      )
    
    datatable(
      DQWarnings,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi'))
  })
  
  output$cocDQErrors <- renderPlot(dq_plot_projects_errors)
  output$cocHHErrors <- renderPlot(dq_plot_hh_errors)
  output$cocUnshelteredHigh <- renderPlot(dq_plot_unsheltered_high)
  output$cocDQWarnings <- renderPlot(dq_plot_projects_warnings)
  output$cocDQErrorTypes <- renderPlot(dq_plot_errors)
  output$cocDQWarningTypes <- renderPlot(dq_plot_warnings)
  output$cocEligibility <- renderPlot(dq_plot_eligibility)
  output$dq_plot_hh_no_spdat <- renderPlot(dq_plot_hh_no_spdat)
  output$dq_plot_outstanding_referrals <- renderPlot(dq_plot_outstanding_referrals)

  output$unshIncorrectResPriorTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    ResPrior <- dq_unsheltered() %>%
      filter(
        Issue == "Wrong Provider (Not Unsheltered)" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate
      )
    ResPrior
  }) 
   
  output$unshIncorrectResPrior <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    ResPrior <- dq_unsheltered() %>%
      filter(
        Issue == "Wrong Provider (Not Unsheltered)" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate
      )
    if (nrow(ResPrior) > 0) {
      box(
        id = "unshResPrior",
        title = "Incorrect Residence Prior or Not Unsheltered",
        status = "danger",
        solidHeader = TRUE,
        HTML(
          "Only clients who are in a place not meant for habitation should be 
          entered into the Unsheltered provider. If the client(s) here were 
          incorrectly entered into the Unsheltered provider, their Entry Exit 
          should be deleted. <p>Please review the 
          <a href=\"https://www.youtube.com/watch?v=qdmrqOHXoN0&t=174s\" 
          target=\"_blank\">data entry portion of the Unsheltered video training</a>
          for more info."
        ),
        tableOutput("unshIncorrectResPriorTable")
      )
    }
    else {
      
    }
  })
  
  output$unshIncorrectEETypeTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    EEType <- dq_unsheltered() %>%
      filter(
        Issue == "Incorrect Entry Exit Type" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate
      )
    EEType
  }) 
  
  output$unshIncorrectEEType <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    EEType <- dq_unsheltered() %>%
      filter(
        Issue == "Incorrect Entry Exit Type" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate
      )
    if (nrow(EEType) > 0) {
      box(
        id = "unshEEType",
        title = "Incorrect Entry Exit Type",
        status = "danger",
        solidHeader = TRUE,
        HTML(
          "All households entered into the Unsheltered provider should have
          an Entry Exit Type of \"Standard\""
        ),
        tableOutput("unshIncorrectEETypeTable")
      )
    }
    else {
      
    }
  })
  
  output$unshDuplicateEEsTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    DuplicateEEs <- dq_unsheltered() %>%
      filter(
        Issue == "Duplicate Entry Exits" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate
      ) %>% unique()
    DuplicateEEs
  })
  
  output$unshDuplicateEEs <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    DuplicateEEs <- dq_unsheltered() %>%
      filter(
        Issue == "Duplicate Entry Exits" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      unique()
    
    if (nrow(DuplicateEEs) > 0) {
      box(
        id = "dup_ees",
        title = "Duplicate Entry Exits",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "Please correct this issue before moving on to your other errors.<br>
         Duplicate Entry Exits are created when the user clicks \"Add Entry Exit\"
         instead of clicking the Entry pencil to get back into an assessment.
         These must be deleted for each member of the household. Please take
         care to not delete Entry Exits with valid Interims attached."
        ),
        tableOutput("unshDuplicateEEsTable")
      )
    }
    else {
      
    }
  })
  
  output$unshHHIssuesTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    HHIssues <- dq_unsheltered() %>%
      filter(
        Issue %in% c("Too Many Heads of Household", 
                     "Missing Relationship to Head of Household",
                     "Children Only Household", 
                     "No Head of Household") &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        Issue
      ) %>% unique()
    HHIssues
  })
  
  output$unshHHIssues <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    HHIssues <- dq_unsheltered() %>%
      filter(
        Issue %in% c("Too Many Heads of Household", 
                     "Missing Relationship to Head of Household",
                     "Children Only Household", 
                     "No Head of Household") &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        Issue
      ) %>% unique()
    
    if (nrow(HHIssues) > 0) {
      box(
        id = "unshhhs",
        title = "Household Issues",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "Please correct your household issues before moving on to your other 
          errors."
        ),
        tableOutput("unshHHIssuesTable")
      )
    }
    else {
      
    }
  })
  
  output$unshMissingCountyTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    county <- dq_unsheltered() %>%
      filter(
        Issue == "Missing County Served" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate
      ) %>% unique()
    county
  })
  
  output$unshMissingCounty <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    county <- dq_unsheltered() %>%
      filter(
        Issue == "Missing County Served" &
          DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate
      ) %>% unique()
    
    if (nrow(county) > 0) {
      box(
        id = "unshcounty",
        title = "Missing County",
        status = "danger",
        solidHeader = TRUE,
        HTML(
          "When a client is entered into the Unsheltered Provider with no County,
          housing providers cannot tell where they are to know if they can help
          get them housed. This field is essential to everyone in the Balance of
          State CoC trying to prioritize its clients."
        ),
        tableOutput("unshMissingCountyTable")
      )
    }
    else {
      
    }
  })
  
  output$unshOverlapsTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    
    overlaps <- unsh_overlaps() %>%
      filter(DefaultProvider == input$unshDefaultProvidersList &
          served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Overlaps With This Provider's Stay" = PreviousProject
      ) %>% 
      unique()
    overlaps
  })
  
  output$unshOverlaps <- renderUI({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(ymd(meta_HUDCSV_Export_End), "%m-%d-%Y")
    
    overlaps <- unsh_overlaps() %>%
      filter(DefaultProvider == input$unshDefaultProvidersList &
               served_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(
        PersonalID = format(PersonalID, digits = NULL),
        EntryDate = format(EntryDate, "%m-%d-%Y"),
        ExitDate = format(ExitDate, "%m-%d-%Y")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Overlaps With This Provider's Stay" = PreviousProject
      ) %>% unique()
    
    if (nrow(overlaps) > 0) {
      box(
        id = "overlaps_unsh",
        title = "Overlapping Entry Exits",
        status = "warning",
        solidHeader = TRUE,
        HTML(
          "A client cannot be unsheltered and reside in an ES, TH, or Safe Haven 
          at the same time. Nor can they have a Move-In Date into a PSH or RRH 
          project while they are unsheltered. <br>
          Please look the client(s) up in HMIS and determine which project stay's
          Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may not be your
          project's mistake, but if you are seeing clients here, it means your
          project stay was entered last. <br>
          If the overlap is not your project's mistake, please work with the 
          project that has the incorrect Entry/Move-In/or Exit Date to get this 
          corrected or send an email to hmis@cohhio.org if you cannot get it 
          resolved. These clients will NOT show on their Data Quality app. <br>
          If YOUR dates are definitely correct, it is fine to continue with other
          data corrections as needed."
        ), 
        tableOutput("unshOverlapsTable")
      )
    }
    else {
      
    }
  })
  
  output$unshDQErrorsTable <- DT::renderDataTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(meta_HUDCSV_Export_Date, "%m-%d-%Y")
    
    unshDQErrors <- dq_unsheltered() %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Missing County Served",
          "Missing County of Prior Residence",
          "Duplicate Entry Exits",
          "Wrong Provider (Not Unsheltered)",
          "Incorrect Entry Exit Type"
        ) &
          served_between(., ReportStart, ReportEnd) &
          DefaultProvider == input$unshDefaultProvidersList &
          Type == "Error"
      ) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      arrange(HouseholdID, PersonalID) %>%
      select("Client ID" = PersonalID,
             "Error" = Issue,
             "Entry Date" =  EntryDate)
    
    datatable(
      unshDQErrors,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi'))
  })
  
  output$unshDQWarningsTable <- DT::renderDataTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(meta_HUDCSV_Export_Date, "%m-%d-%Y")
    
    unshDQWarnings <- dq_unsheltered() %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits",
          "Check Eligibility"
        ) &
          served_between(., ReportStart, ReportEnd) &
          DefaultProvider == input$unshDefaultProvidersList &
          Type == "Warning"
      ) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      arrange(HouseholdID, PersonalID) %>%
      select(
        "Client ID" = PersonalID,
        "Warning" = Issue,
        "Entry Date" =  EntryDate
      )
    
    datatable(
      unshDQWarnings,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi'))
    
  })
  
  mod_QPR_server("spdat1", "Community Need, Entered Permanent Housing")
  
  mod_QPR_server("spdat2", "Community Need, Literally Homeless in the County")
  
  mod_QPR_server("LoS", "Length of Stay")
  
  # Placeholder for Exits to PH
  
  mod_QPR_server("NCB", "Non-Cash Benefits at Exit")
  
  mod_QPR_server("HI", "Health Insurance at Exit")
  
  mod_QPR_server("income", "Income Growth")
  
  mod_QPR_server("rapid", "Average Days to House")
  
  
  output$ExitsToPH <- DT::renderDataTable({
    ReportStart <- format.Date(input$ExitsToPHDateRange[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$ExitsToPHDateRange[2], "%m-%d-%Y")
    
    SuccessfullyPlaced <- qpr_leavers() %>%
      filter(((ProjectType %in% c(3, 9, 13) &
                 !is.na(MoveInDateAdjust)) |
                ProjectType %in% c(1, 2, 4, 8, 12)
      ) &
        # excluding non-mover-inners
        (((DestinationGroup == "Permanent" |
             #exited to ph or still in PSH/HP
             is.na(ExitDate)) &
            ProjectType %in% c(3, 9, 12) &
            served_between(., ReportStart, ReportEnd)# PSH & HP
        ) |
          (
            DestinationGroup == "Permanent" & # exited to ph
              ProjectType %in% c(1, 2, 4, 8, 13) &
              exited_between(., ReportStart, ReportEnd)
          )
        )) # ES, TH, SH, RRH, OUT) %>%
    
    # calculating the total households to compare successful placements to
    TotalHHsSuccessfulPlacement <- qpr_leavers() %>%
      filter((
        served_between(., ReportStart, ReportEnd) &
          ProjectType %in% c(3, 9, 12) # PSH & HP
      ) |
        (
          exited_between(., ReportStart, ReportEnd) &
            ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
        )) # For PSH & HP, it's total hhs served;
    # otherwise, it's total hhs *exited* during the reporting period
    
    SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
      left_join(
        SuccessfullyPlaced,
        by = c(
          "EnrollmentID",
          "ProjectType",
          "ProjectName",
          "PersonalID",
          "EntryDate",
          "MoveInDate",
          "MoveInDateAdjust",
          "ExitDate",
          "DestinationGroup",
          "Destination",
          "HouseholdID"
        )
      ) %>%
      filter(ProjectName == input$ExitsToPHProjectList) %>%
      mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                MoveInDate, EntryDate),
             PersonalID = as.character(PersonalID)) %>%
      arrange(DestinationGroup, PersonalID) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Bed Start" = BedStart,
        "Exit Date" = ExitDate,
        "Destination Group" =  DestinationGroup
      )
    
    datatable(SuccessfulPlacement,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
    
  })
  
  
  
  
  
  
  #  QPR HP vs RRH Spending
  output$RRHSpending <-
    DT::renderDataTable({
      ReportStart <- format.Date(input$RRHSpendingDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$RRHSpendingDateRange[2], "%m-%d-%Y")
      
      rrhSpending <- qpr_spending() %>%
        filter(
          OrganizationName == input$RRHSpendingOrganizationList &
            entered_between(., ReportStart, ReportEnd) &
            ProjectType == 13
        ) %>%
        mutate(ProjectName = as.factor(ProjectName),
               PersonalID = as.character(PersonalID)) %>%
        select(
          "Client ID" = PersonalID,
          "RRH Project Name" = ProjectName,
          "Service Date" = ServiceStartDate,
          Description,
          Amount
        )
      
      datatable(rrhSpending,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi')) %>%
        formatCurrency("Amount")
      
    
    })
  
  output$HPSpending <-
    DT::renderDataTable({
      ReportStart <- format.Date(input$RRHSpendingDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$RRHSpendingDateRange[2], "%m-%d-%Y")
      
      hpSpending <- qpr_spending() %>%
        filter(
          OrganizationName == input$RRHSpendingOrganizationList &
            entered_between(., ReportStart, ReportEnd) &
            ProjectType == 12
        ) %>%
        mutate(ProjectName = as.factor(ProjectName),
               PersonalID = as.character(PersonalID)) %>%
        select(
          "Client ID" = PersonalID,
          "Prevention Project Name" = ProjectName,
          "Service Date" = ServiceStartDate,
          Description,
          Amount
        )
      
      datatable(hpSpending,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi')) %>%
        formatCurrency("Amount")
      
      
    })
  
  output$pe_ProjectSummary <-
    DT::renderDataTable({
      ptc <- summary_pe_final_scoring() %>%
        filter(AltProjectName == input$pe_provider) %>%
        pull(ProjectType)
      
      summary_pe_final_scoring <- summary_pe_final_scoring() %>%
        mutate(
          ExitsToPHMath = str_replace(ExitsToPHMath, "/", "÷"),
          # OwnHousingMath = str_replace(OwnHousingMath, "/", "÷"),
          # IncreasedIncomeMath = str_replace(IncreasedIncomeMath, "/", "÷"),
          BenefitsAtExitMath = str_replace(BenefitsAtExitMath, "/", "÷"),
          AverageLoSMath = str_replace(AverageLoSMath, "/", "÷"),
          LHResPriorMath = str_replace(LHResPriorMath, "/", "÷"),
          NoIncomeAtEntryMath = str_replace(NoIncomeAtEntryMath, "/", "÷"),
          MedianHHIMath = str_replace(MedianHHIMath, "/", "÷"),
          LongTermHomelessMath = str_replace(LongTermHomelessMath, "/", "÷"),
          ScoredAtEntryMath = str_replace(ScoredAtEntryMath, "/", "÷"),
          DQMath = str_replace(DQMath, "/", "÷"),
          PrioritizationWorkgroupMath = str_replace(PrioritizationWorkgroupMath, "/", "÷"),
          HousingFirstMath = str_replace(HousingFirstMath, "/", "÷"),
          ChronicPrioritizationMath = str_replace(ChronicPrioritizationMath, "/", "÷")
        )
      
      a <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHPoints,
          # "Moved into Own Housing" = OwnHousingPoints,
          # "Increased Income" = IncreasedIncomePoints,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPoints,
          "Average Length of Stay" = AverageLoSPoints,
          "Living Situation at Entry" = LHResPriorPoints,
          "No Income at Entry" = NoIncomeAtEntryPoints,
          "Median Homeless History Index" = MedianHHIPoints,
          "Long Term Homeless" = LongTermHomelessPoints,
          "VISPDAT Completion at Entry" = ScoredAtEntryPoints,
          "Data Quality" = DQPoints,
          "Prioritization Workgroup" = PrioritizationWorkgroupScore,
          "Housing First" = HousingFirstScore,
          "Prioritization of Chronic" = ChronicPrioritizationScore
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Estimated Score")
      
      b <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHDQ,
          # "Moved into Own Housing" = OwnHousingDQ,
          # "Increased Income" = IncreasedIncomeDQ,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitDQ,
          "Average Length of Stay" = AverageLoSDQ,
          "Living Situation at Entry" = LHResPriorDQ,
          "No Income at Entry" = NoIncomeAtEntryDQ,
          "Median Homeless History Index" = MedianHHIDQ,
          "Long Term Homeless" = LTHomelessDQ,
          "VISPDAT Completion at Entry" = ScoredAtEntryDQ,
          "Housing First" = HousingFirstDQ,
          "Prioritization of Chronic" = ChronicPrioritizationDQ
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "DQflag")
      
      c <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHPossible,
          # "Moved into Own Housing" = OwnHousingPossible,
          # "Increased Income" = IncreasedIncomePossible,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
          "Average Length of Stay" = AverageLoSPossible,
          "Living Situation at Entry" = LHResPriorPossible,
          "No Income at Entry" = NoIncomeAtEntryPossible,
          "Median Homeless History Index" = MedianHHIPossible,
          "Long Term Homeless" = LongTermHomelessPossible,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryPossible,
          "Data Quality" = DQPossible,
          "Prioritization Workgroup" = PrioritizationWorkgroupPossible,
          "Housing First" = HousingFirstPossible,
          "Prioritization of Chronic" = ChronicPrioritizationPossible
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Possible Score")
      
      d <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHMath,
          # "Moved into Own Housing" = OwnHousingMath,
          # "Increased Income" = IncreasedIncomeMath,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
          "Average Length of Stay" = AverageLoSMath,
          "Living Situation at Entry" = LHResPriorMath,
          "No Income at Entry" = NoIncomeAtEntryMath,
          "Median Homeless History Index" = MedianHHIMath,
          "Long Term Homeless" = LongTermHomelessMath,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryMath,
          "Data Quality" = DQMath,
          "Prioritization Workgroup" = PrioritizationWorkgroupMath,
          "Housing First" = HousingFirstMath,
          "Prioritization of Chronic" = ChronicPrioritizationMath
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Calculation")
      
      psh <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in% c("Moved into Own Housing",
                               "Average Length of Stay")) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      rrh <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in%
                 c("Long Term Homeless",
                   "Prioritization of Chronic",
                   "Prioritization Workgroup")) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      th <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "", # "Documents not yet received",
            DQflag == 3 ~ "", # "Docs received, not yet scored",
            DQflag == 4 ~ "", # "CoC Error",
            DQflag == 5 ~ "" # "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in% c(
          "Long Term Homeless",
          "Prioritization of Chronic",
          "Prioritization Workgroup"
        )) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      datatable(
        if (ptc == 3) {
          psh
        } else if (ptc == 13) {
          rrh
        } else if(ptc == 2) {
          th
        },
        rownames = FALSE,
        options = list(dom = 't',
                       pageLength = 100)
      )
    })
  
  output$pe_ExitsToPH <- DT::renderDataTable({
    a <- pe_exits_to_ph() %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
             Destination = living_situation(Destination)) %>%
      select("Client ID" = PersonalID,
             "Entry Date" = EntryDate,
             "Move In Date" = MoveInDateAdjust,
             "Exit Date" = ExitDate,
             Destination,
             "Destination Group" = DestinationGroup,
             "Meets Objective" = MeetsObjective)    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "PSH: Heads of Household | 
              TH, RRH: Heads of Household Leavers")
    
  })
  
  # output$pe_OwnHousing <- DT::renderDataTable({
  #   
  #   a <- pe_own_housing() %>%
  #     filter(AltProjectName == input$pe_provider) %>%
  #     mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
  #            Destination = living_situation(Destination)) %>%
  #     select(
  #       "Client ID" = PersonalID,
  #       "Entry Date" = EntryDate,
  #       "Exit Date" = ExitDate,
  #       Destination,
  #       "Destination Group" = DestinationGroup,
  #       "Meets Objective" = MeetsObjective
  #     )    
  #   
  #   datatable(a,
  #             rownames = FALSE,
  #             filter = 'top',
  #             options = list(dom = 'ltpi'),
  #             caption = "RRH, TH, SH: Heads of Household Leavers who moved into 
  #             the project's housing")
  #   
  # })
  
  output$pe_BenefitsAtExit <- DT::renderDataTable({
    a <- pe_benefits_at_exit() %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(
        BenefitsFromAnySource = case_when(
          BenefitsFromAnySource == 1 ~ "Yes", 
          BenefitsFromAnySource == 0 ~ "No",
          is.na(BenefitsFromAnySource) ~ "Missing"),
        InsuranceFromAnySource = case_when(
          InsuranceFromAnySource == 1 ~ "Yes",
          InsuranceFromAnySource == 0 ~ "No",
          is.na(InsuranceFromAnySource) ~ "Missing"
        ),
        MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Move-In Date" = MoveInDateAdjust,
        "Exit Date" = ExitDate,
        "Non-Cash Benefits at Exit" = BenefitsFromAnySource,
        "Health Insurance at Exit" = InsuranceFromAnySource,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "ALL Project Types: Adult Leavers who moved into the
              project's housing")
    
  })
  
  # output$pe_IncreasedIncome <- DT::renderDataTable({
  #   a <- pe_increase_income() %>%
  #     filter(AltProjectName == input$pe_provider) %>%
  #     mutate(
  #       IncomeDifference = IncomeMostRecent - IncomeAtEntry,
  #       MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
  #     ) %>%
  #     select(
  #       "Client ID" = PersonalID,
  #       "Entry Date" = EntryDate,
  #       "Move-In Date" = MoveInDateAdjust,
  #       "Exit Date" = ExitDate,
  #       "Income at Entry" = IncomeAtEntry,
  #       "Most Recent Income" = IncomeMostRecent,
  #       "Increase/Decrease" = IncomeDifference,
  #       "Meets Objective" = MeetsObjective
  #     )    
  #   
  #   datatable(a,
  #             rownames = FALSE,
  #             filter = 'top',
  #             options = list(dom = 'ltpi'),
  #             caption = "ALL Project Types: Adults who moved into the project's
  #             housing")
  #   
  # })
  
  output$pe_LivingSituationAtEntry <- DT::renderDataTable({
    a <- pe_res_prior() %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(
        LivingSituation = living_situation(LivingSituation),
        MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Residence Prior" = LivingSituation,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "ALL Project Types: Adults who entered the project
              during the reporting period")
    
  })
  
  output$pe_NoIncomeAtEntry <- DT::renderDataTable({
    a <- pe_entries_no_income() %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(
        MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
        IncomeFromAnySource = case_when(
          IncomeFromAnySource == 1 ~ "Yes", 
          IncomeFromAnySource == 0 ~ "No",
          IncomeFromAnySource %in% c(8, 9) ~ "Don't Know/Refused",
          IncomeFromAnySource == 99 ~ "Missing")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Income From Any Source" = IncomeFromAnySource,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "ALL Project Types: Adults who entered the project
              during the reporting period")
    
  })
  
  output$pe_LengthOfStay <- DT::renderDataTable({
    a <- pe_length_of_stay() %>%
      filter(AltProjectName == input$pe_provider &
               ProjectType %in% c(2, 8, 13)) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Move-In Date" = MoveInDateAdjust,
        "Exit Date" = ExitDate,
        "Days in Project" = DaysInProject
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "RRH, TH: Client Leavers who moved into the project's 
              housing")
    
  })
  
  output$pe_MedianHHI <- DT::renderDataTable({
    
    times <- HUD_specs() %>%
      filter(DataElement == "TimesHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    months <- HUD_specs() %>%
      filter(DataElement == "MonthsHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    a <- pe_homeless_history_index() %>%
      left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) %>%
      mutate(TimesHomelessPastThreeYears = Description) %>%
      select(-Description)
    
    b <- a %>%
      left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) %>%
      mutate(MonthsHomelessPastThreeYears = Description) %>%
      select(-Description)
    
    c <- b %>%
      filter(AltProjectName == input$pe_provider) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Approximate Date Homeless" = DateToStreetESSH,
        "Days Homeless at Entry" = DaysHomelessAtEntry,
        "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
        "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
        "Homeless Hisory Index" = HHI
      )    
    
    datatable(c,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "ALL Project Types: Adults who entered the project 
              during the reporting period")
    
  })
  
  output$pe_LongTermHomeless <- DT::renderDataTable({
    
    times <- HUD_specs() %>%
      filter(DataElement == "TimesHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    months <- HUD_specs() %>%
      filter(DataElement == "MonthsHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    a <- pe_long_term_homeless() %>%
      filter(ProjectType == 3) %>%
      left_join(times, by = c("TimesHomelessPastThreeYears" = "ReferenceNo")) %>%
      mutate(TimesHomelessPastThreeYears = Description) %>%
      select(-Description)
    
    b <- a %>%
      left_join(months, by = c("MonthsHomelessPastThreeYears" = "ReferenceNo")) %>%
      mutate(MonthsHomelessPastThreeYears = Description) %>%
      select(-Description)
    
    c <- b %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Approximate Date Homeless" = DateToStreetESSH,
        "Days Homeless at Entry" = CurrentHomelessDuration,
        "Times Homeless Past 3 Years" = TimesHomelessPastThreeYears,
        "Months Homeless Past 3 Years" = MonthsHomelessPastThreeYears,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(c,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "PSH: Adults who entered the project during the 
              reporting period")
    
  })
  
  output$pe_ScoredAtPHEntry <- DT::renderDataTable({
    
    a <- pe_scored_at_ph_entry() %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "All Project Types: Heads of Household who entered the 
              project during the reporting period")
    
  })
  
}
