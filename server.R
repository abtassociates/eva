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
        "<p>R minor elevated is intended for use by Ohio Balance of State HMIS
        users. This site requires a login because client-level data is shown
        (without Personally Identifying Information). Please use this
        site to verify that your HMIS data is accurate and complete.
        <p><a href=\"https://ohiobalanceofstatecoc.shinyapps.io/Rminor\"
        target=\"_blank\">R minor</a> is a separate COHHIO site used for Ohio
        Balance of State CoC performance reporting. Visitors to R minor will
        include HMIS users, program executives, funders, government
        representatives, advocates, and other interested parties. R minor
        contains no client-level data.<br>
        <p>We're glad you're here! Please select a report in the left sidebar."
      )
    )
    
  })
  
  output$headerPrioritization <- renderUI({
    list(h2("Prioritization Report"),
         h4("Literally Homeless Clients as of", FileEnd))
  })
  
  output$headerCurrent <- renderUI({
    list(h2("Current Clients as of", FileEnd),
         h4(input$currentProviderList))
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
    list(
      h2("2020 CoC Competition: Project Evaluation"),
      a("CoC Competition Specifications and Timeline",
        href = "https://cohhio.org/boscoc/coc-program/"), 
      h5(strong("Next Due Date:"),
         format(ymd(next_thing_due$DueDate), "%A %b %e, %Y"),
         "| ",
         next_thing_due$Event),
      h4("Fixed Date Range: January 2019 - December 2019"),
      h4(input$pe_provider)
    )
  })
  
  output$headerDataQuality <- renderUI({
    list(h2("Data Quality"),
         h4(input$providerListDQ),
         h4(paste(
           format(input$dq_startdate, "%m-%d-%Y"),
           "to",
           format(update_date, "%m-%d-%Y")
         )))
  })
  
  output$headerDeskTime <- renderUI({
    list(h2("Data Entry Delay"),
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
    the Entry Date. If this is the case, please email the HMIS team."
      )
    )
  
  output$headerUnshDataQuality <- renderUI({
    list(h2("Unsheltered Data Quality"),
         h4("Entered into the Unsheltered Provider by a User whose Default 
            Provider is", input$unshDefaultProvidersList),
         h4(paste(
           format(input$unsh_dq_startdate, "%m-%d-%Y"),
           "to",
           format(update_date, "%m-%d-%Y")
         )))
  })
  
  output$headerCocDQ <- renderUI({
    list(
      h2("CoC-wide Data Quality"),
      h4("October 2018 through Last Updated Date")
    )
  })
  
  output$headerCommunityNeedPH <- renderUI({
    ReportStart <- format.Date(input$spdatDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$spdatDateRange[2], "%B %d, %Y")
    
    list(
      h2("Community Need, Entered Permanent Housing"),
      h4(input$regionList1),
      h4(paste(
        ReportStart,
        "to",
        ReportEnd
      ))
    )
  })
  
  output$headerRegionDataQuality <- renderUI({
    list(h2("Regional Data Quality"),
         h4(input$regionList3),
         h4(paste(
           format(input$dq_region_startdate, "%m-%d-%Y"),
           "to",
           format(update_date, "%m-%d-%Y")
         )))
  })
  
  output$headerCommunityNeedCounty <- renderUI({
    ReportStart <- format.Date(input$spdatDateRange2[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$spdatDateRange2[2], "%B %d, %Y")
    
    list(
      h2("Community Need, Literally Homeless in the County"),
      h4(input$regionList2),
      h4(paste(
        ReportStart,
        "to",
        ReportEnd
      ))
    )
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
  
  desk_time <- validation %>%
    filter(entered_between(., ReportStart, ReportEnd) &
             ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
    select(ProjectName, PersonalID, HouseholdID, EntryDate, DateCreated) %>%
    mutate(
      DeskTime = difftime(floor_date(ymd_hms(DateCreated), unit = "day"),
                          ymd(EntryDate),
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
    summarise(MedianDeskTime = median(DeskTime)) %>%
    ungroup()
  
  dq_plot_desk_time <-
    ggplot(
      desk_time %>%
        filter(ProjectName == provider),
      aes(x = ymd(EntryDate), y = DeskTime)
    ) +
    geom_point(aes(color = GoalMet, size = 8, alpha = .2),
               show.legend = FALSE)+
    scale_color_identity() +
    geom_hline(yintercept = 5, color = "forestgreen") +
    geom_hline(yintercept = 0, color = "forestgreen") +
    geom_hline(
      data = desk_time_medians %>%
        filter(ProjectName == provider),
      aes(yintercept = MedianDeskTime),
      color = "black"
    ) +
    xlim(today() - years(1), today()) +
    geom_label(x = today() - months(6),
               y = desk_time_medians %>%
                 filter(ProjectName == provider) %>%
                 pull(MedianDeskTime),
               label = paste("Median:", 
                             desk_time_medians %>%
                               filter(ProjectName == provider) %>%
                               pull(MedianDeskTime),
                             "days")) +
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
  
  desk_time <- validation %>%
    filter(entered_between(., ReportStart, ReportEnd) &
             ProjectType %in% c(1, 2, 3, 4, 8, 9, 12, 13)) %>%
    select(ProjectName, PersonalID, HouseholdID, EntryDate, DateCreated) %>%
    mutate(
      DeskTime = difftime(floor_date(ymd_hms(DateCreated), unit = "day"),
                          ymd(EntryDate),
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
      
      SuccessfullyPlaced <- qpr_leavers %>%
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
      TotalHHsSuccessfulPlacement <- qpr_leavers %>%
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
  
  output$headerLoS <- renderUI({
    ReportStart <- format.Date(input$LoSDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$LoSDateRange[2], "%B %d, %Y")
    
    list(h2("Length of Stay"),
         h4(input$LoSProjectList),
         h4(paste(
           ReportStart,
           "to",
           ReportEnd
         )))
  })
  
  output$LoSSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$LoSDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$LoSDateRange[2], "%m-%d-%Y")
      
      los_summary <- qpr_leavers %>%
        filter(((
          !is.na(MoveInDateAdjust) & ProjectType == 13
        ) |
          (
            !is.na(ExitDate) & ProjectType %in% c(1, 2, 8)
          )) &
          exited_between(., ReportStart, ReportEnd) &
          ProjectName == input$LoSProjectList
        ) %>%
        group_by(ProjectName) %>%
        summarise(Average = format(mean(DaysinProject),
                                   digits = 1),
                  Median = median(DaysinProject))
      
      infoBox(
        title = "Average and Median Length of Stay",
        color = "purple",
        icon = icon("clock"),
        value = paste("Average", 
                      los_summary$Average, 
                      "/ Median", 
                      los_summary$Median,
                      "days"),
        subtitle = "Length of Stay in Project's Housing"
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
  
  output$headerNCBs <- renderUI({
    ReportStart <- format.Date(input$NCBDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$NCBDateRange[2], "%B %d, %Y")
    
    list(h2("Non-Cash Benefits at Exit"),
         h4(input$MBProjectListNC),
         h4(paste(
           ReportStart,
           "to",
           ReportEnd
         )))
  })
  
  output$headerHealthInsurance <- renderUI({
    ReportStart <- format.Date(input$HIDateRange[1], "%B %d, %Y")
    ReportEnd <- format.Date(input$HIDateRange[2], "%B %d, %Y")
    
    list(h2("Health Insurance at Exit"),
         h4(input$MBProjectListHI),
         h4(paste(
           ReportStart,
           "to",
           ReportEnd
         )))
  })
  
  output$headerDaysToHouse <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$RapidRRHDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RapidRRHDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h1("Days to House"),
         h4(input$RapidRRHProviderList),
         h4(paste(
           format(mdy(ReportStart), "%B %Y"),
           "to",
           format(mdy(ReportEnd), "%B %Y")
         )))
  })
  
  output$prioritizationData <- DT::renderDataTable({
    
    active <- active_list %>%
      filter(CountyServed %in% c(input$prioritizationCounty) |
               is.na(CountyServed)) %>%
      arrange(HouseholdID) %>%
      mutate(EntryDate = format.Date(ymd(EntryDate), "%m-%d-%Y")) %>%
      select(
        "HoH Client ID" = PersonalID,
        "Project Name" = ProjectName,
        "Entry Date" = EntryDate,
        "County" = CountyServed,
        "Current Situation (Entry, Referral, Perm Housing Track)" = Situation,
        "Veteran" = VeteranStatus,
        "Fleeing DV" = CurrentlyFleeing,
        "Transition Aged Youth" = TAY,
        "Chronic Status" = ChronicStatus,
        "Eligible for PSH (Disability in Household)" = DisabilityInHH,
        "Household Size" = HouseholdSize,
        "Income" = IncomeFromAnySource,
        Score,
        HH_DQ_issue,
        CountyGuessed
      )
    
    datatable(
      active,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi',
                     columnDefs = list(list(
                       visible = FALSE, targets = c(13:14)
                     )))
    ) %>%
      formatStyle(columns = 'Client ID',
                  valueColumns = 13,
                  backgroundColor = styleEqual(c(1), 
                                               c("#7d7d8d"))) %>%
      formatStyle(columns = 'County',
                  valueColumns = 14,
                  backgroundColor = styleEqual(c(1),
                                               c("#7d7d8d")))
    
  })
  
  output$downloadActiveList <- downloadHandler(
    filename = function() {
      "active_list.xlsx"
    },
    content = function(file) {
      write_xlsx(active_list %>%
                   filter(
                     CountyServed %in% c(input$prioritizationCounty) |
                       is.na(CountyServed)
                   ), path = file)
    }
  )
  
  output$currentClients <- DT::renderDataTable({
    datatable(
      validation %>%
        filter(is.na(ExitDate) &
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
          Days = case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) ~ paste(today() - EntryDate,
                                              "days awaiting housing in project"),
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) ~ paste(today() - MoveInDateAdjust,
                                               "days housed in project"),!ProjectType %in% c(3, 13) ~ paste(today() - EntryDate,
                                                                                                            "days in project")
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
          Days
        ),
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
        format(floor_date(ymd(input$utilizationDate) + months(1),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      z <-
        paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
      
      a <- utilizers_clients %>%
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
        format(floor_date(ymd(input$utilizationDate) + months(1),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      a <- utilizers_clients %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- utilization %>%
        filter(ProjectName == input$providerListUtilization) %>%
        select(BedCount)
      
      daysInMonth <- days_in_month(input$utilizationDate)
      
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
        format(floor_date(ymd(input$utilizationDate) + months(1),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      a <- utilizers_clients %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- utilization %>%
        filter(ProjectName == input$providerListUtilization) %>%
        select(BedCount)
      
      # units <- Utilization %>%
      #   filter(ProjectName == input$providerListUtilization) %>%
      #   select(UnitCount)
      
      daysInMonth <- days_in_month(input$utilizationDate)
      
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
        format(floor_date(ymd(input$utilizationDate) + months(1),
                          unit = "month") - days(1),
               "%m-%d-%Y")
      
      y <- paste0(substr(input$utilizationDate, 6, 7),
                  "01",
                  substr(input$utilizationDate, 1, 4))
      
      a <- utilizers_clients %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, all_of(y))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- utilization %>%
        filter(ProjectName == input$providerListUtilization) %>%
        select(BedCount)
      
      beds <- as.numeric(beds)
      
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
  
  output$dq_provider_summary_table <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    dq_main %>%
      filter(ProjectName == input$providerListDQ &
               served_between(., ReportStart, ReportEnd)) %>%
      group_by(Type, Issue, Guidance) %>%
      ungroup() %>%
      select(Type, Issue, Guidance) %>%
      mutate(Type = factor(Type, levels = c("High Priority",
                                            "Error",
                                            "Warning"))) %>%
      arrange(Type) %>%
      unique()
  })
  
  output$dq_provider_summary_box <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    x <- dq_main %>%
      filter(ProjectName == input$providerListDQ &
               served_between(., ReportStart, ReportEnd))
    if (nrow(x) > 0) {
      box(
        id = "DQSummaryProvider",
        title = "Data Quality Guidance",
        status = "info",
        solidHeader = TRUE,
        tableOutput("dq_provider_summary_table"),
        width = 12
      )
    }
    else {

    }
  })

  output$dq_unsheltered_summary_table <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    dq_unsheltered %>%
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
    x <- dq_unsheltered %>%
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
    a <- dq_main %>%
      filter(ProjectRegion == input$regionList3 &
               served_between(., ReportStart, ReportEnd)) %>%
      select(ProjectName, Type, Issue)
    
    b <- dq_unsheltered %>%
      filter(UserRegion == input$regionList3 &
               served_between(., ReportStart, ReportEnd)) %>%
      mutate(ProjectName = paste("Unsheltered Provider, entered by a user from", 
                                 DefaultProvider))%>%
      select(ProjectName, Type, Issue)
    
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
    DuplicateEEs <- dq_main %>%
      filter(
        Issue == "Duplicate Entry Exits" &
          ProjectName == input$providerListDQ &
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
    DuplicateEEs <- dq_main %>%
      filter(
        Issue == "Duplicate Entry Exits" &
          ProjectName == input$providerListDQ &
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
    HHIssues <- dq_main %>%
      filter(
        Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household"
        ) &
          ProjectName == input$providerListDQ &
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
    HHIssues <- dq_main %>%
      filter(
        Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household"
        ) &
          ProjectName == input$providerListDQ &
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
    HHIssues <- dq_main %>%
      filter(
        Issue == "Missing Client Location" &
          ProjectName == input$providerListDQ &
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
    HHIssues <- dq_main %>%
      filter(
        Issue == "Missing Client Location" &
          ProjectName == input$providerListDQ &
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
    no_contact <- dq_main %>%
      filter(
        Issue == "Missing PATH Contact" &
          ProjectName == input$providerListDQ &
          served_between(., ReportStart, ReportEnd)
      )
    if (nrow(no_contact) > 0) {
      box(
        id = "location",
        title = "Missing Contact (PATH)",
        status = "warning",
        solidHeader = TRUE,
        dq_main %>%
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
    x <- dq_main %>%
      filter(
        Issue == "Missing PATH Contact" &
          ProjectName == input$providerListDQ &
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
    APs_w_EEs <- dq_main %>%
      filter(
        Issue == "Access Point with Entry Exits" &
          ProjectName == input$providerListDQ &
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
    APs_w_EEs <- dq_main %>%
      filter(
        Issue == "Access Point with Entry Exits" &
          ProjectName == input$providerListDQ &
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
    
    OverlappingEEs <- dq_overlaps %>%
      filter(
          ProjectName == input$providerListDQ &
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
    OverlappingEEs <- dq_overlaps %>%
      filter(
        Issue == "Overlapping Project Stays" &
          ProjectName == input$providerListDQ &
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
        status = "info",
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
    AP_not_doing_referrals <- aps_no_referrals %>%
      filter(ProviderCreating == input$providerListDQ)
    
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
      a <- aps_no_referrals %>% arrange(ProviderCreating)
      
      datatable(a, rownames = FALSE)
    })
  
  output$cocOutstandingReferrals <- 
    renderPlot(dq_plot_outstanding_referrals)
  
  output$cocOverlap <- DT::renderDataTable({
    ReportStart <- "10012018"
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    a <- dq_overlaps %>%
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
  
  output$cocLongStayers <- DT::renderDataTable({
    ReportStart <- "10012018"
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    a <- dq_main %>%
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
  
  output$cocWidespreadIssues <- DT::renderDataTable({
    a <- dq_past_year %>%
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
  
  output$cocDQWarnings <- renderPlot(dq_plot_projects_warnings)
  
  output$cocDQErrorTypes <- renderPlot(dq_plot_errors)
  
  output$cocDQWarningTypes <- renderPlot(dq_plot_warnings)
  
  output$cocDQErrors <- renderPlot(dq_plot_projects_errors)
  
  output$cocHHErrors <- renderPlot(dq_plot_hh_errors)
  
  output$cocEligibility <- renderPlot(dq_plot_eligibility)
  
  output$cocUnshelteredHigh <- renderPlot(dq_plot_unsheltered_high)
  
  output$cocAPsNoReferrals <- renderPlot({
    ggplot(data_APs, aes(fill = category, x = providertype, y = percent)) +
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
  
  output$cocSPDAT <- renderPlot(dq_plot_hh_no_spdat)
  
  output$Ineligible <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    Ineligible <- detail_eligibility %>%
      filter(ProjectName == input$providerListDQ &
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
    Ineligible <- detail_eligibility %>%
      filter(ProjectName == input$providerListDQ &
               served_between(., ReportStart, ReportEnd))
    
    if (nrow(Ineligible) > 0) {
      box(
        id = "eligibility",
        title = "Check Eligibility",
        status = "warning",
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
  
  output$DQErrors <- DT::renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(update_date, "%m-%d-%Y")
    
    DQErrors <- dq_main %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits",
          "Access Point with Entry Exits"
        ) & # because these are all in the boxes already
          served_between(., ReportStart, ReportEnd) &
          ProjectName == input$providerListDQ &
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
  
  output$DQWarnings <- DT::renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(update_date, "%m-%d-%Y")
    
    DQWarnings <- dq_main %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits",
          "Check Eligibility"
        ) &
          served_between(., ReportStart, ReportEnd) &
          ProjectName == input$providerListDQ &
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
  
  output$unshIncorrectResPriorTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    ResPrior <- dq_unsheltered %>%
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    ResPrior <- dq_unsheltered %>%
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
  
  output$unshDuplicateEEsTable <- renderTable({
    ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    DuplicateEEs <- dq_unsheltered %>%
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    DuplicateEEs <- dq_unsheltered %>%
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    HHIssues <- dq_unsheltered %>%
      filter(
        Issue %in% c("Too Many Heads of Household", 
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    HHIssues <- dq_unsheltered %>%
      filter(
        Issue %in% c("Too Many Heads of Household", 
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    county <- dq_unsheltered %>%
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    county <- dq_unsheltered %>%
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    
    overlaps <- unsh_overlaps %>%
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
    ReportEnd <- format.Date(mdy(FileEnd), "%m-%d-%Y")
    
    overlaps <- unsh_overlaps %>%
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
    ReportEnd <- format.Date(update_date, "%m-%d-%Y")
    
    unshDQErrors <- dq_unsheltered %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Missing County Served",
          "Missing County of Prior Residence",
          "Duplicate Entry Exits",
          "Wrong Provider (Not Unsheltered)"
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
    ReportEnd <- format.Date(update_date, "%m-%d-%Y")
    
    unshDQWarnings <- dq_unsheltered %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
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
  
  output$SPDATScoresHoused <-
    DT::renderDataTable({
      ReportStart <- format.Date(input$spdatDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$spdatDateRange[2], "%m-%d-%Y")
      
      # counting all households who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- qpr_spdats_project %>%
        filter(entered_between(qpr_spdats_project,
                               ReportStart,
                               ReportEnd)) %>%
        left_join(., regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList1) %>%
        mutate(PersonalID = as.character(PersonalID)) %>%
        arrange(ScoreAdjusted) %>%
        select(
          "Client ID" = PersonalID,
          Project = ProjectName,
          "Entry Date" = EntryDate,
          "County Served" = CountyServed,
          "Score Date" = ScoreDate,
          Score,
          "Score Adjusted" = ScoreAdjusted
        )
      
      datatable(
        CountyHousedAverageScores,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
      
    })
  
  output$ScoredHousedSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$spdatDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$spdatDateRange[2], "%m-%d-%Y")
      
      scores <- qpr_spdats_project %>%
        filter(entered_between(qpr_spdats_project,
                               ReportStart,
                               ReportEnd)) %>%
        left_join(., regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList1) %>%
        group_by(RegionName) %>%
        summarise(AvgScore = as.integer(mean(ScoreAdjusted)))
      
      infoBox(
        title = "Average Score",
        color = "purple",
        icon = icon("parachute-box"),
        value = scores$AvgScore,
        subtitle = "Households who were Housed in RRH or PSH in the Selected Region"
      )
      
    })
  
  output$SPDATScoresServedInCounty <-
    DT::renderDataTable({
      
      ReportStart <- format.Date(input$spdatDateRange2[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$spdatDateRange2[2], "%m-%d-%Y")
      
      # counting all households who were scored AND SERVED between the report dates
      CountyAverageScores <- qpr_spdats_county %>%
        filter(served_between(qpr_spdats_county,
                              ReportStart,
                              ReportEnd)) %>%
        left_join(., regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList2) %>%
        mutate(PersonalID = as.character(PersonalID)) %>%
        select(
          Project = ProjectName,
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "County Served" = CountyServed,
          Score
        ) %>%
        arrange(Score)
      
      datatable(CountyAverageScores,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'))
      
    })
  
  output$ScoredInRegionSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$spdatDateRange2[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$spdatDateRange2[2], "%m-%d-%Y")
      
      # counting all households who were scored AND SERVED between the report dates
      scores <- qpr_spdats_county %>%
        filter(served_between(qpr_spdats_county,
                              ReportStart,
                              ReportEnd)) %>%
        left_join(., regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList2) %>%
        group_by(RegionName) %>%
        summarise(AvgScore = as.integer(mean(Score)))
      
      infoBox(
        title = "Average Score",
        color = "purple",
        icon = icon("shoe-prints"),
        value = scores$AvgScore,
        subtitle = "Literally Homeless Households in the Selected Region"
      )
      
    })
  
  output$LoSDetail <-
    DT::renderDataTable({
      ReportStart <- format.Date(input$LoSDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$LoSDateRange[2], "%m-%d-%Y")
      
      LoSDetail <- qpr_leavers %>%
        filter(((
          !is.na(MoveInDateAdjust) & ProjectType == 13
        ) |
          (
            !is.na(ExitDate) & ProjectType %in% c(1, 2, 8)
          )) &
          exited_between(., ReportStart, ReportEnd) &
          ProjectName == input$LoSProjectList
        ) %>%
        mutate(PersonalID = as.character(PersonalID)) %>%
        arrange(desc(DaysinProject)) %>%
        select(
          "Client ID" = PersonalID,
          "Bed Start" = EntryAdjust,
          "Exit Date" = ExitDate,
          "Days in Project" = DaysinProject
        )
      
      datatable(LoSDetail,
                rownames = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'))
      
    })
  
  output$ExitsToPH <- DT::renderDataTable({
    ReportStart <- format.Date(input$ExitsToPHDateRange[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$ExitsToPHDateRange[2], "%m-%d-%Y")
    
    SuccessfullyPlaced <- qpr_leavers %>%
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
    TotalHHsSuccessfulPlacement <- qpr_leavers %>%
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
  
  output$IncomeIncrease <- DT::renderDataTable({
    ReportStart <- format.Date(input$IncomeDateRange[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$IncomeDateRange[2], "%m-%d-%Y")
    
    a <- qpr_income %>%
      filter(ProjectName == input$incomeProjectList &
               stayed_between(., ReportStart, ReportEnd)) %>%
      mutate(EntryIncome = dollar(EntryIncome, accuracy = .01),
             RecentIncome = dollar(RecentIncome, accuracy = .01),
             Difference = dollar(Difference, accuracy = .01),
             PersonalID = as.character(PersonalID)) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Income at Entry" = EntryIncome,
        "Most Recent Income" = RecentIncome,
        "Increase Amount" = Difference
      )
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
  })
  
  output$qprIncomeSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$IncomeDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$IncomeDateRange[2], "%m-%d-%Y")
      
      meeting_objective <- qpr_income %>%
        filter(
          ProjectName == input$incomeProjectList &
            stayed_between(., ReportStart, ReportEnd) &
            Difference > 0
        ) %>% 
        group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) %>%
        summarise(Increased = n())
      
      # calculating the total households for comparison
      all_hhs <- qpr_income %>%
        filter(ProjectName %in% input$incomeProjectList &
                 stayed_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectName, ProjectType, ProjectCounty, ProjectRegion) %>%
        summarise(TotalHHs = n()) 
      
      IncreasedIncome <- all_hhs %>%
        left_join(
          meeting_objective,
          by = c("ProjectName", "ProjectType", "ProjectCounty", "ProjectRegion")
        )
      
      IncreasedIncome[is.na(IncreasedIncome)] <- 0
      
      IncreasedIncome <- IncreasedIncome %>%
        mutate(Percent = Increased / TotalHHs)
      
      if(nrow(IncreasedIncome) > 0) {
        infoBox(
          title = "Households Increasing Their Income",
          color = "green",
          icon = icon("hand-holding-usd"),
          value = percent(IncreasedIncome$Percent),
          subtitle = paste(IncreasedIncome$Increased, 
                           "out of",
                           IncreasedIncome$TotalHHs, 
                           "households served")
        )
      }
      else{
        infoBox(
          title = "Something's wrong- email us at hmis@cohhio.org!"
        )
      }
    })
  
  output$ExitedWithNCBs <- DT::renderDataTable({
    ReportStart <- format.Date(input$NCBDateRange[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$NCBDateRange[2], "%m-%d-%Y")
    
    a <- qpr_benefits %>%
      filter(ProjectName == input$MBProjectListNC &
               exited_between(., ReportStart, ReportEnd)) %>%
      mutate(
        BenefitsFromAnySource = case_when(
          BenefitsFromAnySource == 0 ~ "No (HUD)",
          BenefitsFromAnySource == 1 ~ "Yes (HUD)",
          BenefitsFromAnySource == 8 ~ "Client doesn't know (HUD)",
          BenefitsFromAnySource == 9 ~ "Client refused (HUD)",
          BenefitsFromAnySource == 99 ~ "Data Not Collected (HUD)"
        ),
        PersonalID = as.character(PersonalID)
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Benefits from Any Source (at Exit)" = BenefitsFromAnySource
      )
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
    
  })
  
  output$qprNCBSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$NCBDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$NCBDateRange[2], "%m-%d-%Y")
      
      meeting_objective <- qpr_benefits %>%
        filter(
            ProjectName == input$MBProjectListNC &
            exited_between(., ReportStart, ReportEnd) &
            BenefitsFromAnySource == 1
        ) %>% 
        group_by(ProjectName) %>%
        summarise(BenefitsAtExit = n())
      
      # calculating the total households for comparison
      all_hhs <- qpr_benefits %>%
        filter(ProjectName == input$MBProjectListNC &
                 exited_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectName) %>%
        summarise(TotalHHs = n()) 
      
      NCBsAtExit <- all_hhs %>%
        left_join(
          meeting_objective,
          by = c("ProjectName")
        )
      
      NCBsAtExit[is.na(NCBsAtExit)] <- 0
      
      NCBsAtExit <- NCBsAtExit %>%
        mutate(Percent = BenefitsAtExit / TotalHHs)
      
      if(nrow(NCBsAtExit) > 0) {
        infoBox(
        title = "Households Exiting With Non Cash Benefits",
        color = "fuchsia",
        icon = icon("shopping-cart"),
        value = percent(NCBsAtExit$Percent),
        subtitle = paste(NCBsAtExit$BenefitsAtExit, 
                          "out of",
                          NCBsAtExit$TotalHHs, 
                          "households")
        )
      }
      else{
        infoBox(
          title = "No Leavers in the Date Range"
        )
      }
    })
  
  output$ExitedWithInsurance <- DT::renderDataTable({
    ReportStart <- format.Date(input$HIDateRange[1], "%m-%d-%Y")
    ReportEnd <- format.Date(input$HIDateRange[2], "%m-%d-%Y")
    
    a <- qpr_benefits %>%
      filter(ProjectName == input$MBProjectListHI &
               exited_between(., ReportStart, ReportEnd)) %>%
      mutate(
        InsuranceFromAnySource = case_when(
          InsuranceFromAnySource == 0 ~ "No (HUD)",
          InsuranceFromAnySource == 1 ~ "Yes (HUD)",
          InsuranceFromAnySource == 8 ~ "Client doesn't know (HUD)",
          InsuranceFromAnySource == 9 ~ "Client refused (HUD)",
          InsuranceFromAnySource == 99 ~ "Data Not Collected (HUD)"
        )
      ) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Health Insurance from Any Source (at Exit)" = InsuranceFromAnySource
      )
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
    
  })
  
  output$healthInsuranceSummary <-
    renderInfoBox({
      ReportStart <- format.Date(input$HIDateRange[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$HIDateRange[2], "%m-%d-%Y")
      
      meeting_objective <- qpr_benefits %>%
        filter(
            ProjectName == input$MBProjectListHI &
            exited_between(., ReportStart, ReportEnd) &
            InsuranceFromAnySource == 1
        ) %>% 
        group_by(ProjectName) %>%
        summarise(InsuranceAtExit = n())
      
      # calculating the total households for comparison
      all_hhs <- qpr_benefits %>%
        filter(ProjectName == input$MBProjectListHI &
                 exited_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectName) %>%
        summarise(TotalHHs = n()) 
      
      HIAtExit <- all_hhs %>%
        left_join(
          meeting_objective,
          by = c("ProjectName")
        )
      
      HIAtExit[is.na(HIAtExit)] <- 0
      
      HIAtExit <- HIAtExit %>%
        mutate(Percent = InsuranceAtExit / TotalHHs)
      
      if(nrow(HIAtExit) > 0) {
        infoBox(
        title = "Total Households Exiting With Health Insurance",
        color = "black",
        icon = icon("medkit"),
        value = percent(HIAtExit$Percent),
        subtitle = paste(HIAtExit$InsuranceAtExit, 
                          "out of",
                         HIAtExit$TotalHHs,
                         "households")
        )}
      else{
        infoBox(
          title = "No Leavers in the Date Range"
        )
      }
    })
  
  output$daysToHouseRRH <- DT::renderDataTable({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$RapidRRHDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RapidRRHDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    daysToHouse <- qpr_rrh_enterers %>%
      filter(
        !is.na(MoveInDateAdjust) &
          ProjectName %in% c(input$RapidRRHProviderList) &
          entered_between(., ReportStart, ReportEnd)
      ) %>%
      mutate(PersonalID = as.character(PersonalID)) %>%
      arrange(DaysToHouse) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Move In Date" = MoveInDate,
        "Days to House" = DaysToHouse
      )
    
    datatable(daysToHouse,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'))
    
  })
  
  output$daysToHouseSummary <-
    renderInfoBox({
      ReportStart <- format.Date(ymd(paste0(
        substr(input$RapidRRHDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RapidRRHDateSlider, 1, 4)
      )), "%m-%d-%Y")
      
      days <- qpr_rrh_enterers %>%
        filter(
          ProjectType == 13 &
            !is.na(MoveInDateAdjust) &
            ProjectName %in% c(input$RapidRRHProviderList) &
            entered_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) %>%
        summarise(AvgDaysToHouse = as.integer(mean(DaysToHouse)))
      
      infoBox(
        title = "Average Days to House",
        color = "purple",
        icon = icon("hourglass-half"),
        value = days$AvgDaysToHouse,
        subtitle = "See table below for detail."
      )
      
    })
  
  output$headerRRHSpending <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$RRHSpendingDateSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$RRHSpendingDateSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Quarterly Performance Report"),
         h3("Rapid Rehousing Spending Goals"),
         # h4(input$RRHRegion),
         h4(ReportStart, "-", ReportEnd))
  })
  
  #  QPR HP vs RRH Spending
  output$RRHSpending <-
    DT::renderDataTable({
      ReportStart <- format.Date(ymd(paste0(
        substr(input$RRHSpendingDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RRHSpendingDateSlider, 1, 4)
      )), "%m-%d-%Y")
      
      rrhSpending <- qpr_spending %>%
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
      ReportStart <- format.Date(ymd(paste0(
        substr(input$RRHSpendingDateSlider, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$RRHSpendingDateSlider, 7, 7) == 1 ~ "03-31-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 2 ~ "06-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 3 ~ "09-30-",
          substr(input$RRHSpendingDateSlider, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$RRHSpendingDateSlider, 1, 4)
      )), "%m-%d-%Y")
      
      hpSpending <- qpr_spending %>%
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
      ptc <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        pull(ProjectType)
      
      summary_pe_final_scoring <- summary_pe_final_scoring %>%
        mutate(
          ExitsToPHMath = str_replace(ExitsToPHMath, "/", "÷"),
          OwnHousingMath = str_replace(OwnHousingMath, "/", "÷"),
          IncreasedIncomeMath = str_replace(IncreasedIncomeMath, "/", "÷"),
          BenefitsAtExitMath = str_replace(BenefitsAtExitMath, "/", "÷"),
          AverageLoSMath = str_replace(AverageLoSMath, "/", "÷"),
          LHResPriorMath = str_replace(LHResPriorMath, "/", "÷"),
          NoIncomeAtEntryMath = str_replace(NoIncomeAtEntryMath, "/", "÷"),
          MedianHHIMath = str_replace(MedianHHIMath, "/", "÷"),
          LongTermHomelessMath = str_replace(LongTermHomelessMath, "/", "÷"),
          ScoredAtEntryMath = str_replace(ScoredAtEntryMath, "/", "÷"),
          DQMath = str_replace(DQMath, "/", "÷"),
          CostPerExitMath = str_replace(CostPerExitMath, "/", "÷"),
          HousingFirstMath = str_replace(HousingFirstMath, "/", "÷"),
          ChronicPrioritizationMath = str_replace(ChronicPrioritizationMath, "/", "÷"),
          OnTrackSpendingMath = str_replace(OnTrackSpendingMath, "/", "÷"),
          UnspentFundsMath = str_replace(UnspentFundsMath, "/", "÷")
        )
      
      a <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHPoints,
          "Moved into Own Housing" = OwnHousingPoints,
          "Increased Income" = IncreasedIncomePoints,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPoints,
          "Average Length of Stay" = AverageLoSPoints,
          "Living Situation at Entry" = LHResPriorPoints,
          "No Income at Entry" = NoIncomeAtEntryPoints,
          "Median Homeless History Index" = MedianHHIPoints,
          "Long Term Homeless" = LongTermHomelessPoints,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryPoints,
          "Data Quality" = DQPoints,
          "Cost per Exit" = CostPerExitScore,
          "Housing First" = HousingFirstScore,
          "Prioritization of Chronic" = ChronicPrioritizationScore,
          "Spending On Track" = OnTrackSpendingScoring,
          "Unspent Funds within Range" = UnspentFundsScoring
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Estimated Score")
      
      b <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHDQ,
          "Moved into Own Housing" = OwnHousingDQ,
          "Increased Income" = IncreasedIncomeDQ,
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
          "Moved into Own Housing" = OwnHousingPossible,
          "Increased Income" = IncreasedIncomePossible,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitPossible,
          "Average Length of Stay" = AverageLoSPossible,
          "Living Situation at Entry" = LHResPriorPossible,
          "No Income at Entry" = NoIncomeAtEntryPossible,
          "Median Homeless History Index" = MedianHHIPossible,
          "Long Term Homeless" = LongTermHomelessPossible,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryPossible,
          "Data Quality" = DQPossible,
          "Cost per Exit" = CostPerExitPossible,
          "Housing First" = HousingFirstPossible,
          "Prioritization of Chronic" = ChronicPrioritizationPossible,
          "Spending On Track" = OnTrackSpendingPossible,
          "Unspent Funds within Range" = UnspentFundsPossible
        ) %>%
        pivot_longer(cols = everything(),
                     names_to = "Measure",
                     values_to = "Possible Score")
      
      d <- summary_pe_final_scoring %>%
        filter(AltProjectName == input$pe_provider) %>%
        select(
          "Exits to Permanent Housing" = ExitsToPHMath,
          "Moved into Own Housing" = OwnHousingMath,
          "Increased Income" = IncreasedIncomeMath,
          "Benefits & Health Insurance at Exit" = BenefitsAtExitMath,
          "Average Length of Stay" = AverageLoSMath,
          "Living Situation at Entry" = LHResPriorMath,
          "No Income at Entry" = NoIncomeAtEntryMath,
          "Median Homeless History Index" = MedianHHIMath,
          "Long Term Homeless" = LongTermHomelessMath,
          "VISPDAT Completion at Entry" =
            ScoredAtEntryMath,
          "Data Quality" = DQMath,
          "Cost per Exit" = CostPerExitMath,
          "Housing First" = HousingFirstMath,
          "Prioritization of Chronic" = ChronicPrioritizationMath,
          "Spending On Track" = OnTrackSpendingMath,
          "Unspent Funds within Range" = UnspentFundsMath
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
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "Documents not yet received",
            DQflag == 3 ~ "Docs received, not yet scored",
            DQflag == 4 ~ "CoC Error",
            DQflag == 5 ~ "Docs received past the due date"
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
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "Documents not yet received",
            DQflag == 3 ~ "Docs received, not yet scored",
            DQflag == 4 ~ "CoC Error",
            DQflag == 5 ~ "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in%
                 c("Long Term Homeless",
                   "Prioritization of Chronic")) %>%
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
            DQflag == 2 ~ "Documents not yet received",
            DQflag == 3 ~ "Docs received, not yet scored",
            DQflag == 4 ~ "CoC Error",
            DQflag == 5 ~ "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in% c(
          "Long Term Homeless",
          "Prioritization of Chronic"
        )) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      sh <- a %>% left_join(b, by = "Measure") %>%
        ungroup() %>%
        left_join(c, by = "Measure") %>%
        left_join(d, by = "Measure") %>%
        mutate(
          DQ = case_when(
            DQflag == 1 ~ "Please correct your Data Quality issues so this item
            can be scored",
            DQflag == 0 ~ "Data Quality passes",
            DQflag == 2 ~ "Documents not yet received",
            DQflag == 3 ~ "Docs received, not yet scored",
            DQflag == 4 ~ "CoC Error",
            DQflag == 5 ~ "Docs received past the due date"
          )
        ) %>%
        filter(!Measure %in% c(
          "Long Term Homeless",
          "VISPDAT Completion at Entry",
          "Prioritization of Chronic"
        )) %>%
        select(1, Calculation, 2, "Possible Score" = 4, "Data Quality" = DQ)
      
      datatable(
        if (ptc == 3) {
          psh
        } else if (ptc == 13) {
          rrh
        } else if(ptc == 2) {
          th
        } else {
          sh
        },
        rownames = FALSE,
        options = list(dom = 't',
                       pageLength = 100)
      )
    })
      
    
  
  output$pe_ExitsToPH <- DT::renderDataTable({
    a <- pe_exits_to_ph %>%
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
              TH, RRH, SH: Heads of Household Leavers")
    
  })
  
  output$pe_OwnHousing <- DT::renderDataTable({
    
    a <- pe_own_housing %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No"),
             Destination = living_situation(Destination)) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        Destination,
        "Destination Group" = DestinationGroup,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "RRH, TH, SH: Heads of Household Leavers who moved into 
              the project's housing")
    
  })
  
  output$pe_BenefitsAtExit <- DT::renderDataTable({
    a <- pe_benefits_at_exit %>%
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
  
  output$pe_IncreasedIncome <- DT::renderDataTable({
    a <- pe_increase_income %>%
      filter(AltProjectName == input$pe_provider) %>%
      mutate(
        IncomeDifference = IncomeMostRecent - IncomeAtEntry,
        MeetsObjective = if_else(MeetsObjective == 1, "Yes", "No")
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Income at Entry" = IncomeAtEntry,
        "Most Recent Income" = IncomeMostRecent,
        "Increase/Decrease" = IncomeDifference,
        "Meets Objective" = MeetsObjective
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "ALL Project Types: Adults who moved into the project's
              housing")
    
  })
  
  output$pe_LivingSituationAtEntry <- DT::renderDataTable({
    a <- pe_res_prior %>%
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
    a <- pe_entries_no_income %>%
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
    a <- pe_length_of_stay %>%
      filter(AltProjectName == input$pe_provider &
               ProjectType %in% c(2, 8, 13)) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Move In Date" = MoveInDateAdjust,
        "Exit Date" = ExitDate,
        "Days in Project" = DaysInProject
      )    
    
    datatable(a,
              rownames = FALSE,
              filter = 'top',
              options = list(dom = 'ltpi'),
              caption = "RRH, TH, SH: Client Leavers who moved into the project's 
              housing")
    
  })
  
  output$pe_MedianHHI <- DT::renderDataTable({
    
    times <- HUD_specs %>%
      filter(DataElement == "TimesHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    months <- HUD_specs %>%
      filter(DataElement == "MonthsHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    a <- pe_homeless_history_index %>%
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
    
    times <- HUD_specs %>%
      filter(DataElement == "TimesHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    months <- HUD_specs %>%
      filter(DataElement == "MonthsHomelessPastThreeYears") %>%
      select(ReferenceNo, Description)
    
    a <- pe_long_term_homeless %>%
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
    
    a <- pe_scored_at_ph_entry %>%
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
              caption = "PSH, RRH, TH: Heads of Household who entered the 
              project during the reporting period")
    
  })
  
}