



function(input, output, session) {
  output$headerUtilization <- renderUI({
    list(
      h2("Bed and Unit Utilization"),
      h4(input$providerListUtilization),
      h4(format(ymd(input$utilizationDate), "%B %Y")))
  })
  
  output$headerLoS <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$LoSSlider1, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$LoSSlider1, 7, 7) == 1 ~ "03-31-",
        substr(input$LoSSlider1, 7, 7) == 2 ~ "06-30",
        substr(input$LoSSlider1, 7, 7) == 3 ~ "09-30-",
        substr(input$LoSSlider1, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$LoSSlider1, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Length of Stay"),
      h4(input$LoSProjectList),
      h4(paste(format(mdy(ReportStart), "%B %Y"), 
             "to", 
             format(mdy(ReportEnd), "%B %Y"))))
  })
  
  output$headerCommunityNeedPH <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$spdatSlider1, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$spdatSlider1, 7, 7) == 1 ~ "03-31-",
        substr(input$spdatSlider1, 7, 7) == 2 ~ "06-30",
        substr(input$spdatSlider1, 7, 7) == 3 ~ "09-30-",
        substr(input$spdatSlider1, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$spdatSlider1, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Community Need, Entered Permanent Housing"),
      h4(input$regionList1),
      h4(paste(format(mdy(ReportStart), "%B %Y"), 
               "to", 
               format(mdy(ReportEnd), "%B %Y"))))
  })
  
  output$headerCommunityNeedCounty <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$spdatSlider2, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$spdatSlider2, 7, 7) == 1 ~ "03-31-",
        substr(input$spdatSlider2, 7, 7) == 2 ~ "06-30",
        substr(input$spdatSlider2, 7, 7) == 3 ~ "09-30-",
        substr(input$spdatSlider2, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$spdatSlider2, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Community Need, Literally Homeless in the County"),
      h4(input$regionList2),
      h4(paste(format(mdy(ReportStart), "%B %Y"), 
               "to", 
               format(mdy(ReportEnd), "%B %Y"))))
  })
  
  output$headerExitsToPH <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$ExitsToPHSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30",
        substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$ExitsToPHSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Successful Placement Detail"),
      h4(input$ExitsToPHProjectList),
      h4(paste(format(mdy(ReportStart), "%B %Y"), 
               "to", 
               format(mdy(ReportEnd), "%B %Y"))))
  })
  
  output$headerHome <- renderUI({
    list(
      h1("Welcome"),
      HTML(
        "<p>R minor _elevated_ is intended for use by Ohio Balance of State HMIS
        users. This site requires a login because client-level data is shown
        (without Personally Identifying Information). Please use this
        site to verify that your HMIS data is accurate and complete.
        <p><a href=\"https://ohiobalanceofstatecoc.shinyapps.io/Rminor\" 
        target=\"_blank\">R minor</a> is a separate COHHIO site used for Ohio 
        Balance of State CoC performance reporting. Visitors to R minor will 
        include HMIS users, program executives, funders, government 
        representatives, advocates, and other interested parties. R minor 
        contains no client-level data.<br><br>
        <p>We're glad you're here! Please select a report in the left sidebar."
      )
    )
  
  })
  
  output$SPDATScoresHoused <-
    renderDataTable({
      ReportStart <- format.Date(ymd(paste0(
        substr(input$spdatSlider1, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$spdatSlider1, 7, 7) == 1 ~ "03-31-",
          substr(input$spdatSlider1, 7, 7) == 2 ~ "06-30",
          substr(input$spdatSlider1, 7, 7) == 3 ~ "09-30-",
          substr(input$spdatSlider1, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$spdatSlider1, 1, 4)
      )), "%m-%d-%Y")
      
      # counting all households who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- SPDATsByProject %>%
        filter(entered_between(SPDATsByProject,
                               ReportStart,
                               ReportEnd)) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList1) %>%
        select(
          "Client ID" = PersonalID,
          Project = ProjectName,
          "Entry Date" = EntryDate,
          "County Served" = CountyServed,
          "Score Date" = StartDate,
          Score,
          "Score Adjusted" = ScoreAdjusted
        )
      
      CountyHousedAverageScores
      
    })
  
  output$SPDATScoresServedInCounty <-
    renderDataTable({
      # ReportStart <- format.Date(mdy(paste0("01-01-", input$y)), "%m-%d-%Y")
      ReportStart <- format.Date(ymd(paste0(
        substr(input$spdatSlider2, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$spdatSlider2, 7, 7) == 1 ~ "03-31-",
          substr(input$spdatSlider2, 7, 7) == 2 ~ "06-30",
          substr(input$spdatSlider2, 7, 7) == 3 ~ "09-30-",
          substr(input$spdatSlider2, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$spdatSlider2, 1, 4)
      )), "%m-%d-%Y")
      # counting all households who were scored AND SERVED between the report dates
      CountyAverageScores <- CountyData %>%
        filter(served_between(CountyData,
                              ReportStart,
                              ReportEnd)) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList2) %>%
        select(
          Project = ProjectName,
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate,
          "County Served" = CountyServed,
          Score
        )
      
      CountyAverageScores
      
    })
  
  output$utilizationDetail <-
    renderDataTable({
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
      
      z <- paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
      
      a <- ClientUtilizers %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, y)
      z <- paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y")) 
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", z)
      
      a 
      
    })
  
  output$utilizationSummary <-
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
      
      a <- ClientUtilizers %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, y)

      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
      
      beds <- Utilization %>%
        filter(ProjectName == input$providerListUtilization) %>%
        select(BedCount)
      
      # units <- Utilization %>%
      #   filter(ProjectName == input$providerListUtilization) %>%
      #   select(UnitCount)
      
      daysInMonth <- days_in_month(input$utilizationDate)
      
      infoBox(
        title = "Total Bed Nights Served",
        color = "purple",
        icon = icon("bed"),
        value = sum(a$BNs),
        subtitle = paste(
          "Bed Count:",
          beds,
          "x",
          daysInMonth,
          "days in",
          format(ymd(input$utilizationDate), "%B"),
          "=",
          beds * daysInMonth,
          "possible bed nights"
        )
      )
    })
  
  output$LoSDetail <- 
    renderDataTable({
      
      ReportStart <- format.Date(ymd(paste0(
        substr(input$LoSSlider1, 1, 4),
        "-01-01"
      )), "%m-%d-%Y")
      
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          substr(input$LoSSlider1, 7, 7) == 1 ~ "03-31-",
          substr(input$LoSSlider1, 7, 7) == 2 ~ "06-30",
          substr(input$LoSSlider1, 7, 7) == 3 ~ "09-30-",
          substr(input$LoSSlider1, 7, 7) == 4 ~ "12-31-"
        ),
        substr(input$LoSSlider1, 1, 4)
      )), "%m-%d-%Y")
      
      LoSDetail <- QPR_EEs %>%
        filter((
          (!is.na(MoveInDateAdjust) & ProjectType == 13) |
          (!is.na(ExitDate) & ProjectType %in% c(1, 2, 8))
          ) &
          exited_between(., ReportStart, ReportEnd) &
          ProjectName == input$LoSProjectList) %>%
        arrange(desc(DaysinProject)) %>%
        select("Client ID" = PersonalID, 
               "Bed Start" = EntryAdjust, 
               "Exit Date" = ExitDate, 
               "Days in Project" = DaysinProject) 
      
      LoSDetail
      
    })

  output$ExitsToPH <- renderDataTable({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$ExitsToPHSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$ExitsToPHSlider, 1, 4)
    )), "%m-%d-%Y")
    
    SuccessfullyPlaced <- QPR_EEs %>%
      filter(((
        ProjectType %in% c(3, 9, 13) &
          !is.na(MoveInDateAdjust)
      ) |
        ProjectType %in% c(1, 2, 4, 8, 12)) &
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
    TotalHHsSuccessfulPlacement <- QPR_EEs %>%
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
                                MoveInDate, EntryDate)) %>%
      arrange(DestinationGroup) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Bed Start" = BedStart,
        "Exit Date" = ExitDate,
        "Destination Group" =  DestinationGroup
      )
    
    SuccessfulPlacement

  })
  
  # output$ExitsToPHOutreach <- renderPlotly({
  #   if (input$radioExitsToPHPTC == "Street Outreach") {
  #     ReportStart <- format.Date(ymd(paste0(
  #       substr(input$ExitsToPHSlider, 1, 4),
  #       "-01-01"
  #     )), "%m-%d-%Y")
  #     ReportEnd <- format.Date(mdy(paste0(
  #       case_when(
  #         substr(input$ExitsToPHSlider, 7, 7) == 1 ~ "03-31-",
  #         substr(input$ExitsToPHSlider, 7, 7) == 2 ~ "06-30-",
  #         substr(input$ExitsToPHSlider, 7, 7) == 3 ~ "09-30-",
  #         substr(input$ExitsToPHSlider, 7, 7) == 4 ~ "12-31-"
  #       ),
  #       substr(input$ExitsToPHSlider, 1, 4)
  #     )), "%m-%d-%Y")
  #     
  #     totalServed <- QPR_EEs %>%
  #       filter(exited_between(., ReportStart, ReportEnd) &
  #                ProjectType == 4) %>%
  #       group_by(FriendlyProjectName, ProjectType, County, Region) %>%
  #       summarise(TotalHHs = n())
  #     
  #     notUnsheltered <- QPR_EEs %>%
  #       filter(
  #         ProjectType == 4 &
  #           Destination != 16 &
  #           DestinationGroup %in% c("Temporary", "Permanent") &
  #           exited_between(., ReportStart, ReportEnd)
  #       ) %>%
  #       group_by(FriendlyProjectName, ProjectType, County, Region) %>%
  #       summarise(NotUnsheltered = n())
  #     
  #     goalOutreach <- goals %>%
  #       filter(Measure == "Exits to Temporary or Permanent Housing") %>%
  #       select(Goal, ProjectType)
  #     
  #     notUnsheltered <- notUnsheltered %>%
  #       left_join(goalOutreach, by = "ProjectType") %>%
  #       left_join(totalServed,
  #                 by = c("FriendlyProjectName",
  #                        "ProjectType",
  #                        "County",
  #                        "Region")) %>%
  #       mutate(
  #         Percent = NotUnsheltered / TotalHHs,
  #         hover = paste0(
  #           FriendlyProjectName,
  #           "\nExited to Temp or PH: ",
  #           NotUnsheltered,
  #           "\nTotal Households: ",
  #           TotalHHs,
  #           "\n",
  #           as.integer(Percent * 100),
  #           "%",
  #           sep = "\n"
  #         )
  #       ) %>%
  #       filter(Region %in% c(input$ExitsToPHRegionSelect))
  #     
  #     title <-
  #       paste0(
  #         "Exits to Temporary or Permanent Housing\n",
  #         "Street Outreach\n",
  #         ReportStart,
  #         " to ",
  #         ReportEnd
  #       )
  #     
  #     plot_ly(
  #       notUnsheltered,
  #       x = ~ FriendlyProjectName,
  #       y = ~ Percent,
  #       text = ~ hover,
  #       hoverinfo = 'text',
  #       type = "bar"
  #     ) %>%
  #       layout(
  #         xaxis = list(title = ""),
  #         yaxis = list(title = "Exited to Temporary or Permanent Housing",
  #                      tickformat = "%"),
  #         title = list(text = title,
  #                      font = list(size = 15)),
  #         margin = list(
  #           l = 50,
  #           r = 50,
  #           b = 100,
  #           t = 100,
  #           pad = 4
  #         ),
  #         shapes = list(
  #           type = "rect",
  #           name = "CoC Goal",
  #           fillcolor = "#008000",
  #           line = list(color = "white", width = .01),
  #           layer = "below",
  #           xref = "paper",
  #           yref = "y",
  #           x0 = 0,
  #           x1 = 1,
  #           y0 = ~ Goal[1],
  #           y1 = 1,
  #           opacity = .2
  #         ),
  #         title = "Obtaining and Maintaining Permanent Housing"
  #       )
  #   }
  #   else{
  #     NULL
  #   }
  # })
      
  # QPR Rapid Placement into RRH
  # output$DaysToHouse <- 
  #   renderPlotly({
  #     
  #     ReportStart <- format.Date(ymd(paste0(
  #       substr(input$RapidRRHDateSlider, 1, 4),
  #       "-01-01"
  #     )), "%m-%d-%Y")
  #     
  #     ReportEnd <- format.Date(mdy(paste0(
  #       case_when(
  #         substr(input$RapidRRHDateSlider, 7, 7) == 1 ~ "03-31-",
  #         substr(input$RapidRRHDateSlider, 7, 7) == 2 ~ "06-30-",
  #         substr(input$RapidRRHDateSlider, 7, 7) == 3 ~ "09-30-",
  #         substr(input$RapidRRHDateSlider, 7, 7) == 4 ~ "12-31-"
  #       ),
  #       substr(input$RapidRRHDateSlider, 1, 4)
  #     )), "%m-%d-%Y")
  #     
  #     daysToHouse <- QPR_EEs %>%
  #       filter(
  #         ProjectType == 13 &
  #           !is.na(MoveInDateAdjust) &
  #           Region %in% c(input$RapidRRHRegion) &
  #           entered_between(., ReportStart, ReportEnd)
  #       ) %>%
  #       mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days"))
  #     
  #     RRHgoal <- goals %>%
  #       filter(SummaryMeasure == "Rapid Placement") %>%
  #       select(ProjectType, Goal)
  #     
  #     summaryDays <- daysToHouse %>%
  #       group_by(FriendlyProjectName, County, Region, ProjectType) %>%
  #       summarise(AvgDays = as.integer(mean(DaysToHouse, na.rm = TRUE)),
  #                 TotalHHs = n()) %>%
  #       left_join(RRHgoal, by = "ProjectType") %>%
  #       mutate(hover = paste0(
  #         FriendlyProjectName,
  #         "\nAverage Days to House: ",
  #         AvgDays,
  #         "\nTotal Households: ",
  #         TotalHHs,
  #         sep = "\n"
  #       ))
  #     
  #     title <- paste0("Average Days to House\nRapid Rehousing\n",
  #                     ReportStart, " to ", ReportEnd)
  #     
  #     plot_ly(
  #       summaryDays,
  #       x = ~ FriendlyProjectName,
  #       y = ~ AvgDays,
  #       text = ~ hover,
  #       hoverinfo = 'text',
  #       type = "bar"
  #     ) %>%
  #       layout(
  #         xaxis = list(title = ~ FriendlyProjectName),
  #         yaxis = list(title = "Average Days to House"),
  #         title = list(
  #           text = title,
  #           font = list(
  #             size = 15
  #           )),
  #         margin = list(
  #           l = 50,
  #           r = 50,
  #           b = 100,
  #           t = 100,
  #           pad = 4
  #         ),
  #         shapes = list(
  #           type = "rect",
  #           name = "CoC Goal",
  #           fillcolor = "#008000",
  #           line = list(color = "white", width = .01),
  #           layer = "below",
  #           xref = "paper",
  #           yref = "y",
  #           x0 = 0,
  #           x1 = 1,
  #           y0 = ~ Goal[1],
  #           y1 = 0,
  #           opacity = .2
  #         ),
  #         title = "Days to House"
  #       )
  #   })
  
}