



function(input, output, session) {
  output$res <- renderPrint({
    format(ymd(input$utilizationDate), "%B %Y")
  })
  observeEvent(c(input$providerList), {
    output$currentHHs <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Households",
            color = "aqua",
            icon = icon("users"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(Households)
          )
        })
      }
    else{
      
    }
    
    output$currentUnits <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Unit Capacity",
            color = "aqua",
            icon = icon("building"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitCount)
          )
        })
      }
    else{
      
    }
    
    
    output$currentUnitUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Unit Utilization",
            color = "aqua",
            icon = icon("building"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitUtilization)
          )
        })
      }
    else{
      
    }
    
    output$currentClients <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Clients",
            color = "purple",
            icon = icon("user"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(Clients)
          )
        })
      }
    else{
      
    }
    
    output$currentBeds <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Bed Capacity",
            color = "purple",
            icon = icon("bed"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedCount)
          )
        })
      }
    else{
      
    }
    
    #   output$currentBedUtilization <-
    #     if (nrow(Utilization %>%
    #              filter(
    #                ProjectName == input$providerList &
    #                ProjectType %in% c(1, 2, 3, 8, 9)
    #              )) > 0) {
    #       renderInfoBox({
    #         infoBox(
    #           "Current Bed Utilization",
    #           color = "purple",
    #           icon = icon("bed"),
    #           Utilization %>%
    #             filter(ProjectName == input$providerList) %>%
    #             select(BedUtilization)
    #         )
    #       })
    #     }
    #   else{
    #
    #   }
  })
  
  output$SPDATScoresHoused <-
    renderDataTable({
      # ReportStart <- format.Date(mdy(paste0("01-01-", input$y)), "%m-%d-%Y")
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
          ClientID = PersonalID,
          Project = ProjectName,
          EntryDate,
          CountyServed,
          ScoreDate = StartDate,
          Score,
          ScoreAdjusted
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
          ClientID = PersonalID,
          EntryDate,
          ExitDate,
          CountyServed,
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
  
  
}