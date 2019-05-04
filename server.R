

function(input, output, session) {
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
    
    output$currentBedUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0) {
        renderInfoBox({
          infoBox(
            "Current Bed Utilization",
            color = "purple",
            icon = icon("bed"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedUtilization)
          )
        })
      }
    else{
      
    }
  })

  output$SPDATScoresByCounty <- 
    renderPlot({
      ReportStart <- format.Date(mdy(paste0("01-01-", input$y)), "%m-%d-%Y")
      
      ReportEnd <- format.Date(mdy(paste0(
        case_when(
          input$q == 1 ~ "03-31-",
          input$q == 2 ~ "06-30",
          input$q == 3 ~ "09-30-",
          input$q == 4 ~ "12-31-"
        ),
        input$y
      )), "%m-%d-%Y")
# counting all households who were scored AND SERVED between the report dates
      CountyAverageScores <- CountyData %>%
        filter(served_between(CountyData, 
                              ReportStart, 
                              ReportEnd)) %>%
        select(CountyServed, PersonalID, Score) %>%
        distinct() %>%
        group_by(CountyServed) %>%
        summarise(AverageScore = round(mean(Score), 1),
                  HHsLHinCounty = n())
# counting all households who ENTERED either RRH or PSH between the report dates
      CountyHousedAverageScores <- SPDATsByProject %>%
        filter(entered_between(SPDATsByProject, 
                              ReportStart, 
                              ReportEnd)) %>%
        group_by(CountyServed) %>%
        summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
                  HHsHousedInCounty = n())
# pulling in both averages for each county plus adding Region for grouping      
      Compare <-
        full_join(CountyAverageScores,
                  CountyHousedAverageScores,
                  by = "CountyServed") %>%
        arrange(CountyServed) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList)
# the plot      
      ggplot(
        Compare,
        aes(x = CountyServed, y = AverageScore)
      ) +
        geom_point(
          aes(x = CountyServed, y = AverageScore),
          size = 14,
          shape = 95
        ) +
        scale_y_continuous(limits = c(0,17)) +
        theme(axis.text.x = element_text(size = 10)) +
        geom_point(
          aes(x = CountyServed, y = HousedAverageScore),
          size = 8,
          shape = 17,
          colour = "#56B4E9"
        ) +
        xlab(input$regionList) +
        ylab("Average SPDAT Score") +
        ggtitle(paste("Date Range:", ReportStart, "to", ReportEnd)) +
        theme_light() + 
        theme(plot.title = element_text(lineheight = 1, size = rel(1.8)),
              axis.text.x = element_text(size = rel(1.8)),
              axis.text.y = element_text(size = rel(1.8)),
              axis.title = element_text(size = rel(1.8)),
              plot.margin = margin(t = 15, r = 15, b = 15, l = 15))
    })
  
  output$CountyScoresText <-
    renderText(hhsServedInCounty)
  
  output$HHsServedScoresText <-
    renderText(hhsHousedInCounty)
  
  output$NoteToUsers <-
    renderText(noteToUsers)
}