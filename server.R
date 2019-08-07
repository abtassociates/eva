# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
#<https://www.gnu.org/licenses/>.


function(input, output, session) {
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
  
  output$headerUtilization <- renderUI({
    list(h2("Bed and Unit Utilization"),
         h4(input$providerListUtilization),
         h4(format(ymd(
           input$utilizationDate
         ), "%B %Y")))
  })
  
  output$headerDataQuality <- renderUI({
    list(h2("Data Quality (Under Construction)"),
         h4(input$providerListDQ),
         h4(paste(
           format(input$dq_startdate, "%m-%d-%Y"),
           "to",
           format(updatedate, "%m-%d-%Y")
         )))
  })
  
  output$headerCommunityNeedPH <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$spdatSlider1, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$spdatSlider1, 7, 7) == 1 ~ "03-31-",
        substr(input$spdatSlider1, 7, 7) == 2 ~ "06-30-",
        substr(input$spdatSlider1, 7, 7) == 3 ~ "09-30-",
        substr(input$spdatSlider1, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$spdatSlider1, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Community Need, Entered Permanent Housing"),
      h4(input$regionList1),
      h4(paste(
        format(mdy(ReportStart), "%B %Y"),
        "to",
        format(mdy(ReportEnd), "%B %Y")
      ))
    )
  })
  
  output$headerCommunityNeedCounty <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$spdatSlider2, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$spdatSlider2, 7, 7) == 1 ~ "03-31-",
        substr(input$spdatSlider2, 7, 7) == 2 ~ "06-30-",
        substr(input$spdatSlider2, 7, 7) == 3 ~ "09-30-",
        substr(input$spdatSlider2, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$spdatSlider2, 1, 4)
    )), "%m-%d-%Y")
    
    list(
      h2("Community Need, Literally Homeless in the County"),
      h4(input$regionList2),
      h4(paste(
        format(mdy(ReportStart), "%B %Y"),
        "to",
        format(mdy(ReportEnd), "%B %Y")
      ))
    )
  })
  
  output$headerExitsToPH <- renderUI({
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
    
    list(h2("Successful Placement Detail"),
         h4(input$ExitsToPHProjectList),
         h4(paste(
           format(mdy(ReportStart), "%B %Y"),
           "to",
           format(mdy(ReportEnd), "%B %Y")
         )))
  })
  
  output$headerLoS <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$LoSSlider1, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$LoSSlider1, 7, 7) == 1 ~ "03-31-",
        substr(input$LoSSlider1, 7, 7) == 2 ~ "06-30-",
        substr(input$LoSSlider1, 7, 7) == 3 ~ "09-30-",
        substr(input$LoSSlider1, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$LoSSlider1, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Length of Stay"),
         h4(input$LoSProjectList),
         h4(paste(
           format(mdy(ReportStart), "%B %Y"),
           "to",
           format(mdy(ReportEnd), "%B %Y")
         )))
  })
  
  output$headerIncomeIncrease <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$dateIncomeSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$dateIncomeSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$dateIncomeSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$dateIncomeSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$dateIncomeSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$dateIncomeSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Income Increase"),
         h4(input$incomeProjectList),
         h4(paste(
           format(mdy(ReportStart), "%B %Y"),
           "to",
           format(mdy(ReportEnd), "%B %Y")
         )))
  })
  
  output$headerNCBs <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$dateNCBSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$dateNCBSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$dateNCBSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$dateNCBSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$dateNCBSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$dateNCBSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Non-Cash Benefits at Exit"),
         h4(input$MBProjectListNC),
         h4(paste(
           format(mdy(ReportStart), "%B %Y"),
           "to",
           format(mdy(ReportEnd), "%B %Y")
         )))
  })
  
  output$headerHealthInsurance <- renderUI({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$dateHealthInsuranceSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$dateHealthInsuranceSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$dateHealthInsuranceSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$dateHealthInsuranceSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$dateHealthInsuranceSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$dateHealthInsuranceSlider, 1, 4)
    )), "%m-%d-%Y")
    
    list(h2("Health Insurance at Exit"),
         h4(input$MBProjectListHI),
         h4(paste(
           format(mdy(ReportStart), "%B %Y"),
           "to",
           format(mdy(ReportEnd), "%B %Y")
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
      
      z <-
        paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
      
      a <- ClientUtilizers %>%
        filter(
          ProjectName == input$providerListUtilization,
          served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
                                  MoveInDate, EntryDate)) %>%
        select(PersonalID, BedStart, ExitDate, y)
      z <-
        paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
      
      colnames(a) <- c("Client ID", "Bed Start", "Exit Date", z)
      
      a
      
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
          beds * daysInMonth,
          "possible bed nights"
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
  
  output$DuplicateEEs <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    DuplicateEEs <- DataQualityHMIS %>%
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
    DuplicateEEs <- DataQualityHMIS %>%
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
    HHIssues <- DataQualityHMIS %>%
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
    HHIssues <- DataQualityHMIS %>%
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
          Data Quality corrections. "
        ),
        tableOutput("HouseholdIssues")
      )
    }
    else {
      
    }
  })
  
  output$Overlaps <- renderTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    OverlappingEEs <- DataQualityHMIS %>%
      filter(
        Issue == "Overlapping Project Stays" &
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
        "Exit Date" = ExitDate
      )
    OverlappingEEs
  })
  
  output$DQOverlappingEEs <- renderUI({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    OverlappingEEs <- DataQualityHMIS %>%
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
        HTML(
          "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor
        can they have a Move-In Date into a PSH or RRH project while they are
        still in an ES, TH, or Safe Haven. <br>
        Please look the client(s) up in HMIS and determine which project stay's
        Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may not be your
        project's mistake, but if you are seeing clients here, it means your
        project stay was entered last. <br>
        If the overlap is not your project's mistake, please work with the project that has the
        incorrect Entry/Move-In/or Exit Date to get this corrected or send an
        email to hmis@cohhio.org if you cannot get it resolved. These clients
        will NOT show on their Data Quality app. <br>
        If YOUR dates are definitely correct, it is fine to continue with other
        data corrections as needed."
        ),
        tableOutput("Overlaps")
      )
    }
    else {
      
    }
  })
  
  output$DQErrors <- renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(updatedate, "%m-%d-%Y")
    
    DQErrors <- DataQualityHMIS %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits"
        ) &
          served_between(., ReportStart, ReportEnd) &
          ProjectName == input$providerListDQ &
          Type == "Error"
      ) %>%
      arrange(HouseholdID, PersonalID) %>%
      select("Client ID" = PersonalID,
             "Error" = Issue,
             "Entry Date" =  EntryDate)
    DQErrors
  })
  
  output$DQWarnings <- renderDataTable({
    ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
    ReportEnd <- format.Date(updatedate, "%m-%d-%Y")
    
    DQWarnings <- DataQualityHMIS %>%
      filter(
        !Issue %in% c(
          "Too Many Heads of Household",
          "No Head of Household",
          "Children Only Household",
          "Overlapping Project Stays",
          "Duplicate Entry Exits"
        ) &
          served_between(., ReportStart, ReportEnd) &
          ProjectName == input$providerListDQ &
          Type == "Warning"
      ) %>%
      arrange(HouseholdID, PersonalID) %>%
      select(
        "Client ID" = PersonalID,
        "Warning" = Issue,
        "Entry Date" =  EntryDate
      )
    
    DQWarnings
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
        arrange(ScoreAdjusted) %>%
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
  
  output$ScoredHousedSummary <-
    renderInfoBox({
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
      
      scores <- SPDATsByProject %>%
        filter(entered_between(SPDATsByProject,
                               ReportStart,
                               ReportEnd)) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
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
        ) %>%
        arrange(Score)
      
      CountyAverageScores
      
    })
  
  output$ScoredInRegionSummary <-
    renderInfoBox({
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
      scores <- CountyData %>%
        filter(served_between(CountyData,
                              ReportStart,
                              ReportEnd)) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
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
        filter(((
          !is.na(MoveInDateAdjust) & ProjectType == 13
        ) |
          (
            !is.na(ExitDate) & ProjectType %in% c(1, 2, 8)
          )) &
          exited_between(., ReportStart, ReportEnd) &
          ProjectName == input$LoSProjectList
        ) %>%
        arrange(desc(DaysinProject)) %>%
        select(
          "Client ID" = PersonalID,
          "Bed Start" = EntryAdjust,
          "Exit Date" = ExitDate,
          "Days in Project" = DaysinProject
        )
      
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
      arrange(DestinationGroup, PersonalID) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Bed Start" = BedStart,
        "Exit Date" = ExitDate,
        "Destination Group" =  DestinationGroup
      )
    
    SuccessfulPlacement
    
  })
  
  output$IncomeIncrease <- renderDataTable({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$dateIncomeSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$dateIncomeSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$dateIncomeSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$dateIncomeSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$dateIncomeSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$dateIncomeSlider, 1, 4)
    )), "%m-%d-%Y")
    
    QPR_Income %>%
      filter(ProjectName == input$incomeProjectList &
               served_between(., ReportStart, ReportEnd)) %>%
      mutate(EntryIncome = dollar(EntryIncome),
             RecentIncome = dollar(RecentIncome),
             Difference = dollar(Difference)) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Income at Entry" = EntryIncome,
        "Most Recent Income" = RecentIncome,
        "Increase Amount" = Difference
      )
    
  })
  
  output$ExitedWithNCBs <- renderDataTable({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$dateNCBSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$dateNCBSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$dateNCBSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$dateNCBSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$dateNCBSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$dateNCBSlider, 1, 4)
    )), "%m-%d-%Y")
    
    QPR_MainstreamBenefits %>%
      filter(ProjectName == input$MBProjectListNC &
               exited_between(., ReportStart, ReportEnd)) %>%
      mutate(
        BenefitsFromAnySource = case_when(
          BenefitsFromAnySource == 0 ~ "No (HUD)",
          BenefitsFromAnySource == 1 ~ "Yes (HUD)",
          BenefitsFromAnySource == 8 ~ "Client doesn't know (HUD)",
          BenefitsFromAnySource == 9 ~ "Client refused (HUD)",
          BenefitsFromAnySource == 99 ~ "Data Not Collected (HUD)"
        )
      ) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Benefits from Any Source (at Exit)" = BenefitsFromAnySource
      )
    
  })
  
  output$ExitedWithInsurance <- renderDataTable({
    ReportStart <- format.Date(ymd(paste0(
      substr(input$dateHealthInsuranceSlider, 1, 4),
      "-01-01"
    )), "%m-%d-%Y")
    ReportEnd <- format.Date(mdy(paste0(
      case_when(
        substr(input$dateHealthInsuranceSlider, 7, 7) == 1 ~ "03-31-",
        substr(input$dateHealthInsuranceSlider, 7, 7) == 2 ~ "06-30-",
        substr(input$dateHealthInsuranceSlider, 7, 7) == 3 ~ "09-30-",
        substr(input$dateHealthInsuranceSlider, 7, 7) == 4 ~ "12-31-"
      ),
      substr(input$dateHealthInsuranceSlider, 1, 4)
    )), "%m-%d-%Y")
    
    QPR_MainstreamBenefits %>%
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
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Exit Date" = ExitDate,
        "Health Insurance from Any Source (at Exit)" = InsuranceFromAnySource
      )
    
  })
  
  output$daysToHouseRRH <- renderDataTable({
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
    
    daysToHouse <- RRHEnterers %>%
      filter(
        !is.na(MoveInDateAdjust) &
          ProjectName %in% c(input$RapidRRHProviderList) &
          entered_between(., ReportStart, ReportEnd)
      ) %>%
      arrange(DaysToHouse) %>%
      select(
        "Client ID" = PersonalID,
        "Entry Date" = EntryDate,
        "Move In Date" = MoveInDate,
        "Days to House" = DaysToHouse
      )
    
    daysToHouse
    
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
      
      days <- RRHEnterers %>%
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
  
}