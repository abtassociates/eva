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
# <https://www.gnu.org/licenses/>. test

function(input, output, session) {

  output$goToUpload_btn <- renderUI({
      req(is.null(input$imported))
      actionButton(inputId = 'goToUpload', label = "Upload Hashed CSV")
  })
  
  output$imported_status <- renderUI(input$imported)
  
  # when they click to go to the Upload Hashed CSV tab, it's like they clicked 
  # the sidebar menu tab
  observeEvent(input$goToUpload, {
    updateTabsetPanel(session, "sidebarmenuid", selected = "uploadCSV")
  })
  
  observeEvent(input$imported, {
    
    source("00_functions.R", local = TRUE) # calling in HMIS-related functions that aren't in the HMIS pkg
    Export <- importFile("Export", col_types = "cncccccccTDDcncnnn")

    Client <- importFile("Client",
                         col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")
    
    hashed <- Export$HashStatus == 4 #&
      # min(nchar(Client$FirstName), na.rm = TRUE) ==
      # max(nchar(Client$FirstName), na.rm = TRUE)
    
    if (hashed == FALSE) {
      # clear imported
      showModal(
        modalDialog(
          title = "You uploaded the wrong data set",
          "You have uploaded an unhashed version of the HMIS CSV Export. If you
          are not sure how to run the hashed HMIS CSV Export in your HMIS, please
          contact your HMIS vendor.",
          easyClose = TRUE
        )
      )
    } else {
      withProgress({
        setProgress(message = "Processing...", value = .15)
        setProgress(detail = "Reading your files..", value = .2)
        source("00_get_Export.R", local = TRUE)
        setProgress(detail = "Checking file integrity", value = .35)
        source("00_integrity_checker.R", local = TRUE)
        setProgress(detail = "Prepping initial data..", value = .4)
        source("00_initial_data_prep.R", local = TRUE)
        source("00_dates.R", local = TRUE)
        setProgress(detail = "Making lists..", value = .5)
        source("01_cohorts.R", local = TRUE)
        setProgress(detail = "Assessing your data quality..", value = .7)
        source("03_DataQuality.R", local = TRUE)
        setProgress(detail = "Done!", value = 1)
        
      })
    }
    
    output$integrityChecker <- DT::renderDataTable(
      {
        req(!is.na(input$imported))
        datatable(issues_enrollment)
      })
    
    
    output$headerFileInfo <- renderUI({
        req(!is.na(input$imported))
        HTML(
          paste0(
            "<strong>Date Range of Current File: </strong>",
            format(Export$ExportStartDate, "%m-%d-%Y"),
            " to ",
            format(meta_HUDCSV_Export_End, "%m-%d-%Y"),
            "<p><strong>Export Date: </strong>",
            format(meta_HUDCSV_Export_Date, "%m-%d-%Y at %I:%M %p")
          )
        )
    })
    
    output$headerNoFileYet <- renderUI({
      req(is.na(input$imported))
      HTML("You have not successfully uploaded your zipped CSV file yet.")
    })
    
    updatePickerInput(session = session, inputId = "currentProviderList",
                      choices = sort(Project$ProjectName))
    
    updatePickerInput(session = session, inputId = "desk_time_providers",
                      choices = sort(Project$ProjectName))
    
    updatePickerInput(session = session, inputId = "providerListDQ",
                      choices = dq_providers)
    
    updatePickerInput(session = session, inputId = "providerDeskTime",
                      choices = desk_time_providers)
    
    updatePickerInput(session = session, inputId = "orgList",
                      choices = c(unique(sort(Organization$OrganizationName))))
    
    updateDateInput(session = session, inputId = "dq_org_startdate", 
                    value = meta_HUDCSV_Export_Start)
    
    updateDateInput(session = session, inputId = "dq_startdate", 
                    value = meta_HUDCSV_Export_Start)

    # output$files <- renderTable(input$imported)
    
    output$headerPrioritization <- renderUI({
      req(!is.na(input$imported))
      list(h2("Prioritization Report"),
           h4("Literally Homeless Clients as of", meta_HUDCSV_Export_End))
    })

    output$headerDataQuality <- renderUI({
      req(!is.na(input$imported))
      list(h2("Data Quality"),
           h4(paste(
             format(input$dq_startdate, "%m-%d-%Y"),
             "to",
             format(meta_HUDCSV_Export_Date, "%m-%d-%Y")
           )))
    })
    
    output$DeskTimePlotDetail <- renderPlot({
      req(!is.na(input$imported))
      provider <- input$providerDeskTime
      
      ReportStart <- ymd(today() - years(1))
      ReportEnd <- ymd(today())
      
      desk_time <- validation %>%
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
      req(!is.na(input$imported))
      provider <- input$providerDeskTimeCoC
      
      ReportStart <- ymd(today() - years(1))
      ReportEnd <- ymd(today())
      
      desk_time <- validation %>%
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
    
    output$clientCountData <- DT::renderDataTable({
      req(!is.na(input$imported))
      ReportStart <- input$dateRangeCount[1]
      ReportEnd <- input$dateRangeCount[2]
      
      datatable(
        validation %>%
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
      req(!is.na(input$imported))
      ReportStart <- input$dateRangeCount[1]
      ReportEnd <- input$dateRangeCount[2]
      
      hhs <- validation %>%
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
            TRUE ~ "something's wrong"
          )
        ) %>%
        group_by(Status) %>%
        summarise(Households = n())
      
      clients <- validation %>%
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
    
    # output$bedPlot <- renderPlotly({
    #   ReportEnd <- ymd(input$utilizationDate) 
    #   ReportStart <- floor_date(ymd(ReportEnd), unit = "month") -
    #     years(1) +
    #     months(1)
    #   ReportingPeriod <- interval(ymd(ReportStart), ymd(ReportEnd))
    #   
    #   Provider <- input$providerListUtilization
    #   
    #   bedPlot <- utilization_bed %>% 
    #     gather("Month",
    #            "Utilization",
    #            -ProjectID,
    #            -ProjectName,
    #            -ProjectType) %>%
    #     filter(ProjectName == Provider,
    #            mdy(Month) %within% ReportingPeriod) %>%
    #     mutate(
    #       Month = floor_date(mdy(Month), unit = "month"),
    #       Bed = Utilization,
    #       Utilization = NULL
    #     )
    #   
    #   unitPlot <- utilization_unit %>% 
    #     gather("Month",
    #            "Utilization",
    #            -ProjectID,
    #            -ProjectName,
    #            -ProjectType) %>%
    #     filter(ProjectName == Provider,
    #            mdy(Month) %within% ReportingPeriod) %>%
    #     mutate(
    #       Month = floor_date(mdy(Month), unit = "month"),
    #       Unit = Utilization,
    #       Utilization = NULL
    #     )
    #   
    #   utilizationPlot <- unitPlot %>%
    #     full_join(bedPlot,
    #               by = c("ProjectID", "ProjectName", "ProjectType", "Month")) 
    #   
    #   plot_ly(utilizationPlot, 
    #           x = ~Month) %>%
    #     add_trace(y = ~ Unit,
    #               name = "Unit Utilization",
    #               type = "scatter",
    #               mode = "lines+markers",
    #               hoverinfo = 'y') %>%
    #     add_trace(y = ~Bed,
    #               name = "Bed Utilization",
    #               type = "scatter",
    #               mode = "lines+markers",
    #               hoverinfo = 'y') %>%
    #     layout(yaxis = list(
    #       title = "Utilization",
    #       tickformat = "%",
    #       range = c(0, 2)
    #     ),
    #     margin = list(
    #       t = 100
    #     ),
    #     title = paste("Bed and Unit Utilization",
    #                   "\n", 
    #                   Provider,
    #                   "\n", 
    #                   format(ymd(ReportStart), "%B %Y"), 
    #                   "to", 
    #                   format(ymd(ReportEnd), "%B %Y")))
    #   
    # })  
    # 
    # output$unitNote <- renderUI(note_unit_utilization)
    # 
    # output$bedNote <- renderUI(note_bed_utilization)
    # 
    # output$utilizationNote <- renderUI(HTML(note_calculation_utilization))
    # 
    # output$utilizationDetail <- DT::renderDataTable({
    #   ReportStart <-
    #     floor_date(ymd(input$utilizationDate),
    #                unit = "month")
    #   ReportEnd <-
    #     floor_date(ymd(input$utilizationDate) + days(31),
    #                unit = "month") - days(1)
    #   
    #   y <- paste0(substr(input$utilizationDate, 6, 7),
    #               "01",
    #               substr(input$utilizationDate, 1, 4))
    #   
    #   z <-
    #     paste("Bed Nights in", format(ymd(input$utilizationDate), "%B %Y"))
    #   # input <- list(providerListUtilization = sample(c(sort(utilization_bed$ProjectName)), 1))
    #   a <- utilizers_clients %>%
    #     filter(
    #       ProjectName == input$providerListUtilization,
    #       served_between(., ReportStart, ReportEnd)
    #     ) %>%
    #     mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
    #                               MoveInDate, EntryDate),
    #            PersonalID = as.character(PersonalID)) %>%
    #     select(PersonalID, BedStart, ExitDate, all_of(y))
    #   
    #   colnames(a) <- c("Client ID", "Bed Start", "Exit Date", z)
    #   
    #   datatable(a,
    #             rownames = FALSE,
    #             filter = 'top',
    #             options = list(dom = 'ltpi'))
    #   
    # })
    # 
    # output$utilizationSummary0 <- renderInfoBox({
    #   ReportStart <-
    #     floor_date(ymd(input$utilizationDetailDate),
    #                unit = "month")
    #   ReportEnd <-
    #     floor_date(ymd(input$utilizationDetailDate) + days(31),
    #                unit = "month") - days(1)
    #   
    #   y <- paste0(substr(input$utilizationDetailDate, 6, 7),
    #               "01",
    #               substr(input$utilizationDetailDate, 1, 4))
    #   
    #   a <- utilizers_clients %>%
    #     filter(
    #       ProjectName == input$providerListUtilization,
    #       served_between(., ReportStart, ReportEnd)
    #     ) %>%
    #     mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
    #                               MoveInDate, EntryDate)) %>%
    #     select(PersonalID, BedStart, ExitDate, all_of(y))
    #   
    #   colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
    #   
    #   beds <- Beds %>%
    #     filter(ProjectName == input$providerListUtilization &
    #              beds_available_between(., ReportStart, ReportEnd)) %>%
    #     group_by(ProjectID) %>%
    #     summarise(BedCount = sum(BedInventory)) %>%
    #     ungroup() %>%
    #     pull(BedCount)
    #   
    #   daysInMonth <- days_in_month(ymd(input$utilizationDetailDate))
    #   
    #   infoBox(
    #     title = "Total Bed Nights Served",
    #     color = "purple",
    #     icon = icon("bed"),
    #     value = sum(a$BNs),
    #     subtitle = "See table below for detail."
    #   )
    # })
    # 
    # output$utilizationSummary1 <- renderInfoBox({
    #   ReportStart <-
    #     floor_date(ymd(input$utilizationDetailDate),
    #                unit = "month")
    #   ReportEnd <-
    #     floor_date(ymd(input$utilizationDetailDate) + days(31),
    #                unit = "month") - days(1)
    #   
    #   y <- paste0(substr(input$utilizationDetailDate, 6, 7),
    #               "01",
    #               substr(input$utilizationDetailDate, 1, 4))
    #   
    #   a <- utilizers_clients %>%
    #     filter(
    #       ProjectName == input$providerListUtilization,
    #       served_between(., ReportStart, ReportEnd)
    #     ) %>%
    #     mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
    #                               MoveInDate, EntryDate)) %>%
    #     select(PersonalID, BedStart, ExitDate, all_of(y))
    #   
    #   colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
    #   
    #   beds <- Beds %>%
    #     filter(ProjectName == input$providerListUtilization &
    #              beds_available_between(., ReportStart, ReportEnd)) %>%
    #     group_by(ProjectID) %>%
    #     summarise(BedCount = sum(BedInventory)) %>%
    #     ungroup() %>%
    #     pull(BedCount)
    #   
    #   # units <- Utilization %>%
    #   #   filter(ProjectName == input$providerListUtilization) %>%
    #   #   select(UnitCount)
    #   
    #   daysInMonth <- days_in_month(ymd(input$utilizationDetailDate))
    #   
    #   infoBox(
    #     title = "Possible Bed Nights",
    #     color = "purple",
    #     icon = icon("bed"),
    #     value = beds * daysInMonth,
    #     subtitle = paste(
    #       "Bed Count:",
    #       beds,
    #       "beds ×",
    #       daysInMonth,
    #       "days in",
    #       format(ymd(input$utilizationDetailDate), "%B"),
    #       "=",
    #       beds * daysInMonth
    #     )
    #   )
    # })
    # 
    # output$utilizationSummary2 <- renderInfoBox({
    #   ReportStart <-
    #     floor_date(ymd(input$utilizationDetailDate),
    #                unit = "month")
    #   ReportEnd <-
    #     floor_date(ymd(input$utilizationDetailDate) + days(31),
    #                unit = "month") - days(1)
    #   
    #   y <- paste0(substr(input$utilizationDetailDate, 6, 7),
    #               "01",
    #               substr(input$utilizationDetailDate, 1, 4))
    #   
    #   a <- utilizers_clients %>%
    #     filter(
    #       ProjectName == input$providerListUtilization,
    #       served_between(., ReportStart, ReportEnd)
    #     ) %>%
    #     mutate(BedStart = if_else(ProjectType %in% c(3, 9, 13),
    #                               MoveInDate, EntryDate)) %>%
    #     select(PersonalID, BedStart, ExitDate, all_of(y))
    #   
    #   colnames(a) <- c("Client ID", "Bed Start", "Exit Date", "BNs")
    #   
    #   beds <- Beds %>%
    #     filter(ProjectName == input$providerListUtilization &
    #              beds_available_between(., ReportStart, ReportEnd)) %>%
    #     group_by(ProjectID) %>%
    #     summarise(BedCount = sum(BedInventory)) %>%
    #     ungroup() %>%
    #     pull(BedCount)
    #   
    #   daysInMonth <-
    #     as.numeric(days_in_month(ymd(input$utilizationDetailDate)))
    #   
    #   bedUtilization <- percent(sum(a$BNs) / (beds * daysInMonth))
    #   
    #   infoBox(
    #     title = "Bed Utilization",
    #     color = "teal",
    #     icon = icon("bed"),
    #     value = bedUtilization,
    #     subtitle = paste(sum(a$BNs),
    #                      "÷",
    #                      beds * daysInMonth,
    #                      "=",
    #                      bedUtilization)
    #   )
    # })
    
    output$dq_provider_summary_table <- DT::renderDataTable({
      req(input$imported)
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      guidance <- dq_main %>%
        filter(OrganizationName %in% c(input$orgList) &
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
                options = list(dom = 'ltpi'))
    })
    
    output$dq_organization_summary_table <- DT::renderDataTable({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      a <- dq_main %>%
        filter(OrganizationName == input$orgList &
                 HMIS::served_between(., ReportStart, ReportEnd)) %>%
        select(ProjectName, 
               Type, 
               Issue, 
               PersonalID) %>%
        group_by(ProjectName, 
                 Type, 
                 Issue) %>%
        summarise(Clients = n()) %>%
        select("Project Name" = ProjectName, 
          Type, 
          Issue, 
          Clients) %>%
        arrange("Project Name", Type, desc(Clients))
      
      datatable(
        a,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
    
    output$DuplicateEEs <- renderTable({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      DuplicateEEs <- dq_main %>%
        filter(
          Issue == "Duplicate Enrollments" &
            OrganizationName %in% c(input$orgList) &
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
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      DuplicateEEs <- dq_main %>%
        filter(
          Issue == "Duplicate Enrollments" &
            OrganizationName %in% c(input$orgList) &
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
          title = "Duplicate Enrollments",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          HTML(
            "Please correct this issue before moving on to your other errors.<br>
         Duplicate Enrollments are created when the user clicks \"Add Entry Exit\"
         instead of clicking the Entry pencil to get back into an assessment.
         These must be deleted for each member of the household. Please take
         care to not delete Enrollments with valid Interims attached."
          ),
          tableOutput("DuplicateEEs")
        )
      }
      else {
        
      }
    })
    
    output$HouseholdIssues <- renderTable({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      HHIssues <- dq_main %>%
        filter(
          Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household"
          ) &
            OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y"),
          MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
          ExitDate = format(ExitDate, "%m-%d-%Y")
        ) %>%
        arrange(ProjectName, PersonalID) %>%
        select("Project Name" = ProjectName,
               "A Client ID in the Household" = PersonalID,
               Issue,
               "Project Start Date" = EntryDate)
      
      HHIssues
    })
    
    output$DQHHIssues <- renderUI({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      HHIssues <- dq_main %>%
        filter(
          Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household"
          ) &
            OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd)
        )
      if (nrow(HHIssues) > 0) {
        box(
          id = "hhs",
          title = "Household Issues",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
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
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      HHIssues <- dq_main %>%
        filter(
          Issue == "Missing Client Location" &
            OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd)
        )
      if (nrow(HHIssues) > 0) {
        box(
          id = "location",
          title = "Missing Client Location",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
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
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      HHIssues <- dq_main %>%
        filter(
          Issue == "Missing Client Location" &
            OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y"),
          MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
          ExitDate = format(ExitDate, "%m-%d-%Y")
        ) %>%
        arrange(ProjectName, PersonalID) %>%
        select("Project Name" = ProjectName,
               "Client ID" = PersonalID,
               "Project Start Date" = EntryDate)
      
      HHIssues
    })
    
    output$DQPATHMissingContact <- renderUI({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      no_contact <- dq_main %>%
        filter(
          Issue == "Missing PATH Contact" &
            OrganizationName %in% c(input$orgList) &
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
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today() 
      x <- dq_main %>%
        filter(
          Issue == "Missing PATH Contact" &
            OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y"),
          MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
          ExitDate = format(ExitDate, "%m-%d-%Y")
        ) %>%
        arrange(ProjectName, PersonalID) %>%
        select("Project Name" = ProjectName,
               "Client ID" = PersonalID,
               "Project Start Date" = EntryDate)
      
      x
    })
    
    output$Overlaps <- renderTable({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      OverlappingEEs <- dq_overlaps %>%
        filter(
          OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y"),
          MoveInDateAdjust = format(MoveInDateAdjust, "%m-%d-%Y"),
          ExitDate = format(ExitDate, "%m-%d-%Y")
        ) %>%
        arrange(ProjectName, PersonalID) %>%
        select(
          "Project Name" = ProjectName,
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Move In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Overlaps With This Provider's Stay" = PreviousProject
        )
      OverlappingEEs
    })
    
    output$DQOverlappingEEs <- renderUI({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      # FIXME Repetition of filter/mutate (use eventReactive)
      OverlappingEEs <- dq_overlaps %>%
        filter(
          Issue == "Overlapping Project Stays" &
            OrganizationName %in% c(input$orgList) &
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
          title = "Overlapping Enrollments",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
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
    
    output$cocOverlap <- DT::renderDataTable({
      req(!is.na(input$imported))
      ReportStart <- hc_check_dq_back_to
      ReportEnd <- today()
      
      a <- dq_overlaps %>%
        filter(served_between(., ReportStart, ReportEnd)) %>%
        group_by(ProjectName) %>%
        summarise(Clients = n()) %>%
        arrange(desc(Clients)) %>%
        top_n(20L, wt = Clients) %>%
        select("Project Name" = ProjectName,
               "Clients with Overlapping Enrollments" = Clients)
      datatable(a,
                rownames = FALSE)
    })
    
    output$cocLongStayers <- DT::renderDataTable({
      req(!is.na(input$imported))
      ReportStart <- meta_HUDCSV_Export_Start
      ReportEnd <- today()
      
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
    
    output$cocRRHDestination <- DT::renderDataTable({
      req(!is.na(input$imported))
      ReportStart <- hc_check_dq_back_to
      ReportEnd <- today()
      
      a <- dq_main %>%
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
      req(!is.na(input$imported))
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
    
    output$systemDQErrors <- renderPlot({
      req(!is.na(input$imported))
      dq_plot_projects_errors})
    
    output$systemHHErrors <- renderPlot({
      req(!is.na(input$imported))
      dq_plot_hh_errors})
    
    output$systemDQWarnings <- renderPlot({
      req(!is.na(input$imported))
      dq_plot_projects_warnings})
    
    output$systemDQErrorTypes <- renderPlot({
      req(!is.na(input$imported))
      dq_plot_errors})
    
    output$systemDQWarningTypes <- renderPlot({
      req(!is.na(input$imported))
      dq_plot_warnings})
    
    output$systemDQEligibility <- renderPlot({
      req(!is.na(input$imported))
      dq_plot_eligibility})
    
    output$Ineligible <- renderTable({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      Ineligible <- detail_eligibility %>%
        filter(OrganizationName %in% c(input$orgList) &
                 served_between(., ReportStart, ReportEnd)) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y"),
          PreviousStreetESSH = if_else(PreviousStreetESSH == 1, "Yes", "No")
        ) %>%
        select(
          "Project Name" = ProjectName,
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Residence Prior" = ResidencePrior,
          "Length of Stay" = LengthOfStay,
          "Literally Homeless Prior" = PreviousStreetESSH
        )
      Ineligible
    })
    
    output$DQIneligible <- renderUI({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      Ineligible <- detail_eligibility %>%
        filter(OrganizationName %in% c(input$orgList) &
                 served_between(., ReportStart, ReportEnd))
      
      if (nrow(Ineligible) > 0) {
        box(
          id = "eligibility",
          title = "Check Eligibility",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
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
    
    output$DQErrors <- DT::renderDT({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      DQErrors <- dq_main %>%
        filter(
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Project Stays",
            "Duplicate Enrollments",
            "Access Point with Enrollments"
          ) & # because these are all in the boxes already
            OrganizationName %in% c(input$orgList) &
            served_between(., ReportStart, ReportEnd) &
            Type == "Error"
        ) %>%
        arrange(ProjectName, HouseholdID, PersonalID) %>%
        select("Project Name" = ProjectName,
               "Client ID" = PersonalID,
               "Error" = Issue,
               "Project Start Date" =  EntryDate)
      
      datatable(
        DQErrors,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
      
    })
    
    output$DQWarnings <- DT::renderDataTable({
      req(!is.na(input$imported))
      ReportStart <- input$dq_startdate
      ReportEnd <- today()
      
      DQWarnings <- dq_main %>%
        filter(
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Project Stays",
            "Duplicate Enrollments",
            "Check Eligibility"
          ) &
            served_between(., ReportStart, ReportEnd) &
            OrganizationName %in% c(input$orgList) &
            Type == "Warning"
        ) %>%
        mutate(PersonalID = as.character(PersonalID)) %>%
        arrange(ProjectName, HouseholdID, PersonalID) %>%
        select(
          "Project Name" = ProjectName,
          "Client ID" = PersonalID,
          "Warning" = Issue,
          "Project Start Date" =  EntryDate
        )
      
      datatable(
        DQWarnings,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi'))
    })

  output$headerCurrent <- renderUI({
    req(!is.na(input$imported))
    list(h2("Client Counts Report"),
         h4(input$currentProviderList))
  })
  
  output$headerUtilization <- renderUI({
    req(!is.na(input$imported))
    list(h2("Bed and Unit Utilization"),
         h4(input$providerListUtilization),
         h4(format(ymd(
           input$utilizationDate
         ), "%B %Y"))
         )
  })
  
  output$headerDeskTime <- renderUI({
    req(!is.na(input$imported))
    list(h2("Data Entry Timeliness"),
         h4(input$providersDeskTime),
         h4(paste("Fixed Date Range:",
                  format(today() - years(1), "%m-%d-%Y"),
                  "to",
                  format(today(), "%m-%d-%Y"))))
  })
  
  output$headerExitsToPH <- renderUI({
    req(!is.na(input$imported))
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
  
  output$headerOrganizationDQ <- renderUI({
    req(!is.na(input$imported))
    list(h2("Data Quality Summary (Organization)"),
         h4(paste(
           format(input$dq_startdate, "%m-%d-%Y"),
           "to",
           format(meta_HUDCSV_Export_Date, "%m-%d-%Y")
         )))
  })
  
  output$headerSystemDQ <- renderUI({
    req(!is.na(input$imported))
    list(h2("System-wide Data Quality"),
         h4(
           paste(format(meta_HUDCSV_Export_Start, "%m-%d-%Y"),
                 "through",
                 format(meta_HUDCSV_Export_End, "%m-%d-%Y"))
         ))
  })
  
  output$deskTimeNote <- renderUI({
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
    )})
  
  # output$headerCocDQ <- renderUI({
  #   req(!is.null(input$imported))
  #   list(h2("System-wide Data Quality"),
  #        h4(
  #          paste(format(meta_HUDCSV_Export_Start, "%m-%d-%Y"),
  #                "through",
  #                format(meta_HUDCSV_Export_End, "%m-%d-%Y"))
  #        ))
  # })
  
  }, ignoreInit = TRUE)

  
  
  # output$cocDQErrors <- renderPlot(dq_plot_projects_errors)
  # 
  # output$cocHHErrors <- renderPlot(dq_plot_hh_errors)
  # 
  # output$cocUnshelteredHigh <- renderPlot(dq_plot_unsheltered_high)
  # 
  # output$cocDQWarnings <- renderPlot(dq_plot_projects_warnings)
  # 
  # output$cocDQErrorTypes <- renderPlot(dq_plot_errors)
  # 
  # output$cocDQWarningTypes <- renderPlot(dq_plot_warnings)
  # 
  # output$cocEligibility <- renderPlot(dq_plot_eligibility)
  # 
  # output$dq_plot_outstanding_referrals <- renderPlot(dq_plot_outstanding_referrals)
  
 }
