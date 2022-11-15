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
  
  valid_file <- reactiveVal(0)

  output$headerNoFileYet <- renderUI({
    req(valid_file() == 0)
    HTML("You have not successfully uploaded your zipped CSV file yet.")
  })

  observeEvent(input$imported, {
    
    source("00_functions.R", local = TRUE) # calling in HMIS-related functions that aren't in the HMIS pkg

    # read Export file
    Export <- importFile("Export", col_types = "cncccccccTDDcncnnn")
    # read Client file
    Client <- importFile("Client",
                         col_types = "cccccncnDnnnnnnnnnnnnnnnnnnnnnnnnnnnTTcTc")
    # decide if the export is hashed
    hashed <-  
      # TRUE
      Export$HashStatus == 4 &
       min(nchar(Client$FirstName), na.rm = TRUE) ==
       max(nchar(Client$FirstName), na.rm = TRUE)
    
    #if it's not hashed, throw an error and clear the upload
    if (hashed == FALSE) {
      # clear imported
      valid_file(0)
      reset("imported")
      showModal(
        modalDialog(
          title = "You uploaded the wrong data set",
          "You have uploaded an unhashed version of the HMIS CSV Export. If you
          are not sure how to run the hashed HMIS CSV Export in your HMIS, please
          contact your HMIS vendor.",
          easyClose = TRUE
        )
      )
    } else { # if it is hashed, set imported_zip to 1 and start running scripts
      valid_file(1)
      hide('imported_progress')
      
      withProgress({
        setProgress(message = "Processing...", value = .15)
        setProgress(detail = "Reading your files..", value = .2)
        source("00_get_Export.R", local = TRUE)
        source("00_dates.R", local = TRUE)
        setProgress(detail = "Checking file integrity", value = .35)
        source("00_integrity_checker.R", local = TRUE)
        # if structural issues were not found, keep going
        if (structural_issues == 0) {
          valid_file(1)
          setProgress(detail = "Prepping initial data..", value = .4)
          source("00_initial_data_prep.R", local = TRUE)
          source("00_dates.R", local = TRUE)
          setProgress(detail = "Making lists..", value = .5)
          source("01_cohorts.R", local = TRUE)
          setProgress(detail = "Assessing your data quality..", value = .7)
          source("03_DataQuality.R", local = TRUE)
          setProgress(detail = "Checking your PDDEs", value = .85)
          source("00_PDDE_Checker.R", local = TRUE)
          setProgress(detail = "Done!", value = 1)
        } else{ # if structural issues were found, reset gracefully
        valid_file(0)
        reset("imported")
        showModal(
          modalDialog(
            title = "Your HMIS CSV Export is not structurally valid",
            "Your HMIS CSV Export has some High Priority issues that must
            be addressed by your HMIS Vendor. Please download the Integrity
            Checker for details.",
            easyClose = TRUE
          )
        )
      }
      })
    } 
    
    dq_main_reactive <- reactive({
      req(valid_file()== 1)
      # browser()
      ESNbN <- calculate_long_stayers(input$ESNbNLongStayers, 0)
      Other <- calculate_long_stayers(input$OtherLongStayers, 7)
      Outreach <- calculate_long_stayers(input$OUTLongStayers, 4)
      DayShelter <- calculate_long_stayers(input$DayShelterLongStayers, 11)
      ServicesOnly <- calculate_long_stayers(input$ServicesOnlyLongStayers, 6)
      
      x <- dq_main %>%
        filter(!Issue %in% c("Days Enrollment Active Exceeds CoC-specific Settings"))
      
      rbind(x, ESNbN, Outreach, DayShelter, ServicesOnly, Other)
      
    })
    
    output$integrityChecker <- DT::renderDataTable(
      {
        req(hashed == 1)
        a <- rbind(integrity_client,
                   integrity_enrollment,
                   integrity_living_situation,
                   integrity_structure) %>%
          group_by(Issue, Type) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          arrange(desc(Type))
        
        datatable(
          a,
          rownames = FALSE,
          filter = 'none',
          options = list(dom = 't')
        )
      })
    
    output$downloadIntegrityBtn <- renderUI({
      req(hashed == 1)
      downloadButton("downloadIntegrityCheck", "Download Integrity Check Detail")
    })  
    
    output$downloadIntegrityCheck <- downloadHandler(
      # req(valid_file() == 1)

      filename = function() {
        paste("integrity-check-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(
          rbind(
            integrity_client,
            integrity_enrollment,
            integrity_living_situation,
            integrity_structure
          ),
          path = file
        )
      }
    )
    
    output$headerFileInfo <- renderUI({
      req(valid_file() == 1)
      HTML(
        paste0(
          "<p>You have successfully uploaded your hashed HMIS CSV Export!</p>
            <p><strong>Date Range of Current File: </strong>",
          format(Export$ExportStartDate, "%m-%d-%Y"),
          " to ",
          format(meta_HUDCSV_Export_End, "%m-%d-%Y"),
          "<p><strong>Export Date: </strong>",
          format(meta_HUDCSV_Export_Date, "%m-%d-%Y at %I:%M %p")
        )
      )
    })
    
    if(valid_file() == 1) {
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
      
      updateDateRangeInput(session = session, inputId = "dateRangeCount",
                           min = meta_HUDCSV_Export_Start)
    }

    output$headerDataQuality <- renderUI({
      req(valid_file() == 1)
      list(h2("Data Quality"),
           h4(paste(
             format(Export$ExportStartDate, "%m-%d-%Y"),
             "to",
             format(meta_HUDCSV_Export_End, "%m-%d-%Y")
           )))
    })
    
    ##### PDDE Checker-----
    # header
    output$headerPDDE <- renderUI({
      req(valid_file() == 1)
      list(h2("Project Descriptor Data Elements Checker"),
           h4(paste(
             format(meta_HUDCSV_Export_Start, "%m-%d-%Y"),
             "to",
             format(meta_HUDCSV_Export_End, "%m-%d-%Y")
           )))
    })
    
    # summary table
    output$pdde_summary_table <- DT::renderDataTable({
      req(valid_file() == 1)
      
      datatable(
        pdde_main %>%
          group_by(Issue, Type) %>%
          summarise(Count = n()) %>%
          ungroup(),
        rownames = FALSE,
        filter = 'none',
        options = list(dom = 't')
      )
    })
    
    # download button
    output$downloadPDDEReportButton  <- renderUI({
      req(valid_file() == 1)
      
      downloadButton(outputId = "downloadPDDEReport",
                       label = "Download")
    })
    
    # download button handler
    output$downloadPDDEReport <- downloadHandler(
      
      filename = function() {
        paste("PDDE Report-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        req(valid_file() == 1)
        
        summary <- pdde_main %>% 
          group_by(Issue, Type) %>%
          summarise(Count = n()) %>%
          ungroup()

        write_xlsx(list("Summary" = summary, "Data" = pdde_main), path = file)
      }
    )
    
    
    output$DeskTimePlotDetail <- renderPlot({
      req(valid_file() == 1)
      provider <- input$providerDeskTime
      
      ReportStart <- ymd(meta_HUDCSV_Export_Start - years(1))
      ReportEnd <- ymd(meta_HUDCSV_Export_End)
      
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
    
    output$clientCountData <- DT::renderDataTable({
      req(valid_file() == 1)
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
                !is.na(ExitDate) ~ "Exited project"
            ),
            sort = today() - EntryDate,
            EntryDate = format.Date(EntryDate, "%m-%d-%Y"),
            MoveInDateAdjust = format.Date(MoveInDateAdjust, "%m-%d-%Y"),
            ExitDate = format.Date(ExitDate, "%m-%d-%Y")
          ) %>%
        #  mutate(PersonalID = as.character(PersonalID)) %>%
          arrange(desc(sort), HouseholdID, PersonalID) %>%
          select(
            "Personal ID" = PersonalID,
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
      req(valid_file() == 1)
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
              !is.na(ExitDate) ~ "Exited project"
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
    
    output$dq_org_guidance_summary <- DT::renderDataTable({
      req(valid_file() == 1)
      
      guidance <- dq_main_reactive() %>%
        filter(OrganizationName %in% c(input$orgList)) %>%
        select(Type, Issue, Guidance) %>%
        mutate(Type = factor(Type, levels = c("High Priority",
                                              "Error",
                                              "Warning"))) %>%
        arrange(Type, Issue) %>%
        unique()
      
      datatable(guidance, 
                rownames = FALSE,
                escape = FALSE,
                options = list(dom = 'ltpi'))
    })
    
    output$dq_organization_summary_table <- DT::renderDataTable({
      req(valid_file() == 1)
      
      a <- dq_main_reactive() %>%
        filter(OrganizationName %in% c(input$orgList)) %>%
        select(ProjectName, 
               Type, 
               Issue, 
               PersonalID) %>%
        group_by(ProjectName, 
                 Type, 
                 Issue) %>%
        summarise(Clients = n()) %>%
        arrange(Type, desc(Clients)) %>%
        select("Project Name" = ProjectName, 
          Type, 
          Issue, 
          Clients)
      
      datatable(
        a,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
    
    #### DQ ORG REPORT #### ----------------------
    # button
    output$downloadOrgDQReportButton  <- renderUI({
      if (valid_file() == 1) {
        downloadButton(outputId = "downloadOrgDQReport",
                       label = "Download")
      }
    })
    
    # list of data frames to include in DQ Org Report
    orgDQReportDataList <- reactive({
      req(valid_file() == 1)
      
      orgDQData <- dq_main_reactive() %>%
        filter(OrganizationName %in% c(input$orgList))
      
      orgDQoverlaps <- overlapNEW %>%
        filter(OrganizationName.x %in% c(input$orgList) | OrganizationName.y %in% c(input$orgList))
      
      getDQReportDataList(orgDQData, orgDQoverlaps)
    })
    
    fullDQReportDataList <- reactive({
      req(valid_file() == 1)
      getDQReportDataList(dq_main_reactive(), overlapNEW)
    })
    
    output$downloadOrgDQReport <- downloadHandler(
      filename = function() {
        paste("Organization Data Quality Report-",
              Sys.Date(),
              ".xlsx",
              sep = "")
      },
      content = function(file) {write_xlsx(orgDQReportDataList(), path = file)}
    )
    
# 
#     output$cocOverlap <- DT::renderDataTable({
# 
#       a <- dq_overlaps %>%
#         group_by(ProjectName) %>%
#         summarise(Clients = n()) %>%
#         arrange(desc(Clients)) %>%
#         top_n(20L, wt = Clients) %>%
#         select("Project Name" = ProjectName,
#                "Clients with Overlapping Enrollments" = Clients)
#       datatable(a,
#                 rownames = FALSE)
#     }) #revisit
    
    # output$cocWidespreadIssues <- DT::renderDataTable({
    #   req(valid_file() == 1)
    #   a <- dq_past_year() %>%
    #     select(Issue, ProjectName, Type) %>%
    #     unique() %>%
    #     group_by(Issue, Type) %>%
    #     summarise(HowManyProjects = n()) %>%
    #     arrange(desc(HowManyProjects)) %>%
    #     head(10L) %>%
    #     select(Issue, Type, "How Many Providers" = HowManyProjects)
    #   
    #   datatable(a,
    #             rownames = FALSE)
    # 
    # })
 
    #System-Level tab plots
       
    output$systemDQHighPriorityErrors <- renderPlot({
      req(valid_file() == 1)
      dq_plot_organizations_high_priority_errors})
    
    output$systemDQHighPriorityErrorTypes <- renderPlot({
      req(valid_file() == 1)
      dq_plot_high_priority_errors_org_level})
    
    output$systemDQErrors <- renderPlot({
      req(valid_file() == 1)
      dq_plot_organizations_errors})
    
    output$systemDQErrorTypes <- renderPlot({
      req(valid_file() == 1)
      dq_plot_errors_org_level})

    output$systemDQWarnings <- renderPlot({
      req(valid_file() == 1)
      dq_plot_organizations_warnings})
    
    output$systemDQWarningTypes <- renderPlot({
      req(valid_file() == 1)
      dq_plot_warnings_org_level})
    
    #ORG-LEVEL TAB PLOTS
    #Create reactive data sets for plots
    dq_hp_top_projects <- reactive({
      dq_hp_top_projects_r <- dq_data_high_priority_errors_top_projects_df %>%
        filter(OrganizationName %in% c(input$orgList))
    })
    
    dq_hp_error_types_org_level <- reactive({
      dq_hp_error_types_org_level_r <- dq_data_high_priority_error_types_org_df %>%
        filter(OrganizationName %in% c(input$orgList))
    })
    
    dq_general_errors_top_projects <- reactive({
      dq_general_errors_top_projects_r <- dq_data_errors_top_projects_df %>%
        filter(OrganizationName %in% c(input$orgList))
    })
    
    dq_general_error_types_org_level <- reactive({
      dq_general_error_types_org_level_r <- dq_data_error_types_org_df %>%
        filter(OrganizationName %in% c(input$orgList))
    })
    
    dq_warnings_top_projects <- reactive({
      dq_warnings_top_projects_r <- dq_data_warnings_top_projects_df %>%
        filter(OrganizationName %in% c(input$orgList))
    })
    
    dq_warning_types_org_level <- reactive({
      dq_warning_types_org_level_r <- dq_data_warning_types_org_df %>%
        filter(OrganizationName %in% c(input$orgList))
    })
    
    #Validate for "empty" org-level plots
    output$dq_hp_errors_null <- renderUI({
      if (nrow(dq_hp_error_types_org_level()) == 0)
        print("Good work! There are no high priority errors to show.")
    })
    
    output$dq_general_errors_null <- renderUI({
      if (nrow(dq_general_error_types_org_level()) == 0)
        print("Good work! There are no general errors to show.")
    })
    
    output$dq_warnings_null <- renderUI({
      if (nrow(dq_warning_types_org_level()) == 0)
        print("Good work! There are no warnings to show.")
    })
    
    #Controls org-level plot heights reactively
    plotHeight_hp_errors <- reactive({
      if (nrow(dq_hp_error_types_org_level()) == 0)
      {plotHeight_hp_errors = 50}
      else {plotHeight_hp_errors = 400}
    })
    
    plotHeight_general_errors <- reactive({
      if (nrow(dq_general_error_types_org_level()) == 0)
      {plotHeight_general_errors = 50}
      else {plotHeight_general_errors = 400}
    })
    
    plotHeight_warnings <- reactive({
      if (nrow(dq_warning_types_org_level()) == 0)
      {plotHeight_warnings = 50}
      else {plotHeight_warnings = 400}
    })
    
    #Plot of projects within selected org with most high priority errors
    
    output$orgDQHighPriorityErrors <- renderPlot({
      req(valid_file() == 1,
          nrow(dq_hp_top_projects()) > 0)
      
      # dq_hp_top_projects()$hover <-
      #   with(dq_hp_top_projects(),
      #        paste0(ProjectName))
      # 
      ggplot(
        head(dq_hp_top_projects(), 10L),
        aes(
          x = reorder(ProjectName, clientsWithErrors),
          y = clientsWithErrors
        )
      ) +
        geom_col(show.legend = FALSE,
                 color = "#063a89",
                 fill = "#063a89") +
        coord_flip() +
        labs(title = "Projects with the Most High Priority Errors",
             x = "",
             y = "Number of Clients with High Priority Errors") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        theme_classic() +
        theme(axis.line = element_line(linetype = "blank"),
              axis.text = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.ticks = element_line(linetype = "blank"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        geom_text(aes(label = clientsWithErrors), hjust = -0.5, color = "black")})
    
    
    output$orgDQHighPriorityErrors_ui <- renderUI({
      plotOutput("orgDQHighPriorityErrors", height = plotHeight_hp_errors())
    })
    
    #Plot of most common high priority errors within an org
    output$orgDQHighPriorityErrorTypes <- renderPlot({
      req(valid_file() == 1,
          nrow(dq_hp_error_types_org_level()) > 0)
      
      ggplot(head(dq_hp_error_types_org_level(), 10L),
             aes(
               x = reorder(Issue, Errors),
               y = Errors
             )) +
        geom_col(show.legend = FALSE,
                 color = "#063A89",
                 fill = "#063a89") +
        coord_flip() +
        labs(title = "Most Common High Priority Errors",
             x = "",
             y = "Number of Clients with High Piority Errors") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        theme_classic() +
        theme(axis.line = element_line(linetype = "blank"),
              axis.text = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.ticks = element_line(linetype = "blank"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        geom_text(aes(label = Errors), hjust = -0.5, color = "black")})
    
    output$orgDQHighPriorityErrorTypes_ui <- renderUI({
      plotOutput("orgDQHighPriorityErrorTypes", height = plotHeight_hp_errors())
    })
    
    #Plot of projects within selected org with most general errors
    output$orgDQErrors <- renderPlot({
      req(valid_file() == 1,
          nrow(dq_general_errors_top_projects()) > 0)
      
      # dq_general_errors_top_projects()$hover <-
      #   with(dq_general_errors_top_projects(),
      #        paste0(ProjectName))
      
      ggplot(
        head(dq_general_errors_top_projects(), 10L),
        aes(
          x = reorder(ProjectName, clientsWithErrors),
          y = clientsWithErrors
        )
      ) +
        geom_col(show.legend = FALSE,
                 color = "#063a89",
                 fill = "#063a89") +
        coord_flip() +
        labs(title = "Projects with the Most General Errors",
             x = "",
             y = "Number of Clients with General Errors") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        theme_classic() +
        theme(axis.line = element_line(linetype = "blank"),
              axis.text = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.ticks = element_line(linetype = "blank"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        geom_text(aes(label = clientsWithErrors), hjust = -0.5, color = "black")})
    
    output$orgDQErrors_ui <- renderUI({
      plotOutput("orgDQErrors", height = plotHeight_general_errors())
    })
    
    #Plot of most common general errors within an org
    output$orgDQErrorTypes <- renderPlot({
      req(valid_file() == 1,
          nrow(dq_general_error_types_org_level()) > 0)
      
      ggplot(head(dq_general_error_types_org_level(), 10L),
             aes(
               x = reorder(Issue, Errors),
               y = Errors
             )) +
        geom_col(show.legend = FALSE,
                 color = "#063A89",
                 fill = "#063a89") +
        coord_flip() +
        labs(title = "Most Common General Errors",
             x = "",
             y = "Number of Clients with General Errors") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        theme_classic() +
        theme(axis.line = element_line(linetype = "blank"),
              axis.text = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.ticks = element_line(linetype = "blank"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        geom_text(aes(label = Errors), hjust = -0.5, color = "black")})
    
    output$orgDQErrorTypes_ui <- renderUI({
      plotOutput("orgDQErrorTypes", height = plotHeight_general_errors())
    })
    
    #Plot of projects within selected org with most warnings
    output$orgDQWarnings <- renderPlot({
      req(valid_file() == 1,
          nrow(dq_warnings_top_projects()) > 0)
      
      # dq_warnings_top_projects()$hover <-
      #   with(dq_warnings_top_projects(),
      #        paste0(ProjectName))
      
      ggplot(head(dq_warnings_top_projects(), 10L),
             aes(
               x = reorder(ProjectName, Warnings),
               y = Warnings
             )) +
        geom_col(show.legend = FALSE,
                 color = "#063a89",
                 fill = "#063A89") +
        coord_flip() +
        labs(title = "Projects with the Most Warnings",
             x = "",
             y = "Number of Clients with Warnings") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        theme_classic() +
        theme(axis.line = element_line(linetype = "blank"),
              axis.text = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.ticks = element_line(linetype = "blank"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        geom_text(aes(label = Warnings), hjust = -0.5, color = "black")})
    
    output$orgDQWarnings_ui <- renderUI({
      plotOutput("orgDQWarnings", height = plotHeight_warnings())
    })
    
    #Plot of most common warnings within an org
    output$orgDQWarningTypes <- renderPlot({
      req(valid_file() == 1,
          nrow(dq_warning_types_org_level()) > 0)
      
      ggplot(head(dq_warning_types_org_level(), 10L),
             aes(
               x = reorder(Issue, Warnings),
               y = Warnings
             )) +
        geom_col(show.legend = FALSE,
                 color = "#063A89",
                 fill = "#063A89") +
        coord_flip() +
        labs(title = "Most Common Warnings",
             x = "",
             y = "Number of Clients with Warnings") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        theme_classic() +
        theme(axis.line = element_line(linetype = "blank"),
              axis.text = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.ticks = element_line(linetype = "blank"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        geom_text(aes(label = Warnings), hjust = -0.5, color = "black")})
    
    output$orgDQWarningTypes_ui <- renderUI({
      plotOutput("orgDQWarningTypes", height = plotHeight_warnings())
    })
    
    ##
    
    output$DQHighPriority <- DT::renderDT({
      req(valid_file() == 1)      
      
      ReportStart <- Export$ExportStartDate
      ReportEnd <- meta_HUDCSV_Export_End
      
      DQHighPriority <- dq_main_reactive() %>%
        filter(
            OrganizationName %in% c(input$orgList) &
            Type == "High Priority"
        ) %>%
        mutate(EntryDate = format.Date(EntryDate, "%m-%d-%Y")) %>%
        arrange(ProjectName, HouseholdID, PersonalID) %>%
        select("Project Name" = ProjectName,
               "Personal ID" = PersonalID,
               "High Priority Issue" = Issue,
               "Project Start Date" =  EntryDate)
      
      datatable(
        DQHighPriority,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
    
    output$DQErrors <- DT::renderDT({
      req(valid_file() == 1)      
      
      DQErrors <- dq_main_reactive() %>%
        filter(
            OrganizationName %in% c(input$orgList) &
            Type == "Error"
        ) %>%
        mutate(EntryDate = format.Date(EntryDate, "%m-%d-%Y")) %>%
        arrange(ProjectName, HouseholdID, PersonalID) %>%
        select("Project Name" = ProjectName,
               "Personal ID" = PersonalID,
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
      req(valid_file() == 1)      
      ReportStart <- Export$ExportStartDate
      ReportEnd <- meta_HUDCSV_Export_End
      
      DQWarnings <- dq_main_reactive() %>%
        filter(
            OrganizationName %in% c(input$orgList) &
            Type == "Warning"
        ) %>%
        #mutate(PersonalID = as.character(PersonalID)) %>%
        arrange(ProjectName, HouseholdID, PersonalID) %>%
        select(
          "Project Name" = ProjectName,
          "Personal ID" = PersonalID,
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
    req(valid_file() == 1)
    list(h2("Client Counts Report"),
         h4(input$currentProviderList))
  })
  
  output$headerUtilization <- renderUI({
    req(valid_file() == 1)
    list(h2("Bed and Unit Utilization"),
         h4(input$providerListUtilization),
         h4(format(ymd(
           input$utilizationDate
         ), "%B %Y"))
         )
  })
  
  output$headerDeskTime <- renderUI({
    req(valid_file() == 1)
    list(h2("Data Entry Timeliness"),
         h4(input$providersDeskTime),
         h4(paste("Fixed Date Range:",
                  format(today() - years(1), "%m-%d-%Y"),
                  "to",
                  format(today(), "%m-%d-%Y"))))
  })
  
  # output$headerExitsToPH <- renderUI({
  #   req(valid_file() == 1)
  #   ReportStart <- format.Date(input$ExitsToPHDateRange[1], "%B %d, %Y")
  #   ReportEnd <- format.Date(input$ExitsToPHDateRange[2], "%B %d, %Y")
  #   
  #   list(h2("Successful Placement Detail"),
  #        h4(input$ExitsToPHProjectList),
  #        h4(paste(
  #          ReportStart,
  #          "to",
  #          ReportEnd
  #        )))
  # })
  
  # output$headerOrganizationDQ <- renderUI({
  #   req(valid_file() == 1)
  #   list(h2("Data Quality Summary (Organization)"),
  #        h4(paste(
  #          format(input$dq_startdate, "%m-%d-%Y"),
  #          "to",
  #          format(meta_HUDCSV_Export_End, "%m-%d-%Y")
  #        )))
  # })
  
  output$headerSystemDQ <- renderUI({
    req(valid_file() == 1)
    list(h2("System-wide Data Quality"),
         h4(
           paste(format(meta_HUDCSV_Export_Start, "%m-%d-%Y"),
                 "through",
                 format(meta_HUDCSV_Export_End, "%m-%d-%Y"))
         ))
  })
  
  #### DQ SYSTEM REPORT #### ----------------------
  # button
  output$downloadFullDQReportButton  <- renderUI({
    if (!is.null(input$imported)) {
      downloadButton(outputId = "downloadFullDQReport",
                     label = "Download")
    }
  })
  
  output$downloadFullDQReport <- downloadHandler(
    filename = function() {
      paste("Full Data Quality Report-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {write_xlsx(fullDQReportDataList(), path = file)}
  )
  
  
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
  
  }, ignoreInit = TRUE)
}
  # output$headerCocDQ <- renderUI({
  #   req(!is.null(input$imported))
  #   list(h2("System-wide Data Quality"),
  #        h4(
  #          paste(format(meta_HUDCSV_Export_Start, "%m-%d-%Y"),
  #                "through",
  #                format(meta_HUDCSV_Export_End, "%m-%d-%Y"))
  #        ))
  # })
  # output$deskTimeNote <- renderUI({
  #   HTML(
  #     "<h4>HUD and Data Quality</h4>
  #       <p>HUD defines \"Data Quality\" as having three elements:
  #   1. Accuracy, 2. Completeness, and 3. Timeliness. Data Entry Delay (aka
  #   \"Desk Time\") refers to how long it is taking to enter a client into HMIS
  #   from the day they enter your project.
  #   <h4>Ohio Balance of State CoC Data Standards</h4>
  #   <p>According to the Data Quality Standards for the Ohio Balance of State
  #   CoC, all clients should be entered within 5 days of their entry into your
  #   project.
  #   <h4>How Do We Fix This?</h4>
  #   <p><strong>There is nothing a user can do</strong> to \"correct\" a client
  #   entered into the system outside the window. We can only resolve to enter
  #   clients within the 5-day range going forward. As you catch up on data entry,
  #   you may see your median get worse at first, but this data looks back exactly
  #   one year, so any clients with an Entry Date over a year ago will fall off
  #   of this plot and your median will change accordingly.
  #   <h4>Interpretation</h4>
  #   <p>Green dots here represent clients entered within the range and orange
  #   dots represent clients entered outside the range. The darker the dot, the
  #   more clients entered your project on that day. (Likely a household.)
  #   <p>The metric COHHIO looks at here is the Median, so if you have orange dots
  #   but your Median is within the 5 day range, that is great!
  #   <p>If you have orange dots BELOW the 0 mark, that means you entered Entry
  #   Dates into the future, which means there is potentially a mis-keyed date or
  #   the user needs technical assistance about how to know what date to enter for
  #   the Entry Date. If this is the case, please email the HMIS team.
  #           <h4>Is it possible there's a mistake?</h4>
  #   It's rare that this occurs, but if an Entry Exit has been created, deleted,
  #   and then recreated, the Entry Exit's \"Date Created\" date is reset,
  #   thus inflating the number of days between the Date Created and the Entry Date.
  #   If you need us to check if this was the case for a particular dot on the
  #   plot, please email us with the provider and number of days it is
  #   displaying that you think may be incorrect so we can verify if this is the
  #       issue."
  #   )})
  
# output$headerUtilization <- renderUI({
#   list(h2("Bed and Unit Utilization"),
#        h4(input$providerListUtilization),
#        h4(format(ymd(
#          input$utilizationDate
#        ), "%B %Y"))
#        )
# })

# output$headerDeskTime <- renderUI({
#   list(h2("Data Entry Timeliness"),
#        h4(input$providersDeskTime),
#        h4(paste("Fixed Date Range:",
#                 format(today() - years(1), "%m-%d-%Y"),
#                 "to",
#                 format(today(), "%m-%d-%Y"))))
# })

  
  
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
#   colnames(a) <- c("Personal ID", "Bed Start", "Exit Date", z)
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
#   colnames(a) <- c("Personal ID", "Bed Start", "Exit Date", "BNs")
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
#   colnames(a) <- c("Personal ID", "Bed Start", "Exit Date", "BNs")
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
#   colnames(a) <- c("Personal ID", "Bed Start", "Exit Date", "BNs")
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