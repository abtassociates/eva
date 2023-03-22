
function(input, output, session) {
  
  # Hard-coded --------------------------------------------------------------
  # hc = hard-coded
  hc_prior_living_situation_required <- ymd("20161001")
  
  #record_heatmap(target = ".wrapper")
  # track_usage(storage_mode = store_json(path = "logs/"))
  # Log the event to a database or file
  source("helper_functions.R", local = TRUE) # calling in HMIS-related functions that aren't in the HMIS pkg
  source("guidance.R", local = TRUE) # guidance text for various issues across the app (DQ, PDDE, etc.)
  source("changelog.R", local = TRUE) # guidance text for various issues across the app (DQ, PDDE, etc.)
  

# If you want an initial dialog box, use this -----------------------------
  
  valid_file <- reactiveVal(0)
  
  logMetadata("Session started")

  observe({ 
    logMetadata(paste("User on",input$sidebarmenuid))
  })
  
  output$fileInfo <- renderUI({
    if(valid_file() == 1) {
      HTML("<p>You have successfully uploaded your hashed HMIS CSV Export!</p>")
    }
  }) 

# Headers -----------------------------------------------------------------

  output$headerUpload <-
    headerGeneric("Upload HMIS CSV Export",
                  h4(
                    strong("Export Date: "),
                    format(meta_HUDCSV_Export_Date, "%m-%d-%Y at %I:%M %p")
                  ))

  
  output$headerLocalSettings <- headerGeneric("Edit Local Settings")
  
  output$headerClientCounts <- headerGeneric("Client Counts Report", renderUI({ 
    organization <- Project0 %>%
      filter(ProjectName == input$currentProviderList) %>%
      pull(OrganizationName)
    
    h4(paste(
      organization, "|", input$currentProviderList
    ))
  }))
  
  output$headerPDDE <- headerGeneric("Project Descriptor Data Elements Checker")
  
  output$headerSystemDQ <- headerGeneric("System-level Data Quality")
    
  output$headerDataQuality <- headerGeneric("Organization-level Data Quality")
  
  observeEvent(input$Go_to_upload, {
    updateTabItems(session, "sidebarmenuid", "tabUpload")
  })
  
  observeEvent(input$timeOut, {
    logMetadata("Timed out")
    session$reload()
  })

# Run scripts on upload ---------------------------------------------------

  observeEvent(input$imported, {

    initially_valid_zip <- zip_initially_valid()
    
    if(initially_valid_zip == 1) {

      hide('imported_progress')
      
      withProgress({
        setProgress(message = "Processing...", value = .15)
        setProgress(detail = "Reading your files..", value = .2)
        source("01_get_Export.R", local = TRUE)
        source("02_dates.R", local = TRUE)
        setProgress(detail = "Checking file structure", value = .35)
        source("03_integrity_checker.R", local = TRUE)
        # if structural issues were not found, keep going
        if (structural_issues == 0) {
          valid_file(1)
          setProgress(detail = "Prepping initial data..", value = .4)
          source("04_initial_data_prep.R", local = TRUE)
          setProgress(detail = "Making lists..", value = .5)
          source("05_cohorts.R", local = TRUE)
          setProgress(detail = "Assessing your data quality..", value = .7)
          source("06_DataQuality.R", local = TRUE)
          setProgress(detail = "Checking your PDDEs", value = .85)
          source("07_PDDE_Checker.R", local = TRUE)
          setProgress(detail = "Done!", value = 1)
          
          showModal(
            modalDialog(
              title = "Upload successful",
              "Congratulations! You have succesfully uploaded an HMIS CSV Export.",
              easyClose = TRUE,
              footer = modalButton("OK")
            )
          )

          logMetadata("Successful upload")
        } else{ # if structural issues were found, reset gracefully
          valid_file(0)
          reset("imported")
          showModal(
            modalDialog(
              title = "Your HMIS CSV Export is not structurally valid",
              "Your HMIS CSV Export has some High Priority issues that must
              be addressed by your HMIS Vendor. Please download the File Structure
              Analysis for details.",
              easyClose = TRUE,
              footer = modalButton("OK")
            )
          )
          logMetadata("Unsuccessful upload - not structurally valid")
        }
      })
    }
    
    output$integrityChecker <- DT::renderDataTable(
      {
        req(initially_valid_zip == 1)

        a <- integrity_main %>%
          group_by(Type, Issue) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          arrange(Type, desc(Count))
        
        datatable(
          a,
          rownames = FALSE,
          filter = 'none',
          options = list(dom = 't')
        )
      })
    
    output$downloadIntegrityBtn <- renderUI({
      req(initially_valid_zip == 1)
      downloadButton("downloadIntegrityCheck", "Download Structure Analysis Detail")
    })  
    
    output$downloadIntegrityCheck <- downloadHandler(
      # req(valid_file() == 1)

      filename = date_stamped_filename("File-Structure-Analysis-"),
      content = function(file) {
        write_xlsx(
          integrity_main %>%
            arrange(Type, Issue),
          path = file
        )
        
        logMetadata("Downloaded File Structure Analysis Report")
      }
    )
    
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
                           min = meta_HUDCSV_Export_Start,
                           start = meta_HUDCSV_Export_Start,
                           max = meta_HUDCSV_Export_End,
                           end = meta_HUDCSV_Export_End)
    }
    
    ##### PDDE Checker-----
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
      
      filename = date_stamped_filename("PDDE Report-"),
      content = function(file) {
        req(valid_file() == 1)
        
        summary <- pdde_main %>% 
          group_by(Issue, Type) %>%
          summarise(Count = n()) %>%
          ungroup()

        write_xlsx(list("Summary" = summary, "Data" = pdde_main), path = file)
        logMetadata("Downloaded PDDE Report")
      }
    )
    
    # guidance box
    output$pdde_guidance_summary <- DT::renderDataTable({
      req(valid_file() == 1)
      
      guidance <- pdde_main %>%
        select(Type, Issue, Guidance) %>%
        mutate(Type = factor(Type, levels = c("Error",
                                              "Warning"))) %>%
        arrange(Type, Issue) %>%
        unique()
      
      datatable(guidance, 
                rownames = FALSE,
                escape = FALSE,
                filter = 'top',
                options = list(dom = 'ltpi'))
    })
    
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
    
    ### Client Counts -------------------------------
    source("client_counts_functions.R", local = TRUE)
    
    # CLIENT COUNT DETAILS - APP
    output$clientCountData <- DT::renderDataTable({
      req(valid_file() == 1)

      datatable(
        client_count_data_df() %>%
          filter(`Project Name` == input$currentProviderList) %>%
          select(all_of(clientCountDetailCols)),
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
      
    # CLIENT COUNT SUMMARY - APP
    output$clientCountSummary <- DT::renderDataTable({
      req(valid_file() == 1)
      
      datatable(
        client_count_summary_df(),
        rownames = FALSE,
        filter = 'none',
        options = list(dom = 't')
      )
    })
    
    # CLIENT COUNT DOWNLOAD
    output$downloadClientCountsReportButton  <- renderUI({
      req(valid_file() == 1)
      
      downloadButton(outputId = "downloadClientCountsReport",
                     label = "Download System-Wide")
    })
    
    # the download basically contains a pivoted and summarized version of the
    # two app tables, but for all projects along with a Current tab limited to
    # just the current date.
    output$downloadClientCountsReport <- downloadHandler(
      filename = date_stamped_filename("Client Counts Report-"),
      content = get_clientcount_download_info
    )

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
                filter = 'top',
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
    source("06_DataQuality_functions.R", local = TRUE)
    
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
      
      orgDQoverlaps <- overlaps %>%
        filter(OrganizationName %in% c(input$orgList) | 
                 PreviousOrganizationName %in% c(input$orgList))
      
      orgDQReferrals <- calculate_outstanding_referrals(input$CEOutstandingReferrals) %>%
        filter(OrganizationName %in% c(input$orgList))
      
      getDQReportDataList(orgDQData, orgDQoverlaps, "ProjectName", orgDQReferrals)
    })
    
    fullDQReportDataList <- reactive({
      req(valid_file() == 1)
      getDQReportDataList(dq_main_reactive(), overlaps, "OrganizationName",
                          calculate_outstanding_referrals(input$CEOutstandingReferrals))
    })
    
    output$downloadOrgDQReport <- downloadHandler(
      filename = date_stamped_filename(str_glue("{input$orgList} Data Quality Report-")),
      content = function(file) {
        write_xlsx(orgDQReportDataList(), path = file)
        logMetadata("Downloaded Org-level DQ Report")
      }
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
    
    #SYSTEM-LEVEL DQ TAB PLOTS
    # By-org shows organizations containing highest number of HP errors/errors/warnings
    # By-issue shows issues that are the most common of that type (HP errors/errors/warnings)
    output$systemDQHighPriorityErrorsByOrg_ui <- renderUI({
      renderDQPlot("sys", "High Priority", "Org", "#16697A")
    })
    
    output$systemDQHighPriorityErrorsByIssue_ui <- renderUI({
      renderDQPlot("sys", "High Priority", "Issue", "#16697A")
    })
    
    output$systemDQErrorsByOrg_ui <- renderUI({
      renderDQPlot("sys", "Error", "Org", "#489FB5")
    })
    
    output$systemDQErrorByIssue_ui <- renderUI({
      renderDQPlot("sys", "Error", "Issue", "#489FB5")
    })
    
    output$systemDQWarningsByOrg_ui <- reactive({
      renderDQPlot("sys", "Warning", "Org", "#82C0CC")
    })
    
    output$systemDQWarningByIssue_ui <- renderUI({
      renderDQPlot("sys", "Warning", "Issue", "#82C0CC")
    })

    #ORG-LEVEL TAB PLOTS
    # By-project shows projects, within the selected org, containing highest number of HP errors/errors/warnings
    # By-issue shows issues, within the selected org, that are the most common of that type (HP errors/errors/warnings)
    output$orgDQHighPriorityErrorsByProject_ui <- renderUI({
      renderDQPlot("org", "High Priority", "Project", "#11697A")
    })
    
    output$orgDQHighPriorityErrorByIssue_ui <- renderUI({
      renderDQPlot("org", "High Priority", "Issue", "#11697A")
    })
    
    output$orgDQErrorsByProject_ui <- renderUI({
      renderDQPlot("org", "Error", "Project", "#489FB5")
    })
    
    output$orgDQErrorByIssue_ui <- renderUI({
      renderDQPlot("org", "Error", "Issue", "#489FB5")
    })
    
    output$orgDQWarningsByProject_ui <- renderUI({
      renderDQPlot("org", "Warnings", "Project", "#82C0CC")
    })
    
    output$orgDQWarningsByIssue_ui <- renderUI({
      renderDQPlot("org", "Warnings", "Issue", "#82C0CC")
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
  
  # output$headerUtilization <- renderUI({
  #   req(valid_file() == 1)
  #   list(h2("Bed and Unit Utilization"),
  #        h4(input$providerListUtilization),
  #        h4(format(ymd(
  #          input$utilizationDate
  #        ), "%B %Y"))
  #        )
  # })
  # 
  # output$headerDeskTime <- renderUI({
  #   req(valid_file() == 1)
  #   list(h2("Data Entry Timeliness"),
  #        h4(input$providersDeskTime),
  #        h4(paste("Fixed Date Range:",
  #                 format(today() - years(1), "%m-%d-%Y"),
  #                 "to",
  #                 format(today(), "%m-%d-%Y"))))
  # })
  
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
  
  #### DQ SYSTEM REPORT #### ----------------------
  # button
  output$downloadFullDQReportButton  <- renderUI({
    if (valid_file() == 1) {
      downloadButton(outputId = "downloadFullDQReport",
                     label = "Download")
    }
  })
  
  output$downloadFullDQReport <- downloadHandler(
    filename = date_stamped_filename("Full Data Quality Report-"),
    content = function(file) {
      write_xlsx(fullDQReportDataList(), path = file)
      logMetadata("Downloaded System-level DQ Report")
    }
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
  
  session$onSessionEnded(function() {
    logMetadata("Session Ended")
  })
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
