
function(input, output, session) {
  #record_heatmap(target = ".wrapper")
  # track_usage(storage_mode = store_json(path = "logs/"))
  # Log the event to a database or file
  source("hardcodes.R", local = TRUE) # hard-coded variables and data frames
  # used throughout the app
  source("helper_functions.R", local = TRUE) # calling in HMIS-related functions
  # that aren't in the HMIS pkg
  source("changelog.R", local = TRUE) # changelog entries
  
  # log that the session has started
  logMetadata("Session started")
  
  # this will be a requirement for proceeding with many parts of the code 
  valid_file <- reactiveVal(0)
  
  # log when user navigate to a tab
  observe({ 
    logMetadata(paste("User on",input$sidebarmenuid))
  })

  # Headers -----------------------------------------------------------------

  output$headerUpload <-
    headerGeneric("Upload HMIS CSV Export",
                  h4(
                    strong("Export Date: "),
                    format(meta_HUDCSV_Export_Date, "%m-%d-%Y at %I:%M %p")
                  ))

  
  output$headerLocalSettings <- headerGeneric("Edit Local Settings")
  
  # the reason we split the Client Count header into two is for shinytest reasons
  # this _supp renderUI needed to be associated with an output in order to make 
  # the HTML <div> id the same each time. Without associating with an output, 
  # the id changed each time and the shinytest would catch the difference and fail
  output$headerClientCounts_supp <- renderUI({ 
    organization <- Project0 %>%
      filter(ProjectName == input$currentProviderList) %>%
      pull(OrganizationName)
    
    h4(organization, "|", input$currentProviderList)
  })
  output$headerClientCounts <- headerGeneric("Client Counts Report", htmlOutput("headerClientCounts_supp"))
  
  output$headerPDDE <- headerGeneric("Project Descriptor Data Elements Checker")
  
  output$headerSystemDQ <- headerGeneric("System-level Data Quality")
    
  output$headerDataQuality <- headerGeneric("Organization-level Data Quality")
  
  observeEvent(input$Go_to_upload, {
    if(isTruthy(input$in_demo_mode)) {
      updateTabItems(session, "sidebarmenuid", "tabClientCount")
    } else {
      updateTabItems(session, "sidebarmenuid", "tabUpload")
    }
  }) 
  
  observeEvent(input$timeOut, {
    logMetadata("Timed out")
    session$reload()
  })
  
  # file upload status text ----------------------------------------------------
  output$fileInfo <- renderUI({
    HTML("<p>Please upload your hashed HMIS CSV Export!</p>")
    if(is.null(input$imported)) {
      return("")
    } else if(valid_file() == 1) {
      HTML("<p>You have successfully uploaded your hashed HMIS CSV Export!</p>")
    }
  }) 
  
  # Handle demo mode toggle --------------------------------------------------
  should_activate_demo <- reactive({
    observeEvent(input$continue_demo_btn, {
      removeModal()
      return(TRUE)
    });
    
    if(length(input$imported)) {
      showModal(
        modalDialog(
          "If you switch to demo mode, your uploaded HMIS CSV will be removed. Continue?",
          title = NULL,
          footer = tagList(actionButton("continue_demo_btn", "Continue"),
                           modalButton("Cancel"))
        )
      )
      shinyjs::runjs('document.getElementById("isdemo").checked = false;')
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  activate_demo <- reactive({
    capture.output("Switching to demo mode!")
    print("Switching to demo mode!")
    # clear environment
    reset("imported")
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    
    # let user know things take a min to load then load the demo data
    showModal(
      modalDialog(
        "Activating demo mode. This may take a minute...",
        title = NULL,
        footer = NULL
      )
    )
    load("demo.Rdata", envir = .GlobalEnv)
    removeModal()
    
    # mark the file as valid
    valid_file(1)
    
    # update inputs choices and defaults
    updatePickerInput(session = session, inputId = "currentProviderList",
                      choices = sort(Project$ProjectName))
    
    updatePickerInput(session = session, inputId = "providerListDQ",
                      choices = dq_providers)
    
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
  })
  
  
  values <- reactiveValues(modal_closed=0)
  
  observeEvent(input$continue_demo_btn, {
    removeModal()
    activate_demo()
  })
  
  observeEvent(input$cancel_demo, {
    values$modal_closed <- 1
    removeModal()
  })
  
  observe({
    if(values$modal_closed){
      shinyjs::runjs('document.getElementById("isdemo").checked = false;')
    }
  })
  
  observeEvent(input$in_demo_mode, {
    if(input$in_demo_mode) {
      if(length(input$imported)) {
        values$modal_closed <- 0
        showModal(
          modalDialog(
            "If you switch to demo mode, your uploaded HMIS CSV will be removed. Continue?",
            title = NULL,
            footer = tagList(actionButton("continue_demo_btn", "Continue"),
                             actionButton("cancel_demo", "Cancel"))
          )
        )
      } else {
        activate_demo()
      }
      
    } else {
      print("It's in live mode!")
      showModal(
        modalDialog(
          "Please upload your hashed HMIS CSV Export.",
          title = "Upload your HMIS CSV Export",
          easyClose = TRUE
        )
      )
      valid_file(0)
      reset("imported")
      updateTabItems(session, "sidebarmenuid", "tabUpload")
    }
  }, ignoreInit = TRUE)
  
# Run scripts on upload ---------------------------------------------------
  
  observeEvent(input$imported, {
    valid_file(0)
    shinyjs::runjs('document.getElementById("isdemo").checked = false;')
    
    source("00_initially_valid_import.R", local = TRUE)
    
    if(initially_valid_import == 1) {

      hide('imported_progress')
      
      withProgress({
        setProgress(message = "Processing...", value = .15)
        setProgress(detail = "Reading your files..", value = .2)
        source("01_get_Export.R", local = TRUE)
        source("02_export_dates.R", local = TRUE)
        setProgress(detail = "Checking file structure", value = .35)
        
        # if we're in shiny testmode and the script has gotten here,
        # that means we've gotten all the exports 
        # we can edit those to capture all File Structure Analysis 
        # issues and then continue running to test
        if(isTRUE(getOption("shiny.testmode")) && 
           input$imported$name == "FY24-ICF-fsa-test.zip") {
          source("tests/update_test_good_fsa.R", local = TRUE)  
        }
        
        source("03_file_structure_analysis.R", local = TRUE)
        # if structural issues were not found, keep going
        if (structural_issues == 0) {
          valid_file(1)
          
          if(nrow(
            file_structure_analysis_main %>%
              filter(Issue == "Impermissible characters"))) {
            showModal(
              modalDialog(
                "Eva has detected impermissible characters in your HMIS CSV file. 
                Please note that these characters may cause Eva to crash.",
                title = "Impermissible characters",
                easyClose = TRUE
              )
            )
          }
          setProgress(detail = "Prepping initial data..", value = .4)
          source("04_initial_data_prep.R", local = TRUE)
          setProgress(detail = "Assessing your data quality..", value = .7)
          
          # if we're in shiny testmode and the script has gotten here,
          # that means we're using the hashed-test-good file. 
          # we will update that file to capture the various issues we want to test
          # we have confirmed that it is correctly capturing these issues
          if(isTRUE(getOption("shiny.testmode")) && 
             input$imported$name == "FY24-ICF-hashed-current-good.zip") {
            source("tests/update_test_good_dq.R", local = TRUE)  
          }
          
          source("05_DataQuality.R", local = TRUE)
          setProgress(detail = "Checking your PDDEs", value = .85)
          source("06_PDDE_Checker.R", local = TRUE)
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
          rlang::env_coalesce(.GlobalEnv, environment())
          # # AS 2/13/24: For saving the demo data file:
          ## browser is needed to pause so you can save from the console
          # browser()
          # # this saves everything in the global and calling environment. 
          ## The former includes the meta_ variables,
          ## the latter includes everything else: functions, data frames, and values
          ## xz compression gets the file down to 22MB, instead of the default
          ## which gets down to 1GB, which is too large to upload to GitHub
          # save(list = c(ls(envir = .GlobalEnv, all.names = TRUE), ls(all.names = TRUE)), file = "demo.RData", compress="xz")
          
        } else{ # if structural issues were found, reset gracefully
          valid_file(0)
          reset("imported")
          showModal(
            modalDialog(
              title = "Unsuccessful Upload: Your HMIS CSV Export is not
              structurally valid",
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
  }, ignoreInit = TRUE)
# File Structure Analysis Summary -----------------------------------------
    
    output$fileStructureAnalysis <- DT::renderDataTable(
      {
        req(exists("file_structure_analysis_main"))
        
        a <- file_structure_analysis_main %>%
          group_by(Type, Issue) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          arrange(Type, desc(Count))
        
        
        exportTestValues(fileStructureAnalysis = a)
        
        datatable(
          a,
          rownames = FALSE,
          filter = 'none',
          options = list(dom = 't', 
                         language = list(
                          zeroRecords = "No file structure analysis issues! 
                        Visit the other tabs to view the rest of Eva's output")
                         )
        )
      }) |> bindEvent(input$imported, input$in_demo_mode)
# File Structure Analysis Download ----------------------------------------

    output$downloadFileStructureAnalysisBtn <- renderUI({
      req(exists("file_structure_analysis_main"))
      req(nrow(file_structure_analysis_main) > 0)
      downloadButton("downloadFileStructureAnalysis",
                     "Download Structure Analysis Detail")
    })  
    
    output$downloadFileStructureAnalysis <- downloadHandler(
      filename = date_stamped_filename("File-Structure-Analysis-"),
      content = function(file) {
        write_xlsx(
          file_structure_analysis_main %>%
            arrange(Type, Issue) %>%
            nice_names(),
          path = file
        )
        
        logMetadata("Downloaded File Structure Analysis Report")
        
        exportTestValues(file_structure_analysis_main = file_structure_analysis_main)
      }
    )


# # System Data Quality Overview --------------------------------------------
# empty_dq_overview_plot <- function(currPlot) {
#   return(currPlot + 
#     theme(
#       axis.line = element_blank(),
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     ) +
#     annotate(
#       "text",
#       x = 0.5,
#       y = 0.5,
#       label = "No issues!",
#       size = 12,
#       color = "gray50",
#       fontface = "bold"
#     )
#   )
# }
    
# output$dq_overview_plot <- renderPlot({
#   req(valid_file() == 1)
# # browser()
#   detail <- dq_main_reactive() %>%
#     count(Type, name = "Total") %>%
#     mutate(Type = factor(
#       case_when(
#         Type == "High Priority" ~ "High Priority Issues",
#         Type == "Error" ~ "Errors",
#         Type == "Warning" ~ "Warnings"
#       ),
#       levels = c("High Priority Issues",
#                  "Errors",
#                  "Warnings")
#     ))

#   dq_plot_overview <-
#     ggplot(
#       detail,
#       aes(x = Type, y = Total)
#     ) +
#     geom_col(fill = "#71b4cb", alpha = .7, width = .4) +
#     scale_y_continuous(label = comma_format()) +
#     labs(
#       title = "System-wide Data Quality Issues",
#       x = "Data Quality Issue Type",
#       y = "System-wide Issues") +
#     theme_minimal(base_size = 18) +
#     theme(
#       plot.title.position = "plot",
#       title = element_text(colour = "#73655E")
#     ) +
#     geom_text(aes(label = prettyNum(Total, big.mark = ",")),
#                vjust = -.5,
#                color = "gray14")
  
#   if (nrow(detail) == 0) {
#     dq_plot_overview <- empty_dq_overview_plot(dq_plot_overview)
#   }
#   dq_plot_overview
# })  
    
# 
#     output$dq_orgs_overview_plot <- renderPlot({
#       req(valid_file() == 1)
# # browser()
#       highest_type <- dq_main_reactive() %>%
#         count(Type) %>% 
#         head(1L) %>%
#         mutate(Type = as.character(Type)) %>%
#         pull(Type)
#       
#       highest_type_display <-
#         case_when(
#           highest_type == "High Priority" ~ "High Priority Issues",
#           highest_type == "Error" ~ "Errors",
#           TRUE ~ "Warnings"
#         )
#       
#       detail <- dq_main_reactive() %>%
#         count(OrganizationName, Type, name = "Total") %>%
#         filter(Type == highest_type)
# 
#       dq_plot_overview <-
#         ggplot(
#           detail %>%
#             arrange(desc(Total)) %>%
#             head(5L) %>%
#             mutate(OrganizationName = fct_reorder(OrganizationName, Total)),
#           aes(x = OrganizationName, y = Total)
#         ) +
#         geom_col(fill = "#D5BFE6", alpha = .7)+
#         scale_y_continuous(label = comma_format()) +
#         labs(
#           title = paste("Highest Counts of",
#                         ifelse(is_empty(highest_type_display),
#                                "Issue",
#                                highest_type_display)),
#           x = "Top 5 Organizations",
#           y = ifelse(is_empty(highest_type_display),"Issue",highest_type_display)
#         ) +
#         coord_flip() +
#         theme_minimal(base_size = 18) +
#         theme(
#           plot.title.position = "plot",
#           title = element_text(colour = "#73655E")
#         ) +
#         geom_text(aes(label = prettyNum(Total, big.mark = ",")),
#                   nudge_y = 2,
#                   color = "gray14")
#       
#       if (nrow(detail) == 0) {
#         dq_plot_overview <- empty_dq_overview_plot(dq_plot_overview)
#       }
#       dq_plot_overview
#     })
    
    output$validate_plot <- renderPlot({
      req(valid_file() == 1)
      # browser()

      detail <- client_count_data_df() %>%
        filter(str_detect(Status, "Exit", negate = TRUE)) %>%
        mutate(Status = factor(
          case_when(
            str_detect(Status, "Currently in") ~ "Currently in project",
            str_detect(Status, "Currently Moved") ~ "Currently Moved In",
            TRUE ~ Status
          ),
          levels = c("Currently in project",
                     "Active No Move-In",
                     "Currently Moved In")
        )) %>% 
        count(ProjectType, Status, name = "Total")
      
      detail_order <- detail %>%
        group_by(ProjectType) %>%
        summarise(InProject = sum(Total, na.rm = FALSE)) %>%
        ungroup()
      
      
      plot_data <- detail %>%
        left_join(detail_order, by = "ProjectType") %>%
        group_by(ProjectType) %>%
        arrange(ProjectType, desc(Total)) %>%
        mutate(
          movedin = lag(Total, default = 0),
          text_position = case_when(
            !ProjectType %in% c(ph_project_types) ~ InProject / 2,
            ProjectType %in% c(ph_project_types) ~ 
              Total / 2 + movedin
          )
        )
      
      validate_by_org <-
        ggplot(
          plot_data,
          aes(x = reorder(project_type_abb(ProjectType), InProject),
              y = Total, fill = Status)
        ) +
        geom_col(alpha = .7, position = "stack")  +
        geom_text(aes(label = prettyNum(Total, big.mark = ","),
                      y = text_position),
                  color = "gray14")+
        scale_y_continuous(label = comma_format()) +
        scale_colour_manual(
          values = c(
            "Currently in project" = "#71B4CB",
            "Active No Move-In" = "#7F5D9D",
            "Currently Moved In" = "#52BFA5"
          ),
          aesthetics = "fill"
        ) +
        labs(
          title = "Current System-wide Counts",
          x = "",
          y = ""
        ) +
        theme_minimal(base_size = 18) +
        theme(
          plot.title.position = "plot",
          title = element_text(colour = "#73655E"),
          legend.position = "top"
        )
      
      validate_by_org
    })
    
# PDDE Checker ------------------------------------------------------------

    # summary table
    output$pdde_summary_table <- DT::renderDataTable({
      req(valid_file() == 1)
      
      a <- pdde_main %>%
        group_by(Issue, Type) %>%
        summarise(Count = n()) %>%
        ungroup() %>%
        arrange(Type)
      
      exportTestValues(pdde_summary_table = a)

      datatable(
        a,
        rownames = FALSE,
        filter = 'none',
        options = list(dom = 't')
      )
    })

# PDDE Download Button ----------------------------------------------------

    output$downloadPDDEReportButton  <- renderUI({
      req(valid_file() == 1)
      req(nrow(pdde_main) > 0)
      downloadButton(outputId = "downloadPDDEReport",
                       label = "Download")
    })


# Download Button Handler -------------------------------------------------

    output$downloadPDDEReport <- downloadHandler(
      
      filename = date_stamped_filename("PDDE Report-"),
      content = function(file) {
        req(valid_file() == 1)
        
        summary <- pdde_main %>% 
          group_by(Issue, Type) %>%
          summarise(Count = n()) %>%
          ungroup()

        write_xlsx(
          list("Summary" = summary,
               "Data" = pdde_main %>%
                 nice_names()),
          path = file)
        
        logMetadata("Downloaded PDDE Report")
        
        exportTestValues(pdde_download = list("Summary" = summary, "Data" = pdde_main))
      }
    )
    

# PDDE Guidance -----------------------------------------------------------

    output$pdde_guidance_summary <- DT::renderDataTable({
      req(valid_file() == 1)
      
      guidance <- pdde_main %>%
        select(Type, Issue, Guidance) %>%
        arrange(Type, Issue) %>%
        unique()
      
      exportTestValues(pdde_guidance_summary = guidance)
      
      datatable(
        guidance, 
        rownames = FALSE,
        escape = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })

# Client Counts -----------------------------------------------------------

    source("client_counts_functions.R", local = TRUE)
    
# CLIENT COUNT DETAILS - APP ----------------------------------------------
    output$clientCountData <- DT::renderDataTable({
      req(valid_file() == 1)

      x <- client_count_data_df() %>%
        filter(ProjectName == input$currentProviderList) %>%
        select(all_of(clientCountDetailCols)) %>%
          nice_names()
      
      exportTestValues(clientCountData = x)
      
      datatable(
        x,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
      

# CLIENT COUNT SUMMARY - APP ----------------------------------------------

    output$clientCountSummary <- DT::renderDataTable({
      req(valid_file() == 1)
      
      exportTestValues(clientCountSummary = client_count_summary_df())
      
      datatable(
        client_count_summary_df() %>%
          nice_names(),
        rownames = FALSE,
        filter = 'none',
        options = list(dom = 't')
      )
    })
    

# CLIENT COUNT DOWNLOAD ---------------------------------------------------

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
      
      exportTestValues(dq_org_guidance_summary = guidance)
      
      datatable(
        guidance, 
        rownames = FALSE,
        escape = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
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
      
      exportTestValues(dq_organization_summary_table = a)
      
      datatable(
        a,
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
    

# Prep DQ Downloads -------------------------------------------------------

    source("05_DataQuality_functions.R", local = TRUE)

    # list of data frames to include in DQ Org Report
    dqDownloadInfo <- reactive({
      req(valid_file() == 1)

      # org-level data prep (filtering to selected org)
      orgDQData <- dq_main_reactive() %>%
        filter(OrganizationName %in% c(input$orgList))
      
      orgDQoverlaps <- overlaps %>%
        filter(OrganizationName %in% c(input$orgList) | 
                 PreviousOrganizationName %in% c(input$orgList))
#browser()
      orgDQReferrals <- 
        calculate_outstanding_referrals(input$CEOutstandingReferrals) %>%
        filter(OrganizationName %in% c(input$orgList))
      
      # return a list for reference in downloadHandler
      list(
        orgDQData = 
          getDQReportDataList(orgDQData,
                              orgDQoverlaps,
                              "ProjectName",
                              orgDQReferrals
                              ),
           
        systemDQData = 
          getDQReportDataList(dq_main_reactive(),
                              overlaps,
                              "OrganizationName",
                              calculate_outstanding_referrals(input$CEOutstandingReferrals)
                              )
      )
    })

# Download Org DQ Report --------------------------------------------------

    output$downloadOrgDQReportButton  <- renderUI({
      req(valid_file() == 1)
      
      req(length(dqDownloadInfo()$orgDQData) > 0)
        downloadButton(outputId = "downloadOrgDQReport",
                       label = "Download")
    })
    
    output$downloadOrgDQReport <- downloadHandler(
      filename = reactive(date_stamped_filename(
        str_glue("{input$orgList} Data Quality Report-"))),
      content = function(file) {
        write_xlsx(dqDownloadInfo()$orgDQData, path = file)
        logMetadata("Downloaded Org-level DQ Report")
        exportTestValues(orgDQ_download = dqDownloadInfo()$orgDQData)
      }
    )
    
# Download System DQ Report -----------------------------------------------
    # button
    output$downloadSystemDQReportButton  <- renderUI({
      req(valid_file() == 1)
      req(length(dqDownloadInfo()$systemDQData) > 0)
      downloadButton(outputId = "downloadSystemDQReport",
                       label = "Download")
    })
    
    output$downloadSystemDQReport <- downloadHandler(
      filename = date_stamped_filename("Full Data Quality Report-"),
      content = function(file) {
        write_xlsx(dqDownloadInfo()$systemDQData, path = file)
        logMetadata("Downloaded System-level DQ Report")
        exportTestValues(systemDQ_download = dqDownloadInfo()$systemDQData)
      }
    )

# SYSTEM-LEVEL DQ TAB PLOTS -----------------------------------------------
    # By-org shows organizations containing highest number of HP/errors/warnings
    # By-issue shows issues that are the most common of that type
    output$systemDQHighPriorityErrorsByOrg_ui <- renderUI({
      renderDQPlot("sys", "High Priority", "Org", "#71B4CB")
    })

   
    output$systemDQHighPriorityErrorsByIssue_ui <- renderUI({
      renderDQPlot("sys", "High Priority", "Issue", "#71B4CB")
    })
    
    output$systemDQErrorsByOrg_ui <- renderUI({
      renderDQPlot("sys", "Error", "Org", "#71B4CB")
    })
    
    output$systemDQErrorsByIssue_ui <- renderUI({
      renderDQPlot("sys", "Error", "Issue", "#71B4CB")
    })
    
    output$systemDQWarningsByOrg_ui <- renderUI({
      renderDQPlot("sys", "Warning", "Org", "#71B4CB")
    })
    
    output$systemDQWarningsByIssue_ui <- renderUI({
      renderDQPlot("sys", "Warning", "Issue", "#71B4CB")
    })


# ORG-LEVEL TAB PLOTS -----------------------------------------------------
    # By-project shows projects, within the selected org, containing highest 
    # number of HP errors/errors/warnings
    # By-issue shows issues, within the selected org, that are the most common 
    # of that type (HP errors/errors/warnings)
    output$orgDQHighPriorityErrorsByProject_ui <- renderUI({
      renderDQPlot("org", "High Priority", "Project", "#71B4CB")
    })
    
    output$orgDQHighPriorityErrorByIssue_ui <- renderUI({
      renderDQPlot("org", "High Priority", "Issue", "#71B4CB")
    })
    
    output$orgDQErrorsByProject_ui <- renderUI({
      renderDQPlot("org", "Error", "Project", "#71B4CB")
    })
    
    output$orgDQErrorByIssue_ui <- renderUI({
      renderDQPlot("org", "Error", "Issue", "#71B4CB")
    })
    
    output$orgDQWarningsByProject_ui <- renderUI({
      renderDQPlot("org", "Warning", "Project", "#71B4CB")
    })
    
    output$orgDQWarningsByIssue_ui <- renderUI({
      renderDQPlot("org", "Warning", "Issue", "#71B4CB")
    })
  
    
    
    # output$headerUtilization <- renderUI({
    #   list(h2("Bed and Unit Utilization"),
    #        h4(input$providerListUtilization),
    #        h4(format(ymd(
    #          input$utilizationDate
    #        ), "%B %Y"))
    #        )
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
  session$onSessionEnded(function() {
    logMetadata("Session Ended")
  })
  
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
}