
function(input, output, session) {
  # record_heatmap(target = ".wrapper")
  # track_usage(storage_mode = store_json(path = "logs/"))
  set.seed(12345)
  
  sessionVars <- c(
    "validation", 
    "Export", 
    "initially_valid_import",
    "valid_file", 
    "file_structure_analysis_main", 
    "Project0", 
    "CurrentLivingSituation", 
    "Event", 
    "ReportStart", 
    "ReportEnd", 
    "meta_HUDCSV_Export_Start", 
    "meta_HUDCSV_Export_End", 
    "meta_HUDCSV_Export_Date", 
    "overlap_details", 
    "base_dq_data_func", 
    "dq_main_df", 
    "pdde_main", 
    "days_of_data",
    "client_categories"
  )
  
  # Dynamic datasets, dependent on user filters -------------------------------
  sys_plot_datasets <- c(
    "inflow_outflow_full",
    "inflow_outflow_monthly",
    "people_universe_filtered",
    "sankey",
    "client_level_export_df"
  )
  sys_plot_data <- reactiveValues()
  
  windowSize <- reactiveVal(NULL)
  triggerPlot <- reactiveVal(NULL)
  
  reset_session_vars()
  
  # 
  # # functions used throughout the app
  # source("helper_functions.R", local = TRUE)
  
  # functions used throughout the app
  source("helper_functions.R", local = TRUE)
  
  # glossary entries
  source("glossary.R", local = TRUE)
  # Asynchronous processing, using mirai, of DQ and PDDE to save time------
  # for a single user and multiple users
  # Create DQ and PDDE script environment
  menv <- environment()
  mirai::everywhere({
    library(data.table)
    library(tidyverse)
    library(janitor)
    library(readr)
    source("helper_functions.R", local=menv)
  })
  for(obj in ls(.GlobalEnv, all.names=TRUE)) {
    assign(obj, get(obj, .GlobalEnv), envir = menv)
  }
  
  observe({
    req(session$clientData$url_search != "")
    updateTabItems(session,
                   "sidebarmenuid",
                   "tabGlossary")
    #parseQueryString(session$clientData$url_search))
  })
  
  # changelog entries
  source("changelog.R", local = TRUE)

  # manages toggling demo mode on and off
  source("demo_management.R", local = TRUE)
  
  # log that the session has started
  logMetadata("Session started")
  
  # set during initially valid processing stop. Rest of processing stops if invalid
  # FSA is hidden unless initially_valid_import() == 1
  initially_valid_import <- reactiveVal() 
  
  # in Demo Mode, tracks if user has seen tab-specific pop-up
  seen_message <- reactiveValues() 
  
  demo_modal_closed <- reactiveVal()
  
  # syso_gender_cats <- reactive({
  #   ifelse(
  #     input$methodology_type == 1,
  #     list(syso_gender_method1),
  #     list(syso_gender_method2)
  #   )[[1]]
  # })
  

  # log when user navigate to a tab
  observe({ 
    logMetadata(paste0("User on ",input$sidebarmenuid, 
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  })
  
  # Headers -----------------------------------------------------------------
  
  output$headerUpload <-
    headerGeneric("Upload HMIS CSV Export",
                  h4(
                    strong("Export Date: "),
                    format(session$userData$meta_HUDCSV_Export_Date, "%m-%d-%Y at %I:%M %p")
                  ))

  output$headerLocalSettings <- headerGeneric("Edit Local Settings")

  # the reason we split the Client Count header into two is for shinytest reasons
  # this _supp renderUI needed to be associated with an output in order to make 
  # the HTML <div> id the same each time. Without associating with an output, 
  # the id changed each time and the shinytest would catch the difference and fail
  output$headerClientCounts_supp <- renderUI({ 
    req(session$userData$valid_file() == 1)
    organization <- session$userData$Project0 %>%
      filter(ProjectName == input$currentProviderList) %>%
      pull(OrganizationName)
    
    h4(organization, "|", input$currentProviderList)
  })
  
  output$headerClientCounts <- headerGeneric("Client Counts Report",
                                             htmlOutput("headerClientCounts_supp"))
  
  output$headerPDDE <- headerGeneric("Project Descriptor Data Elements Checker")
  
  output$headerSystemDQ <- headerGeneric("System-level Data Quality")
  
  output$headerDataQuality <- headerGeneric("Organization-level Data Quality")
  
  output$headerSystemOverview <- headerGeneric("System Overview")

  output$headerSystemExit <- headerGeneric("System Exit")

  observeEvent(input$Go_to_upload, {
    updateTabItems(session, "sidebarmenuid", "tabUpload")
  }) 
  
  # decides when it's time to time out the session
  observeEvent(input$timeOut, {
    logMetadata("Timed out")
    session$reload()
  })
  
  # file upload status text ----------------------------------------------------
  output$fileInfo <- renderUI({
    HTML("<p>Please upload your hashed HMIS CSV Export!</p>")
    if(is.null(input$imported)) {
      return("")
    } else if(session$userData$valid_file == 1) {
      HTML("<p id='successful_upload'>You have successfully uploaded your hashed
           HMIS CSV Export!</p>")
    }
  }) 
  
  # Run scripts on upload ---------------------------------------------------
  
  process_upload <- function(upload_filename, upload_filepath) {
    withProgress({
      setProgress(message = "Processing...", value = .01)
      
      setProgress(detail = "Checking initial validity ", value = .05)
      source("00_initially_valid_import.R", local = TRUE)

    if(initially_valid_import() == 1) {

      hide('imported_progress')

      setProgress(detail = "Unzipping...", value = .10)
      list_of_files <- unzip(
        zipfile = upload_filepath, 
        files = paste0(unique(cols_and_data_types$File), ".csv"),
        exdir = tempdir()
      )
        setProgress(detail = "Reading your files..", value = .2)
        source("01_get_Export.R", local = TRUE)
        
        source("02_export_dates.R", local = TRUE)

        setProgress(detail = "Checking file structure", value = .35)
        source("03_file_structure_analysis.R", local = TRUE)
        
        # if structural issues were not found, keep going
        if (session$userData$valid_file == 1) {
          setProgress(detail = "Prepping initial data..", value = .4)
          source("04_initial_data_prep.R", local = TRUE) 
          
          # Create DQ and PDDE script environment
          for(obj in ls(.GlobalEnv, all.names=TRUE)) {
            assign(obj, get(obj, .GlobalEnv), envir = menv)
          }
          assign("session", session, envir = menv)
          
          setProgress(detail = "Assessing your data quality..", value = .7)
          dq_mirai <- mirai(source("05_DataQuality.R"), menv)
          
          setProgress(detail = "Checking your PDDEs", value = .85)
          pdde_mirai <- mirai(source("06_PDDE_Checker.R"), menv)
            
          setProgress(detail = "Preparing System Overview Data", value = .85)
          source("07_system_overview.R", local = TRUE)
            
          collect_mirai(list(dq_mirai, pdde_mirai))
          
          session$userData$pdde_main <- pdde_mirai$data$value
          session$userData$dq_main_df <- dq_mirai$data$value$dq_main
          session$userData$base_dq_data_func <- dq_mirai$data$value$base_dq_data
          session$userData$overlap_details <- dq_mirai$data$value$overlap_details
          
          # if user changes filters, update the reactive vals
          # which get used for the various System Overview charts
          observeEvent({
            input$syso_hh_type
            input$syso_level_of_detail
            input$syso_project_type
            input$methodology_type
            input$syso_age
            input$syso_spec_pops
            input$syso_gender
            input$syso_race_ethnicity
          }, {
            # hide download buttons if < 11 records
            # All Served is handled in system_composition_server.R
            # for that chart, we also hide if all *cells* are < 11
            shinyjs::toggle("sys_inflow_outflow_download_btn", condition = nrow(sys_plot_data$inflow_outflow_full()) > 10)
            shinyjs::toggle("sys_inflow_outflow_download_btn_ppt", condition = nrow(sys_plot_data$inflow_outflow_full()) > 10)
            
            shinyjs::toggle("sys_status_download_btn", condition = sum(sys_plot_data$sankey()$freq) > 10)
            shinyjs::toggle("sys_status_download_btn_ppt", condition = sum(sys_plot_data$sankey()$freq) > 10)
          })
          
          setProgress(detail = "Done!", value = 1)
          logToConsole("Done processing")
          
          logToConsole("Upload processing complete")
          
          # browser()
          
          if(nrow(session$userData$file_structure_analysis_main) > 0) {
            msg <- "Congratulations! You have successfully uploaded a hashed HMIS 
                  CSV Export to Eva! Your upload has file structure errors, but 
                  none are High Priority. Thus, Eva can read your file and you can
                  move forward with utilizing the rest of Eva. However, still 
                  please share the identified file structure issues with your HMIS
                  vendor to fix."
            
            if("Impermissible characters" %in% c(session$userData$file_structure_analysis_main$Issue)) {
              showModal(
                modalDialog(
                  HTML(paste0(msg, "<br><br>", "Additionally, Eva has detected 
                  impermissible characters in your upload. Please note that these 
                  characters may cause Eva to crash.")),
                  title = "Successful Upload: No High Priority File Structure Errors",
                  easyClose = TRUE,
                  footer = modalButton("OK")
                )
              )
            } else {
              showModal(
                modalDialog(
                  msg,
                  title = "Successful Upload: No High Priority File Structure Errors",
                  easyClose = TRUE,
                  footer = modalButton("OK")
                )
              )
            }
          } else {
            showModal(
              modalDialog(
                "Congratulations! You have successfully uploaded a hashed HMIS 
                CSV Export to Eva! Your upload has none of the file structure 
                errors Eva checks for. Thus, Eva can read your file, and you can 
                move forward with utilizing the rest of Eva.",
                title = "Successful Upload: No file structure errors",
                easyClose = TRUE,
                footer = modalButton("OK")
              )
            )
          }
          
          
          shinyjs::show("fileStructureAnalysis")
          
          logMetadata("Successful upload")
          
          logToConsole("Updating inputs")
          
          
          # Update inputs --------------------------------
          if(is.null(input$imported) & !isTruthy(input$in_demo_mode)) {
            logToConsole("User is in upload processing but imported is null and demo_mode is not on")
          } else {
            # mark the "uploaded file" as demo.zip
            if(isTruthy(input$in_demo_mode)) {
              shinyjs::runjs(str_glue("
                $('#imported')
                  .closest('.input-group-btn')
                  .next()
                  .val('demo.zip');
              "))
            }
            
            updatePickerInput(session = session,
                              inputId = "currentProviderList",
                              choices = sort(Project$ProjectName))
            
            updatePickerInput(session = session,
                              inputId = "orgList",
                              choices = c(unique(sort(Organization$OrganizationName))))
            
            updateDateRangeInput(session = session,
                                 inputId = "dateRangeCount",
                                 min = session$userData$meta_HUDCSV_Export_Start,
                                 start = session$userData$meta_HUDCSV_Export_Start,
                                 max = session$userData$meta_HUDCSV_Export_End,
                                 end = session$userData$meta_HUDCSV_Export_End)
          }
          
        } else{ # if structural issues were found, reset gracefully
          session$userData$valid_file <- 0
          showModal(
            modalDialog(
              "Your uploaded HMIS CSV Export has at least one High Priority File 
              Structure Error. To be able to read an uploaded hashed HMIS CSV 
              Export, Eva requires the .zip file to have zero High Priority File 
              Structure Errors. Thus, to use Eva, your upload must have zero High 
              Priority File Structure Errors. Please share the file structure 
              issues, prioritizing the High Priotity File Structure Errrors, 
              with your HMIS vendor to fix.",
              easyClose = TRUE,
              title = "Unsuccessful Upload: Your HMIS CSV Export is not
              structurally valid",
              footer = modalButton("OK")
            )
          )
          
          logMetadata("Unsuccessful upload - not structurally valid")
          
          reset_postvalid_components()
        }
        toggle_sys_components(session$userData$valid_file == 1)
      }
    })
  }
  
  observeEvent(input$imported, {
    process_upload(input$imported$name, input$imported$datapath)
  }, ignoreInit = TRUE)
  
  # File Structure Analysis Summary -----------------------------------------
  # update_fsa <- function() {
  output$fileStructureAnalysis <- renderDT({
    req(nrow(session$userData$file_structure_analysis_main))
    req(initially_valid_import() == 1)
    a <- session$userData$file_structure_analysis_main %>%
      group_by(Type, Issue) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      arrange(Type, desc(Count))

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
  })
  
  # File Structure Analysis Download ----------------------------------------
  
  output$downloadFileStructureAnalysisBtn <- renderUI({
    req(nrow(session$userData$file_structure_analysis_main) > 0)
    downloadButton("downloadFileStructureAnalysis",
                   "Download Structure Analysis Detail")
  }) 
  
  output$downloadFileStructureAnalysis <- downloadHandler(
    filename = date_stamped_filename("File-Structure-Analysis-"),
    content = function(file) {
      write_xlsx(
        session$userData$file_structure_analysis_main %>%
          arrange(Type, Issue) %>%
          nice_names(),
        path = file
      )

      logMetadata(paste0("Downloaded File Structure Analysis Report", 
                         if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      
      exportTestValues(file_structure_analysis_main = session$userData$file_structure_analysis_main %>% nice_names())
    }
  )
  
  output$downloadImpermissibleCharacterDetailBtn <- renderUI({
    # browser()
    req("Impermissible characters" %in% c(session$userData$file_structure_analysis_main$Issue))
    tagList(
      actionButton("showDownloadImpermissibleButton",
                   "Download Impermissible Character Detail", 
                   icon("download")),
      downloadButton("downloadImpermissibleCharacterDetail",
                     "Download Impermissible Character Detail", style="visibility:hidden;")
    )
  })
  
  output$downloadImpermissibleCharacterDetail <- downloadHandler(
    filename = date_stamped_filename("Impermissible-Character-Locations-"),
    content = function(file) {
      bracket_files_detail <- bracket_files_detail()
      
      write_xlsx(
        bracket_files_detail %>% nice_names(),
        path = file
      )
      
      logMetadata(paste0("Impermissible Character Locations Report", 
                         if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      
      exportTestValues(bracket_files_detail = bracket_files_detail)
    }
  )
  
  observeEvent(input$showDownloadImpermissibleButton, {
    showModal(modalDialog(
      "The Impermissible Character Detail export identifies the precise location 
      of all impermissible characters in your HMIS CSV export. 
      Therefore, it can take up to several minutes to run. To proceed with this 
      export, please click Continue.",
      title = "Confirmation",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmDownload", "Continue")
      )
    ))
  })
  
  observeEvent(input$confirmDownload, {
    removeModal()
    shinyjs::click("downloadImpermissibleCharacterDetail")
  })
  # }
  
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
  #   req(session$userData$valid_file == 1)
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
  #       req(session$userData$valid_file == 1)
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
  
  # Client Counts -----------------------------------------------------------
  
  source("client_counts_functions.R", local = TRUE)
  
  output$validate_plot <- renderPlot({
    req(session$userData$valid_file == 1)
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
  
  # CLIENT COUNT DETAILS - APP ----------------------------------------------
  output$clientCountData <- renderDT({
    req(session$userData$valid_file == 1)
    req(nrow(validation()) > 0)
    
    # getting an error sometimes? Warning: Error in filter: â„¹ In argument: `ProjectName == input$currentProviderList`.
    # Caused by error:
    #   ! `..1` must be of size 292 or 1, not size 0.
    
    x <- client_count_data_df() %>%
      filter(ProjectName == input$currentProviderList) %>%
      select(all_of(clientCountDetailCols)) %>%
      nice_names()
    
    datatable(
      x,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    )
  })
  
  
  # CLIENT COUNT SUMMARY - APP ----------------------------------------------
  
  output$clientCountSummary <- renderDT({
    req(session$userData$valid_file == 1)
    
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
    req(session$userData$valid_file == 1)
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
  # }
  
  # PDDE Checker ------------------------------------------------------------
  # PDDE Download Button ----------------------------------------------------
  output$downloadPDDEReportButton  <- renderUI({
    req(session$userData$valid_file == 1)
    req(nrow(session$userData$pdde_main) > 0)
    downloadButton(outputId = "downloadPDDEReport",
                   label = "Download")
  })
  
  
  # Download Button Handler -------------------------------------------------
  
  output$downloadPDDEReport <- downloadHandler(
    
    filename = date_stamped_filename("PDDE Report-"),
    content = function(file) {
      req(session$userData$valid_file == 1)
      browser()
      summary_df <- session$userData$pdde_main %>% 
        group_by(Issue, Type) %>%
        summarise(Count = n()) %>%
        ungroup()
      
      write_xlsx(
        list("Summary" = summary_df,
             "Data" = session$userData$pdde_main %>% 
               left_join(session$userData$Project0 %>% select(ProjectID, ProjectType), by="ProjectID") %>%
               nice_names()
             ),
        path = file)
      
      logMetadata(paste0("Downloaded PDDE Report",
                         if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      
      exportTestValues(pdde_download_summary = summary_df)
      exportTestValues(pdde_main = session$userData$pdde_main %>% nice_names())
    }
  )
  
  # summary table
  output$pdde_summary_table <- renderDT({
    req(session$userData$valid_file == 1)
    a <- session$userData$pdde_main %>%
      group_by(Issue, Type) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      arrange(Type)
    
    exportTestValues(pdde_summary_table = summarize_df(a))
    
    datatable(
      a,
      rownames = FALSE,
      filter = 'none',
      options = list(dom = 't')
    )
  })
  
  # PDDE Guidance -----------------------------------------------------------
  
  output$pdde_guidance_summary <- renderDT({
    req(session$userData$valid_file == 1)
    guidance <- session$userData$pdde_main %>%
      select(Type, Issue, Guidance) %>%
      arrange(Type, Issue) %>%
      unique()
    
    exportTestValues(pdde_guidance_summary = summarize_df(guidance))
    
    datatable(
      guidance, 
      rownames = FALSE,
      escape = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    )
  })
  
  
  # DQ Org Summary -------------------------------------------------------
  source("05_DataQuality_functions.R", local = TRUE)
  
  output$dq_organization_summary_table <- renderDT({
    req(session$userData$valid_file == 1)
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
    
    exportTestValues(dq_organization_summary_table = summarize_df(a))
    
    datatable(
      a,
      rownames = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    )
  })
  
  # DQ Org Guidance -------------------------------------------------------
  
  output$dq_org_guidance_summary <- renderDT({
    req(session$userData$valid_file == 1)
    guidance <- dq_main_reactive() %>%
      filter(OrganizationName %in% c(input$orgList)) %>%
      select(Type, Issue, Guidance) %>%
      mutate(Type = factor(Type, levels = c("High Priority",
                                            "Error",
                                            "Warning"))) %>%
      arrange(Type, Issue) %>%
      unique()
    
    exportTestValues(dq_org_guidance_summary = summarize_df(guidance))
    
    datatable(
      guidance, 
      rownames = FALSE,
      escape = FALSE,
      filter = 'top',
      options = list(dom = 'ltpi')
    )
  })
  
  # Download Org DQ Report --------------------------------------------------
  
  output$downloadOrgDQReportButton  <- renderUI({
    req(session$userData$valid_file == 1)
    req(length(dqDownloadInfo()$orgDQData) > 0)
    downloadButton(outputId = "downloadOrgDQReport",
                   label = "Download")
  })
  
  output$downloadOrgDQReport <- downloadHandler(
    filename = reactive(date_stamped_filename(
      str_glue("{input$orgList} Data Quality Report-"))),
    content = function(file) {
      write_xlsx(dqDownloadInfo()$orgDQData, path = file)
      logMetadata(paste0("Downloaded Org-level DQ Report",
                         if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      exportTestValues(orgDQ_download = summarize_df(dqDownloadInfo()$orgDQData))
    }
  )
  
  # Download System DQ Report -----------------------------------------------
  # button
  output$downloadSystemDQReportButton  <- renderUI({
    req(session$userData$valid_file == 1)
    req(length(dqDownloadInfo()$systemDQData) > 0)
    downloadButton(outputId = "downloadSystemDQReport",
                   label = "Download") %>% withSpinner()
  })
  
  output$downloadSystemDQReport <- downloadHandler(
    filename = date_stamped_filename("Full Data Quality Report-"),
    content = function(file) {
      write_xlsx(dqDownloadInfo()$systemDQData, path = file)
      logMetadata(paste0("Downloaded System-level DQ Report",
                         if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
      exportTestValues(systemDQ_download = summarize_df(dqDownloadInfo()$systemDQData))
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
  #   req(session$userData$valid_file() == 1)
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

  output$orgDQWarningsByIssue_ui <- renderUI({
    renderDQPlot("org", "Warning", "Issue", "#71B4CB")
  })
  
  source("fsa_server.R", local = TRUE)
  
  # SYSTEM ACTIVITY - SYSTEM OVERVIEW ----------------------------------------
  
  source("system_overview_server.R", local = TRUE)
  
  ## System Inflow/Outflow ----
  session$userData$period_cache <- new.env()
  source("system_inflow_outflow_server.R", local = TRUE)
    
  ## System Composition ----
  source("system_composition_server.R", local = TRUE)

  ## Sankey Chart/System Status ----
  source("system_status_server.R", local = TRUE)
  
  session$onSessionEnded(function() {
    rm(list = ls(session$userData$period_cache), 
       envir = session$userData$period_cache)
    gc()
    memoise::forget(session$userData$get_period_specific_enrollment_categories)
    memoise::forget(session$userData$get_period_specific_nbn_enrollment_services)
    cat(paste0("Session ", session$token, " ended at ", Sys.time()))
    logMetadata("Session Ended")
  })
}
