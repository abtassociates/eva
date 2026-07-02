
function(input, output, session) {
  # record_heatmap(target = ".wrapper")
  # track_usage(storage_mode = store_json(path = "logs/"))
  set.seed(12345)
  
  upcoming_maintenance_notification <- HTML("")
  if(nchar(upcoming_maintenance_notification) > 1) {
    showModal(
      modalDialog(
        upcoming_maintenance_notification,
        title = "Upcoming Maintenance",
        easyClose = TRUE
      )
    )
  }
  
  windowSize <- reactiveVal(NULL)
  triggerPlot <- reactiveVal(NULL)
  
  reset_app(session)
  
  # Handle if user arrives from external site
  observe({
    req(session$clientData$url_search != "")
   
    nav_select(id = 'pageid', selected = 'tabGlossary', session = session)
    #parseQueryString(session$clientData$url_search))
  })
  
  # while these seem like they could be "globalized" they have output UI elements
  # and are therefore session based

  source(here("server_00_glossary.R"), local = TRUE)
  source(here("server_00_changelog.R"), local = TRUE)
  source(here("server_00_demo_management.R"), local = TRUE)

  logMetadata(session, "Session started")
  
  # set during initially valid processing stop. Rest of processing stops if invalid
  # FSA is hidden unless initially_valid_import() == 1
  # initially_valid_import <- reactiveVal() 
  
  # in_demo_mode <- reactiveVal(value=FALSE)
  # log when user navigate to a tab
  observe({
    logMetadata(session, paste0("User on ",input$pageid,
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  })
  
  
  # Headers -----------------------------------------------------------------
  
  output$headerUpload <-
    headerGeneric(session, "Upload HMIS CSV Export",
                  h4(
                    strong("Export Date: "),
                    format(session$userData$meta_HUDCSV_Export_Date, "%m-%d-%Y at %I:%M %p")
                  ))


  output$headerLocalSettings <- headerGeneric(session, "Edit Local Settings")

  # the reason we split the Client Count header into two is for shinytest reasons
  # this _supp renderUI needed to be associated with an output in order to make 
  # the HTML <div> id the same each time. Without associating with an output, 
  # the id changed each time and the shinytest would catch the difference and fail
  output$headerClientCounts_supp <- renderUI({ 
    req(session$userData$valid_file() == 1)
    organization <- session$userData$Project0 %>%
      fsubset(ProjectName == input$currentProviderList) %>%
      pull(OrganizationName)
    
    h4(organization, "|", input$currentProviderList)
  })
  
  output$headerClientCounts <- headerGeneric(session, "Project Dashboard Report",
                                             htmlOutput("headerClientCounts_supp"))
  
  output$headerPDDE <- headerGeneric(session, "Project Descriptor Data Elements Checker")
  
  output$headerSystemDQ <- headerGeneric(session, "System-level Data Quality")
  
  output$headerDQOrg_supp <- renderUI({ 
    req(session$userData$valid_file() == 1)
    
    h4(input$orgList)
  })
  output$headerDataQuality <- headerGeneric(session, "Organization-level Data Quality",
                                            htmlOutput("headerDQOrg_supp"))
  
  output$headerDQExport <- headerGeneric(session, "Data Quality Export Interface")
  
  output$headerSystemOverview <- headerGeneric(session, "System Overview")

  output$headerSystemExit <- headerGeneric(session, "System Exits")

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
  
  # Detect user clicking to go to upload page -------------------------------
  observeEvent(input$Go_to_upload, {
    nav_select(id = "pageid", selected = "tabUpload", session = session)
  }) 
  
  # Timeout detection ---------------------
  observeEvent(input$timeOut, {
    logMetadata(session, "Timed out")
    session$reload()
  })
  
  source("server_01_upload.R", local=TRUE)
  
  source("server_03_file_structure_analysis.R", local = TRUE)
  
  source("server_05_data_quality.R", local = TRUE)
  
  source("server_06_client_counts.R", local = TRUE)
  source("server_06.1_client_level_export.R", local=TRUE)
  
  
  source("functions_07_system_performance.R", local = TRUE)
  
  source("server_07_system_overview.R", local = TRUE)
  source("server_07.1_system_composition.R", local = TRUE)
  source("server_07.2_system_inflow_outflow.R", local = TRUE)
  source("server_07.3_system_status.R", local = TRUE)
  
  source("server_07.4_system_exits.R", local = TRUE)
  source("server_07.4.1_syse_types.R", local = TRUE)
  source("server_07.4.2_syse_time.R", local = TRUE)
  source("server_07.4.3_syse_subpop.R", local = TRUE)
  source("server_07.4.4_syse_phd.R", local = TRUE)
  
  # Handle session end --------------------------------------------------------
  session$onSessionEnded(function(){
    gc()
    cat(paste0("Session ", session$token, " ended at ", Sys.time()))
    logMetadata(session, "Session Ended")
  })
}
