
function(input, output, session) {
  # record_heatmap(target = ".wrapper")
  # track_usage(storage_mode = store_json(path = "logs/"))
  set.seed(12345)
  
  source("helper_functions.R", local = TRUE)
  
  # Upload-specific static variables shared across session --------------------
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
  
  # Handle if user arrives from external site
  observe({
    req(session$clientData$url_search != "")
    updateTabItems(session,
                   "sidebarmenuid",
                   "tabGlossary")
    #parseQueryString(session$clientData$url_search))
  })
  
  source("glossary.R", local = TRUE)
  source("changelog.R", local = TRUE)
  source("demo_management.R", local = TRUE)
  
  logMetadata("Session started")
  
  # Tab Management -----------------------------------------------------------------
  observeEvent(input$sidebarmenuid, { 
    logMetadata(paste0("User on ",input$sidebarmenuid, 
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
  })
  
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
  
  
  observeEvent(input$Go_to_upload, {
    updateTabItems(session, "sidebarmenuid", "tabUpload")
  }) 
  
  # decides when it's time to time out the session
  observeEvent(input$timeOut, {
    logMetadata("Timed out")
    session$reload()
  })
  
  source("upload_server.R", local=TRUE)
  
  source("client_counts_server.R", local = TRUE)
  
  source("fsa_server.R", local = TRUE)
  
  source("data_quality_server.R", local = TRUE)
  
  source("system_overview_server.R", local = TRUE)
  
  source("system_inflow_outflow_server.R", local = TRUE)
    
  source("system_composition_server.R", local = TRUE)

  source("system_status_server.R", local = TRUE)
  
  # Handle session end --------------------------------------------------------
  session$onSessionEnded(function(){
    if("period_cache" %in% ls(session$userData)) {
      rm(list = ls(session$userData$period_cache), 
         envir = session$userData$period_cache)
      gc()
    }
    memoise::forget(session$userData$get_period_specific_enrollment_categories)
    memoise::forget(session$userData$get_period_specific_nbn_enrollment_services)
    cat(paste0("Session ", session$token, " ended at ", Sys.time()))
    logMetadata("Session Ended")
  })
}
