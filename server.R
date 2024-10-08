
function(input, output, session) {
  # record_heatmap(target = ".wrapper")
  # track_usage(storage_mode = store_json(path = "logs/"))
  set.seed(12345)
  # session-wide variables (NOT visible to multiple sessions) -----------------
  visible_reactive_vals <- list(
    # validation <- reactiveVal(),
    CurrentLivingSituation <- reactiveVal(),
    Export <- reactiveVal(),
    Project0 <- reactiveVal(),
    Event <- reactiveVal(),
    meta_HUDCSV_Export_Start <- reactiveVal(),
    meta_HUDCSV_Export_End <- reactiveVal(),
    meta_HUDCSV_Export_Date <- reactiveVal(),
    valid_file <- reactiveVal(0), # from FSA. Most stuff is hidden unless valid == 1
    sys_inflow_outflow_plot_data <- reactiveVal(),
    ReportStart <- reactiveVal(),
    ReportEnd <- reactiveVal(),
    days_of_data <- reactiveVal(1095),
    windowSize <- reactiveVal(1299)
  )
  
  # functions used throughout the app
  source("helper_functions.R", local = TRUE)
  
  # Run scripts on upload ---------------------------------------------------
  
  process_upload <- function(upload_filename, upload_filepath) {
    valid_file(1)
    list_of_files <- unzip(
      zipfile = upload_filepath, 
      files = paste0(unique(cols_and_data_types$File), ".csv"))
    
    Export(importFile(upload_filepath, "Export"))

    source("01_get_Export.R", local = TRUE)
    source("02_export_dates.R", local = TRUE)
    source("04_initial_data_prep.R", local = TRUE) 
    source("07_system_overview.R", local = TRUE)
    source("09_system_status.R", local = TRUE)
          
    # if user changes filters, update the reactive vals
    # which get used for the various System Overview charts
    # observeEvent({
    #   input$syso_hh_type
    #   input$syso_level_of_detail
    #   input$syso_project_type
    #   input$methodology_type
    #   input$syso_age
    #   input$syso_spec_pops
    #   input$syso_gender
    #   input$syso_race_ethnicity
    # }, {
      sys_inflow_outflow_plot_data(inflow_outflow_df())
    #   sys_df_people_universe_filtered_r(
    #     enrollment_categories_reactive() %>%
    #       select(PersonalID, lookback, lecr, eecr, CorrectedHoH) %>%
    #       inner_join(client_categories, join_by(PersonalID)) %>%
    #       filter(!(lookback == 0 &
    #                   eecr == FALSE & lecr == FALSE)) %>%
    #       group_by(PersonalID) %>%
    #       filter(max(lecr, na.rm = TRUE) == 1 &
    #                 max(eecr, na.rm = TRUE) == 1) %>%
    #       ungroup() %>%
    #       select(colnames(client_categories)) %>%
    #       unique()
    #   )
    #   sankey_plot_data(sankey_plot_df())
      
    #   # hide download buttons if < 11 records
    #   # All Served is handled in system_composition_server.R
    #   # for that chart, we also hide if all *cells* are < 11
    #   shinyjs::toggle("sys_inflow_outflow_download_btn", condition = nrow(sys_inflow_outflow_plot_data()) > 10)
    #   shinyjs::toggle("sys_inflow_outflow_download_btn_ppt", condition = nrow(sys_inflow_outflow_plot_data()) > 10)
      
    #   shinyjs::toggle("sys_status_download_btn", condition = sum(sankey_plot_data()$freq) > 10)
    #   shinyjs::toggle("sys_status_download_btn_ppt", condition = sum(sankey_plot_data()$freq) > 10)
    # })
          
    message("Done processing")
    
    
    message("Upload processing complete")
    
    # showModal(
    #   modalDialog(
    #     title = "Upload successful",
    #     "Congratulations! You have succesfully uploaded an HMIS CSV Export.",
    #     easyClose = TRUE,
    #     footer = modalButton("OK")
    #   )
    # )
          
    # shinyjs::show("fileStructureAnalysis")
          
    # logMetadata("Successful upload")
    
    # logToConsole("Updating inputs")
          
          
    # Update inputs --------------------------------
    toggle_sys_components(valid_file() == 1)
  }
  
  observeEvent(input$imported, {
    process_upload(input$imported$name, input$imported$datapath)
  }, ignoreInit = TRUE)
  
  # SYSTEM ACTIVITY - SYSTEM OVERVIEW ----------------------------------------
  source("system_overview_server.R", local = TRUE)
  source("system_inflow_outflow_server.R", local = TRUE)
  
  session$onSessionEnded(function() {
    logMetadata("Session Ended")
  })
}