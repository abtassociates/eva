process_upload <- function(upload_filename, upload_filepath) {
  hide('imported_progress')
  withProgress({
    
    # run script inside tryCatch block, create modal if script fails
    source_trycatch <- function(script_name){
      src_att <- if(!IN_DEV_MODE) {
        tryCatch(source(script_name, local = parent.env(environment())), 
                          error = function(e) {e})
      } else {
        source(script_name, local = parent.env(environment()))
      }
      
      if(inherits(src_att, 'simpleError')){
        logToConsole(session, src_att)
        logToConsole(session, paste0("Error occured in ", script_name))
        show_trycatch_popup(script_name)
        return("err")
      } else {}
      
    }
    
    setProgress(message = "Processing...", value = .01)
    
    setProgress(detail = "Checking initial validity ", value = .05)

    err <- source_trycatch("00_initially_valid_import.R")
    if(!is.null(err)) return(NULL)

    if(session$userData$initially_valid_import() == 0)
      return(NULL)
    
    setProgress(detail = "Unzipping...", value = .10)
    list_of_files <- unzip(
      zipfile = upload_filepath, 
      files = paste0(unique(cols_and_data_types$CSV), ".csv"),
      exdir = tempdir()
    )
    
    setProgress(detail = "Reading your files..", value = .2)

    err <- source_trycatch("01_get_Export.R")
    if(!is.null(err)) return(NULL)
    
    err <- source_trycatch("02_export_dates.R")
    if(!is.null(err)) return(NULL)
    
    setProgress(detail = "Checking file structure", value = .35)
    
    err <- source_trycatch("03_file_structure_analysis.R")
    if(!is.null(err)) return(NULL)
    
    if(session$userData$valid_file() == 0)
      return(NULL)
    
    setProgress(detail = "Prepping initial data..", value = .4)
    
    err <- source_trycatch("04_initial_data_prep.R")
    if(!is.null(err)) return(NULL)
    
    setProgress(detail = "Assessing your data quality..", value = .7)

    dq_and_pdde_dependencies <- mget(unique(c(
      dq_mirai_dependencies, 
      pdde_mirai_dependencies, 
      dq_null_unless_rules$CSV, # make sure to pull in any CSVs with null-unless checks 
      pdde_null_unless_rules$CSV
    )))
    for(i in specs_mirai_dependencies) {
      dq_and_pdde_dependencies[[i]] <- get(i, envir = .GlobalEnv)
    }
    
    dq_and_pdde_dependencies[["session"]] <- list(
      token = session$token,
      userData = list(
        Project0 = session$userData$Project0,
        meta_HUDCSV_Export_Date = session$userData$meta_HUDCSV_Export_Date,
        meta_HUDCSV_Export_End = session$userData$meta_HUDCSV_Export_End,
        validation = session$userData$validation
      )
    )
    dq_pdde_mirai <- mirai({
      local_env <- environment()
      # Setting enclosing environments for specs stuff -------
      # These functions are originally stored in the global environment, 
      # but when passed to the mirai are stored in the mirai's local environment 
      # via load_machine_readable_specs.R
      # When they run and call another function, e.g. get_null_unless_issue_records 
      # calls join_prereqs, they look for that function in their enclosing environment. 
      # But when we pass them to the mirai, nothing lives in the global environment anymore
      # By resetting the enclosing environment we make sure those sub-functions are found. 
      environment(get_null_unless_issue_records) <- local_env
      environment(join_prereqs) <- local_env
      environment(get_foreign_key_issues) <- local_env
      environment(add_reporting_info) <- local_env
      
      logToConsole(session, "About to run dq_mirai")
      source("05_DataQuality.R", local = TRUE)
      
      logToConsole(session, "About to run pdde_mirai")
      source("06_PDDE_Checker.R", local = TRUE)
      
      list(
        dq_main = dq_main,
        overlap_details = overlap_details,
        outstanding_referrals = outstanding_referrals,
        pdde_main = pdde_main,
        long_stayers = long_stayers
      )
    }, .args = dq_and_pdde_dependencies) %...>% {
      # Store results of DQ and PDDE ------------------------------------------
      dq_pdde_results <- .[]

      logToConsole(session, "saving DQ and PDDE results to session")
      session$userData$pdde_main <- dq_pdde_results$pdde_main
      session$userData$dq_main <- dq_pdde_results$dq_main
      session$userData$overlap_details <- dq_pdde_results$overlap_details
      session$userData$outstanding_referrals <- dq_pdde_results$outstanding_referrals
      session$userData$long_stayers <- dq_pdde_results$long_stayers
      session$userData$dq_pdde_mirai_complete(1)
    } %...!% {
      logToConsole(session, paste0("dq_pdde_results mirai failed with error: ", .))
      show_trycatch_popup("05_DataQuality.R / 06_PDDE_Checker.R")
      if(IN_DEV_MODE) browser()
    }
    ## if only project type is HP (12), skip System Overview script and hide Sys Perf tab
    if(all(EnrollmentAdjust$ProjectType == 12)){
      logToConsole(session, "Only HP enrollments found - skipping System Performance")
      nav_hide(id = 'pageid', target = "tabSystemOverview", session = session)
    } else {
     
      err <- source_trycatch("07_system_overview.R")
      if(!is.null(err)) {
        nav_hide(id = 'pageid', target = "tabSystemOverview", session = session)
      } else {
        nav_show(id = 'pageid', target = "tabSystemOverview", session = session)
        setProgress(detail = "Preparing System Overview Data", value = .85)
      }
    }
    
    
    setProgress(detail = "Done!", value = 1)
    
    logToConsole(session, "Done processing")
    
    # Show successful upload pop-up ----------------------------------------
    if(input$in_demo_mode) {
      # do not display if in demo mode
    } else if(nrow(session$userData$file_structure_analysis_main()) > 0) {
      msg <- "Congratulations! You have successfully uploaded a hashed HMIS 
              CSV Export to Eva! Your upload has file structure errors, but 
              none are High Priority. Thus, Eva can read your file and you can
              move forward with utilizing the rest of Eva. However, still 
              please share the identified file structure issues with your HMIS
              vendor to fix."
      
      if("Impermissible characters" %in% c(session$userData$file_structure_analysis_main()$Issue)) {
        msg <- HTML(paste0(msg, "<br><br>", "Additionally, Eva has detected 
              impermissible characters in your upload. Please note that these 
              characters may cause Eva to crash."))
      }
      showModal(
        modalDialog(
          msg,
          title = "Successful Upload: No High Priority File Structure Errors",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
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
    
    logMetadata(session, "Successful upload")
    
    logToConsole(session, "Updating inputs")
    
    
    # Update inputs -----------------------------------------------------------
    if(is.null(input$imported) & !isTruthy(input$in_demo_mode)) {
      logToConsole(session, "PROBLEM: User is in upload processing but imported is null and demo_mode is not on")
      stop(
        str_squish(
          "Eva encountered a problem. Please submit an issue on GitHub and note the
             date and time in order to help the team diagnose the issue."
        )
      )
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
      
      updatePickerInput(session = session,
                        inputId = 'dq_export_orgList',
                        choices = sort(unique(Organization$OrganizationName)),
                        options = pickerOptions(
                          selectedTextFormat = paste("count >", length(unique(Organization$OrganizationName))-1),
                        ), selected = sort(unique(Organization$OrganizationName)))
      
      updateDateRangeInput(session = session,
                           inputId = "dateRangeCount",
                           min = session$userData$meta_HUDCSV_Export_Start,
                           start = session$userData$meta_HUDCSV_Export_Start,
                           max = session$userData$meta_HUDCSV_Export_End,
                           end = session$userData$meta_HUDCSV_Export_End)
      
      updateDateRangeInput(session = session,
                           inputId = "dq_export_date_multiple",
                           min = session$userData$meta_HUDCSV_Export_Start,
                           start = session$userData$meta_HUDCSV_Export_Start,
                           max = session$userData$meta_HUDCSV_Export_End,
                           end = session$userData$meta_HUDCSV_Export_End)
      
      updateDateInput(session = session,
                      inputId = "dq_export_date_single",
                      min = session$userData$meta_HUDCSV_Export_Start,
                      max = session$userData$meta_HUDCSV_Export_End,
                      value = session$userData$meta_HUDCSV_Export_End)
      
    }
    
    toggle_sys_components(session$userData$valid_file() == 1)
  })
}

observeEvent(input$imported, {
  process_upload(input$imported$name, input$imported$datapath)
}, ignoreInit = TRUE)

# file upload status text ----------------------------------------------------
output$fileInfo <- renderUI({
  HTML("<p>Please upload your hashed HMIS CSV Export!</p>")
  if(is.null(input$imported)) {
    return("")
  } else if(session$userData$valid_file() == 1) {
    HTML("<p id='successful_upload'>You have successfully uploaded your hashed
           HMIS CSV Export!</p>")
  }
}) 
