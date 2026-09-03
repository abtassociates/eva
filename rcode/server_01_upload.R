process_upload <- function(upload_filename, upload_filepath) {
  hide('imported_progress')
  withProgress({
    
    reset_app(session)
    
    # run script inside tryCatch block, create modal if script fails
    source_trycatch <- function(script_name){
      src_att <- tryCatch(source(script_name, local = parent.env(environment())), 
                          error = function(e) {e})

      if(inherits(src_att, 'simpleError')){
        logToConsole(session, src_att)
        logToConsole(session, paste0("Error occured in ", script_name))
        if(src_att$message != "No valid Continuum records in enrollment_prep")
          show_trycatch_popup(script_name)
        return("err")
      } else if("intentional_stop" %in% class(src_att)) {
        logToConsole(session, paste0("Intentional stop occurred: ", src_att$message))
      }
      
    }
    
    # 00 -------------------
    setProgress(message = "Processing...", value = .01)
    
    setProgress(detail = "Checking initial validity ", value = .05)
    err <- source_trycatch(here("rcode","00_initially_valid_import.R"))
    if(!is.null(err)) return(NULL)

    if(session$userData$initially_valid_import() == 0)
      return(NULL)
    
    setProgress(detail = "Unzipping...", value = .10)
    
    list_of_files <- unzip(
      zipfile = upload_filepath, 
      files = paste0(unique(cols_and_data_types$File), ".csv"),
      exdir = tempdir()
    )
    
    # 01 -------------------
    setProgress(detail = "Reading your files..", value = .2)

    log_memory("before extracting")
    err <- source_trycatch(here("rcode","01_get_export.R"))
    if(!is.null(err)) return(NULL)
    log_memory("after extracting")
    
    # 02 -------------------
    err <- source_trycatch(here("rcode","02_export_dates.R"))
    if(!is.null(err)) return(NULL)
    # 03 -------------------
    setProgress(detail = "Checking file structure", value = .35)
    
    err <- source_trycatch(here("rcode","03_file_structure_analysis.R"))
    if(!is.null(err)) return(NULL)
    
    if(session$userData$valid_file() == 0)
      return(NULL)
    # 04 -------------------
    setProgress(detail = "Prepping initial data..", value = .4)
    
    err <- source_trycatch(here("rcode","04_initial_data_prep.R"))
    if(!is.null(err)) return(NULL)
    log_memory("after 04_initial_data_prep")
    
    # 05 & 06 (MIRAI) -------------------
    setProgress(detail = "Assessing your data quality..", value = .7)
    
    dq_and_pdde_dependencies <- mget(unique(c(dq_mirai_dependencies, pdde_mirai_dependencies)))
    dq_and_pdde_dependencies[["session"]] <- list(
      token = session$token,
      userData = list(
        Project0 = session$userData$Project0,
        meta_HUDCSV_Export_Date = session$userData$meta_HUDCSV_Export_Date,
        meta_HUDCSV_Export_Start = session$userData$meta_HUDCSV_Export_Start,
        meta_HUDCSV_Export_End = session$userData$meta_HUDCSV_Export_End
      )
    )

    deps_df_sizes <- data.frame(
      object_size_mb = vapply(dq_and_pdde_dependencies, function(x) as.numeric(object.size(x)) / 1024^2, numeric(1)),
      stringsAsFactors = FALSE
    ) |> fselect(object_size_mb) |> fmutate(object_size_mb = round(object_size_mb, 1)) |> roworder(-object_size_mb)
  
    logToConsole(session, print(deps_df_sizes))
    
    logToConsole(session, paste0("Total dependency size: ", fsum(deps_df_sizes$object_size_mb), "MB"))
                 
    qs2_filepath <- file.path(
      paste0(tempdir(), "/", session$token),
      "dependencies.qs2"
    )
    dir.create(dirname(qs2_filepath), recursive = TRUE, showWarnings = FALSE)
    
    qs2::qs_save(
      dq_and_pdde_dependencies,
      qs2_filepath
    )
    rm(dq_and_pdde_dependencies)
    
    log_memory("before calling mirai")
    
    dq_pdde_mirai <- mirai({
      # Recreate the same named objects that .args
      # makes available to the worker.
      log_memory("initial start")
      deps <- qs2::qs_read(dependency_filepath)
      unlink(dependency_filepath)
      
      # Unpack into R datasets:
      list2env(deps, envir = environment())
      rm(deps)
      log_memory("after dependencies read in")
      
      logToConsole(session, "About to run dq_mirai")
      source(here("rcode", "05_data_quality.R"), local = TRUE)
      log_memory("after 05_data_quality")
      
      logToConsole(session, "About to run pdde_mirai")
      source(here("rcode", "06_PDDE_checker.R"), local = TRUE)
      log_memory("after 06_PDDE_checker")
      
      res <- list(
        dq_main = dq_main,
        overlap_details = overlap_details,
        outstanding_referrals = outstanding_referrals,
        pdde_main = pdde_main,
        long_stayers = long_stayers
      )
      
      rm(list = setdiff(ls(all.names = TRUE), "res"))
      
      release_worker_memory()
      
      log_memory("after release mirai memory")
      res
    }, .args =  list(dependency_filepath = qs2_filepath)
    ) %...>% (function(dq_pdde_results) {
      # Store results of DQ and PDDE ------------------------------------------
      # dq_pdde_results <- .[]

      unlink(qs2_filepath)
      logToConsole(session, "saving DQ and PDDE results to session")
      session$userData$pdde_main <- dq_pdde_results$pdde_main
      session$userData$dq_main <- dq_pdde_results$dq_main
      session$userData$overlap_details <- dq_pdde_results$overlap_details
      session$userData$outstanding_referrals <- dq_pdde_results$outstanding_referrals
      session$userData$long_stayers <- dq_pdde_results$long_stayers
      session$userData$dq_pdde_mirai_complete(1)
      
      log_memory("after mirai returns results")
    }) %...!% {
      unlink(qs2_filepath)
      
      logToConsole(session, paste0("dq_pdde_results mirai failed with error: ", .))
      show_trycatch_popup("05_DataQuality.R / 06_PDDE_Checker.R")
      if(IN_DEV_MODE) browser()
    }
    
    # 07 -------------------
    ## if only project type is HP (12), skip System Overview script and hide Sys Perf tab
    err <- source_trycatch(here("rcode", "07_system_performance.R"))
    if(!is.null(err)) {
      nav_hide(id = 'pageid', target = "menuSysPerf", session = session)
    } else {
      nav_show(id = 'pageid', target = "menuSysPerf", session = session)
      setProgress(detail = "Preparing System Overview Data", value = .85)
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
              .val('./util/demo.zip');
          "))
      }
      
      updatePickerInput(session = session,
                        inputId = "currentProviderList",
                        choices = setNames(Project$ProjectID[order(Project$ProjectName)],sort(Project$ProjectName)))
      
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
    
    toggle_sys_components(prefix='sys', session$userData$valid_file() == 1)
    toggle_sys_components(prefix = 'syse', session$userData$valid_file() == 1)
    
    log_memory(paste0("Upload processing complete. Mirai still processing? ", mirai::unresolved(dq_pdde_mirai)))
  })
}

observeEvent(input$imported, {
  process_upload(input$imported$name, input$imported$datapath)
  session$sendCustomMessage("uploaded_file", TRUE)
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
