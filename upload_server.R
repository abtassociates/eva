process_upload <- function(upload_filename, upload_filepath) {
  hide('imported_progress')
  withProgress({
    setProgress(message = "Processing...", value = .01)
    
    setProgress(detail = "Checking initial validity ", value = .05)
    source("00_initially_valid_import.R", local = TRUE)
    
    if(session$userData$initially_valid_import() == 0) 
      return(NULL)
    
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
    
    if(session$userData$valid_file() == 0)
      return(NULL)
    
    setProgress(detail = "Prepping initial data..", value = .4)
    source("04_initial_data_prep.R", local = TRUE) 

    setProgress(detail = "Assessing your data quality..", value = .7)
    logToConsole(session, "About to run dq_mirai")
    
    dq_mirai <- mirai(
      source("05_DataQuality.R", local = TRUE), 
      .args = mget(dq_mirai_dependencies)
    )

    setProgress(detail = "Checking your PDDEs", value = .85)
    logToConsole(session, "About to run pdde_mirai")
    
    pdde_mirai <- mirai(
      source("06_PDDE_Checker.R", local = TRUE), 
      .args = mget(pdde_mirai_dependencies)
    )
    
    setProgress(detail = "Preparing System Overview Data", value = .85)
    source("07_system_overview.R", local = TRUE)
    
    # Store results of DQ and PDDE ------------------------------------------
    logToConsole(session, "collecting DQ results")
    dq_mirai[]
    if(mirai::is_error_value(dq_mirai$data)) {
      logToConsole(paste0("dq_mirai failed with error: ", dq_mirai$data))
    }

    logToConsole(session, "collecting PDDE results")
    pdde_mirai[]
    if(mirai::is_error_value(pdde_mirai$data)) {
      logToConsole(paste0("pdde_mirai failed with error: ", pdde_mirai$data))
    }
    
    logToConsole(session, "saving DQ and PDDE results to session")
    session$userData$pdde_main <- pdde_mirai$data$value
    session$userData$dq_main_df <- dq_mirai$data$value$dq_main
    session$userData$base_dq_data_func <- dq_mirai$data$value$base_dq_data
    session$userData$overlap_details <- dq_mirai$data$value$overlap_details
    
    setProgress(detail = "Done!", value = 1)
    
    logToConsole(session, "Done processing")
    
    # Show successful upload pop-up ----------------------------------------
    if(nrow(session$userData$file_structure_analysis_main()) > 0) {
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
      
      updateDateRangeInput(session = session,
                           inputId = "dateRangeCount",
                           min = session$userData$meta_HUDCSV_Export_Start,
                           start = session$userData$meta_HUDCSV_Export_Start,
                           max = session$userData$meta_HUDCSV_Export_End,
                           end = session$userData$meta_HUDCSV_Export_End)
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
