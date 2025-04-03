bracket_regex <- "[\\[\\]<>\\{}]"

# Function to detect non-UTF-8 characters and brackets
detect_bracket_characters <- function(dt, file) {
  # Vectorized detection for non-UTF-8 and bracket issues
  # Identify character columns only
  char_cols <- names(dt)[sapply(dt, is.character)]
  if (length(char_cols) == 0) next
  
  results <- lapply(char_cols, function(col) {
    bracket_rows <- which(grepl(bracket_regex, dt[[col]], perl=TRUE))
    if (length(bracket_rows) == 0) return(NULL)
    
    data.table(
      File = file,
      Location = paste0("column ", col, ", row ", bracket_rows),
      Text = dt[[col]][bracket_rows]
    )
  })
  
  rbindlist(results, use.names = TRUE, fill = TRUE)
}

bracket_files_detail <- function() {
  file_list <- unique(cols_and_data_types$File)
  withProgress(
    message = "Downloading Impermissible Character Export...", {
    results <- lapply(file_list, function(file) {
      dt <- importFile(upload_filepath = input$imported$datapath, csvFile = file)  # Load file
      incProgress(1 / length(file_list))
      detect_bracket_characters(dt, file)
    })
    
    rbindlist(results, use.names = TRUE, fill = TRUE)
  })
}

# File Structure Analysis Summary -----------------------------------------
# update_fsa <- function() {
output$fileStructureAnalysis <- renderDT({
  req(session$userData$initially_valid_import() == 1)

  a <- session$userData$file_structure_analysis_main() %>%
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
  req(nrow(session$userData$file_structure_analysis_main()) > 0)
  downloadButton("downloadFileStructureAnalysis",
                 "Download Structure Analysis Detail")
}) 

output$downloadFileStructureAnalysis <- downloadHandler(
  filename = date_stamped_filename("File-Structure-Analysis-"),
  content = function(file) {
    write_xlsx(
      session$userData$file_structure_analysis_main() %>%
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
  req("Impermissible characters" %in% c(session$userData$file_structure_analysis_main()$Issue))
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