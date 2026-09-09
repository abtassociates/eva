bracket_regex <- "[\\[\\]<>\\{}]"

# Function to detect non-UTF-8 characters and brackets
detect_bracket_characters <- function(dt, file, cols_to_check, key_info) {
  # Vectorized detection for non-UTF-8 and bracket issues
  # Identify character columns only
  lapply(cols_to_check, function(col) {
    bracket_rows <- stringi::stri_detect_regex(dt[[col]], bracket_regex)
    bracket_values <- fsubset(dt[[col]], bracket_rows)
    data.table(
      CSV = file,
      Column = col,
      Detail = paste0("Text with impermissible characters: ", bracket_values, glue_data(dt[bracket_rows], key_info))
    )
  }) |> rowbind()
}

bracket_files_detail <- function() {
  impermissible_problems <- session$userData$file_structure_analysis_main() |> 
    fsubset(Issue == "Impermissible Characters", CSV, Column, Issue) |>
    funique() |>
    join(
      specs_rules |> fselect(CSV, "Column" = Name, Issue, `Key Fields`),
      on = c("CSV", "Column", "Issue")
    )
  
  file_list <- funique(impermissible_problems$CSV)
  
  withProgress(
    message = "Downloading Impermissible Character Export...",
    lapply(file_list, function(file) {
      cols_to_keep <- funique(na.omit(unlist(
        lapply(impermissible_problems[CSV == file, .(Column, `Key Fields`)], as.character), 
        use.names = FALSE
      )))
      idx <- whichv(impermissible_problems$CSV, file)
      cols_to_check <- as.character(impermissible_problems$Column[idx])
      key_fields    <- as.character(impermissible_problems$`Key Fields`[idx])
      key_info <- fifelse(is.na(key_fields), "", gsub("([A-Za-z0-9_.]+)", ". Key Info: \\1 {\\1}", key_fields))
      
      path <- utils::unzip(
        zipfile = input$imported$datapath, 
        files=paste0(file, ".csv"), 
        exdir=dirname(tempfile())
      )
      
      dt <- fread(path, select = union(cols_to_check, key_fields))
      
      incProgress(1 / fnrow(impermissible_problems))
      detect_bracket_characters(dt, file, cols_to_check, key_info)
    }) |> rowbind()
  )
}

# File Structure Analysis Summary -----------------------------------------
# update_fsa <- function() {
output$fileStructureAnalysis <- renderDT({
  req(session$userData$initially_valid_import() == 1)
  
  a <- session$userData$file_structure_analysis_main()
  req(!is.null(a))
  
  if(fnrow(a) > 0)
    a <- a %>%
      fgroup_by(Priority, Issue) %>%
      fsummarise(Count = fnrow(.)) %>%
      roworder(Priority, -Count)
  
  datatable(
    a,
    rownames = FALSE,
    filter = 'none',
    options = list(dom = 't', 
                   language = list(
                     zeroRecords = "No file structure analysis issues! 
                      Visit Eva's other pages to explore data quality and system performance.")
    ),
    style = "default"
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
        roworder(Priority, Issue) %>%
        nice_names(),
      path = file
    )
    
    logMetadata(session, paste0("Downloaded File Structure Analysis Report", 
                       if_else(isTruthy(input$in_demo_mode), " - DEMO MODE", "")))
    
    exportTestValues(file_structure_analysis_main = session$userData$file_structure_analysis_main() %>% nice_names())
  }
)

output$downloadImpermissibleCharacterDetailBtn <- renderUI({
  # browser()
  req("Impermissible Characters" %in% session$userData$file_structure_analysis_main()$Issue)
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
    
    logMetadata(session, paste0("Impermissible Character Locations Report", 
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
