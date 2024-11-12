customDownload <- function(app, downloadHandler, fname) {
  print(paste("downloading",downloadHandler))
  app$get_download(downloadHandler, fname)
  file.remove(fname)
}
initially_invalid_test_script <- function(test_script_name, test_dataset) {
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))

    app <- AppDriver$new(
        variant = platform_variant(os_name = FALSE), 
        name = test_script_name, 
        seed = 12345,
        load_timeout = 5e+05)
  
    app$set_inputs(Go_to_upload = "click")
    app$wait_for_idle(timeout = 2e+05)
    app$upload_file(imported = paste0(here("tests/temp/"),test_dataset))
    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadFileStructureAnalysis","File-Structure-Analysis-Download")
    app$expect_values()
    
    app$set_inputs(sidebarmenuid = "tabClientCount")
    app$wait_for_idle(timeout = 1e+06)
    app$expect_values()
  
    app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
    app$set_inputs(sidebarmenuid = "tabPDDE")
    app$wait_for_idle(timeout = 1e+06)
    app$expect_values()
  
    app$set_inputs(sidebarmenuid = "tabDQSystem")
    app$wait_for_idle(timeout = 1e+06)
    app$expect_values()
  
    app$set_inputs(sidebarmenuid = "tabDQOrg")
    app$wait_for_idle(timeout = 1e+06)
    app$expect_values()
  })
}

handle_helper_data <- function(app, test_script_name, datasetname) {
  snapshot_folder <- glue::glue(
    "tests/testthat/_snaps/{platform_variant(os_name = FALSE)}/{gsub('test-','',test_script_name)}"
  )
  
  old_path <- here(glue::glue("{snapshot_folder}/{datasetname}.csv"))
  new_path <- gsub(".csv", ".new.csv", old_path)
  
  new_df <- app$get_value(export=datasetname)
  
  if(is.null(new_df) & !file.exists(old_path)) return(NULL)
  
  new_df <- as.data.table(new_df)
  fwrite(new_df, file = new_path)
  
  # if no csv yet, create one at the original filepath
  if(!file.exists(old_path)) {
    message(glue::glue("A csv for {datasetname} does not yet exist. Creating one now!"))
    fwrite(new_df, file = old_path)
  } 
  else {
    # otherwise, if new and old are different warn user
    if(!identical(fread(old_path), fread(new_path))) {
      warning(
        paste0(
          "Difference detected in ",
          datasetname,
          ". Use snapshot_review to visualize diff."
        )
      )
      fwrite(new_df, file = new_path)
    } 
    # otherwise, remove the new one
    else {
      file.remove(new_path)
    }
  }
}