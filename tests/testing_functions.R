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