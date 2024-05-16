customDownload <- function(app, downloadHandler, fname) {
  print(paste("downloading",downloadHandler))
  app$get_download(downloadHandler, paste0(fname, ".xlsx"))
  file.remove(paste0(fname,".xlsx"))
}
main_test_script <- function(test_script_name, test_dataset) {
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))
    
    app <- AppDriver$new(
      variant = platform_variant(os_name = FALSE), 
      name = test_script_name, 
      seed = 12345,
      load_timeout = 2e+05)

    app$set_inputs(Go_to_upload = "click")
    app$wait_for_idle(timeout = 2e+05)
    app$upload_file(imported = paste0(here("tests/"),test_dataset))

    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadFileStructureAnalysis","File-Structure-Analysis-Download")
    app$expect_values()

    app$set_inputs(sidebarmenuid = "tabClientCount")
    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadClientCountsReport", "Client-Counts-Download")
    app$expect_values()

    app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
    app$set_inputs(sidebarmenuid = "tabPDDE")
    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadPDDEReport", "PDDE-Download")
    app$expect_values()

    app$set_inputs(sidebarmenuid = "tabDQSystem")
    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadSystemDQReport", "System-DQ-Download")

    app$set_inputs(sidebarmenuid = "tabDQOrg")
    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadOrgDQReport", "Org-DQ-Download")
  })
}

initially_invalid_test_script <- function(test_script_name, test_dataset) {
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))

    app <- AppDriver$new(
        variant = platform_variant(os_name = FALSE), 
        name = test_script_name, 
        seed = 12345,
        load_timeout = 2e+05)
  
    print("Finished running app init")
    app$set_inputs(Go_to_upload = "click")
    app$upload_file(imported = paste0(here("tests/temp/"),test_dataset))
    app$wait_for_idle(timeout = 2e+05)
    app$expect_values()
  
    app$set_inputs(sidebarmenuid = "tabClientCount")
    app$wait_for_idle(timeout = 2e+05)
    app$expect_values()
  
    app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
    app$set_inputs(sidebarmenuid = "tabPDDE")
    app$wait_for_idle(timeout = 2e+05)
    app$expect_values()
  
    app$set_inputs(sidebarmenuid = "tabDQSystem")
    app$wait_for_idle(timeout = 2e+05)
    app$expect_values()
  
    app$set_inputs(sidebarmenuid = "tabDQOrg")
    app$wait_for_idle(timeout = 2e+05)
    app$expect_values()
  })
}