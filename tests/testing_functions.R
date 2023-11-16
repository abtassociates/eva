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
      load_timeout = 1e+05)

    app$set_inputs(Go_to_upload = "click")
    app$upload_file(imported = paste0("../",test_dataset))
    app$set_inputs(fileStructureAnalysis_state = NULL, allow_no_input_binding_ = TRUE)
    customDownload(app, "downloadFileStructureAnalysis","File-Structure-Analysis-Download")
    app$expect_values()

    app$set_inputs(sidebarmenuid = "tabClientCount")
    app$wait_for_idle() #wait until whole application is idle for 500ms
    customDownload(app, "downloadClientCountsReport", "Client-Counts-Download")
    app$expect_values()

    app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
    app$set_inputs(sidebarmenuid = "tabPDDE")
    customDownload(app, "downloadPDDEReport", "PDDE-Download")
    app$wait_for_idle()
    app$expect_values()

    app$set_inputs(sidebarmenuid = "tabDQSystem")
    customDownload(app, "downloadSystemDQReport", "System-DQ-Download")

    app$set_inputs(sidebarmenuid = "tabDQOrg")
    customDownload(app, "downloadOrgDQReport", "Org-DQ-Download")
  })
}
initially_invalid_test_script <- function(test_script_name, test_dataset) {
  print(paste0("Running ",test_script_name))

  app <- AppDriver$new(
      variant = platform_variant(os_name = FALSE), 
      name = test_script_name, 
      seed = 12345,
      load_timeout = 1e+05)

  app$set_inputs(Go_to_upload = "click")
  app$upload_file(imported = paste0("../temp/",test_dataset))
  app$expect_values()

  app$set_inputs(sidebarmenuid = "tabClientCount")
  app$expect_values()

  app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
  app$set_inputs(sidebarmenuid = "tabPDDE")
  app$expect_values()

  app$set_inputs(sidebarmenuid = "tabDQSystem")
  app$expect_values()

  app$set_inputs(sidebarmenuid = "tabDQOrg")
  app$expect_values()
}