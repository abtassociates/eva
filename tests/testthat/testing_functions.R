customDownload <- function(app, downloadHandler, fname) {
  print(paste("downloading",downloadHandler))
  app$get_download(downloadHandler, paste0(fname, ".xlsx"))
  file.remove(paste0(fname,".xlsx"))
}
main_test_script <- function(test_script_name, test_dataset) {
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))
    app <- AppDriver$new(
      variant = platform_variant(), 
      name = test_script_name, 
      seed = 12345,
      view = TRUE,
      load_timeout = 1e+05)
    app$set_inputs(Go_to_upload = "click")
    app$upload_file(imported = paste0("../",test_dataset))
    customDownload(app, "downloadFileStructureAnalysis","File-Structure-Analysis-Download")
    app$expect_values()

    app$set_inputs(sidebarmenuid = "tabClientCount")
    app$wait_for_idle(500) #wait until whole application is idle for 500ms
    customDownload(app, "downloadClientCountsReport", "Client-Counts-Download")
    app$expect_values()

    app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
    app$set_inputs(sidebarmenuid = "tabPDDE")
    app$wait_for_idle(500)
    customDownload(app, "downloadPDDEReport", "PDDE-Download")
    app$expect_values()

    app$set_inputs(sidebarmenuid = "tabDQSystem")
    customDownload(app, "downloadSystemDQReport", "System-DQ-Download")

    app$set_inputs(sidebarmenuid = "tabDQOrg")
    customDownload(app, "downloadOrgDQReport", "Org-DQ-Download")
  })
}
initially_invalid_test_script <- function(test_script_name, test_dataset) {
  print(paste0("Running ",test_script_name))
  app <- ShinyDriver$new("../../", seed = 1234, loadTimeout = 1e+04, shinyOptions = list(test.mode = TRUE))

  app$snapshotInit(test_script_name, screenshot = FALSE)

  app$setInputs(Go_to_upload = "click")
  app$uploadFile(imported = paste0("../temp/",test_dataset)) # <-- This should be the path to the file, relative to the app's tests/shinytest directory
  app$snapshot()
  app$findElement("button[data-dismiss='modal']")$click()
  Sys.sleep(2)
  app$snapshot()
  app$setInputs(sidebarmenuid = "tabClientCount")
  app$snapshot()
  app$setInputs(sidebarItemExpanded = "AssessDataQuality")
  app$setInputs(sidebarmenuid = "tabPDDE")
  app$snapshot()
  app$setInputs(sidebarmenuid = "tabDQSystem")
  app$snapshot()
  app$setInputs(sidebarmenuid = "tabDQOrg")
  app$snapshot()
}