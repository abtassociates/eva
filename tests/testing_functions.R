customDownload <- function(downloadHandler, fname) {
  app$waitForShiny()
  print(paste("downloading",downloadHandler))
  app$snapshotDownload(downloadHandler, paste0(fname,".xlsx"))
  file.remove(paste0(app$getSnapshotDir(),"-current/",fname,".xlsx"))
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