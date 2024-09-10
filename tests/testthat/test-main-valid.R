test_script_name <- "test-main-valid"
test_dataset_folder <- "tests/temp"
test_dataset <- "FY24-ICF-main-valid.zip"

test_that(paste0("{shinytest2} recording: ",test_script_name), {
  print(paste0("Running ",test_script_name))
  
  app <- AppDriver$new(
    variant = platform_variant(os_name = FALSE), 
    name = test_script_name, 
    seed = 12345,
    load_timeout = 2e+05)
  
  print(paste0("About to click in ",test_script_name))
  
  app$set_inputs(Go_to_upload = "click")
  app$wait_for_idle(timeout = 2e+05)
  app$upload_file(imported = paste0(here(test_dataset_folder),test_dataset))
  
  print(paste0("Just uploaded in ",test_script_name))
  
  app$wait_for_idle(timeout = 1e+06)
  customDownload(app, "downloadFileStructureAnalysis","File-Structure-Analysis-Download")
  app$expect_values()
  
  print(paste0("Just downloaded FSA ",test_script_name))
  
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