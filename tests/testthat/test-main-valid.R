test_script_name <- "test-main-valid"
test_dataset_folder <- "tests/temp/"
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
  
  app$set_inputs(sidebarItemExpanded = "SystemPerformance")
  app$set_inputs(sidebarmenuid = "systemOverview")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
  app$set_inputs(sys_inflow_outflow_subtabs = "Detail Chart")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
  # change universe filters
  app$set_inputs(syso_hh_type = "AO")
  app$set_inputs(syso_project_type = "Residential")
  app$expect_values()
  
  # go back to summary tab 
  app$set_inputs(sys_inflow_outflow_subtabs = "Summary")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
  # go to information
  app$set_inputs(sys_inflow_outflow_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
  customDownload(app, "sys_inflow_outflow_download_btn", "System-Flow-Download")
  customDownload(app, "sys_inflow_outflow_download_btn_ppt", "System-Flow-Download-PPT")
  
  
  app$set_inputs(syso_tabsetpanel = "Client System Status")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
  app$set_inputs(sys_status_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  customDownload(app, "sys_status_download_btn", "System-Status-Download")
  customDownload(app, "sys_status_download_btn_ppt", "System-Status-Download-PPT")
  
  
  app$set_inputs(syso_tabsetpanel = "Composition of All Served")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
  app$set_inputs(system_composition_selections = c("All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+05)
  app$expect_values()
  
  app$set_inputs(system_composition_selections = c("All Races/Ethnicities", "Gender"))
  app$wait_for_idle(timeout = 2e+05)
  app$expect_values()
  customDownload(app, "sys_comp_download_btn", "System-Composition-Download")
  customDownload(app, "sys_comp_download_btn_ppt", "System-Composition-Download-PPT")
  
  app$set_inputs(sys_comp_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values()
  
})