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
  app$expect_values(name="just-uploaded")

  app$set_inputs(sidebarmenuid = "tabClientCount")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="client-count")

  app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
  app$set_inputs(sidebarmenuid = "tabPDDE")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="pdde")

  app$set_inputs(sidebarmenuid = "tabDQSystem")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="dq-system")
  
  app$set_inputs(sidebarmenuid = "tabDQOrg")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="dq-org")

  app$set_inputs(sidebarItemExpanded = "SystemPerformance")
  app$set_inputs(sidebarmenuid = "systemOverview")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-flow-summary")

  app$set_inputs(sys_inflow_outflow_subtabs = "Detail Chart")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-flow-detail")

  # change universe filters
  app$set_inputs(syso_hh_type = "AO")
  app$set_inputs(syso_project_type = "Residential")
  app$expect_values(name="sys-flow-detail-w-AO-Residential")

  # go back to summary tab
  app$set_inputs(sys_inflow_outflow_subtabs = "Summary")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-flow-summary-w-AO-Residential")

  # go to information
  app$set_inputs(sys_inflow_outflow_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-flow-information")

  app$set_inputs(syso_tabbox = "Client System Status")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-status-chart")

  app$set_inputs(sys_status_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-status-information")

  app$set_inputs(syso_tabbox = "Composition of All Served")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-comp-chart-default")

  app$set_inputs(system_composition_selections = c("All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+05)
  app$expect_values(name="sys-comp-all-re")

  app$set_inputs(system_composition_selections = c("Gender", "All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+05)
  app$expect_values(name="sys-comp-all-re-gender")

  app$set_inputs(sys_comp_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name="sys-comp-information")
  
  app$expect_download("downloadFileStructureAnalysis", name="File-Structure-Analysis-Download")
  app$expect_download("downloadClientCountsReport", name="Client-Counts-Download")
  app$expect_download("downloadPDDEReport", name="PDDE-Download")
  app$expect_download("downloadSystemDQReport", name="System-DQ-Download")
  app$expect_download("downloadOrgDQReport", name="Org-DQ-Download")
  app$expect_download("sys_inflow_outflow_download_btn", name="System-Flow-Download")
  app$expect_download("sys_inflow_outflow_download_btn_ppt", name="System-Flow-Download-PPT")
  app$expect_download("sys_status_download_btn", name="System-Status-Download")
  app$expect_download("sys_status_download_btn_ppt", name="System-Status-Download-PPT")
  app$expect_download("sys_comp_download_btn", name="System-Composition-Download")
  app$expect_download("sys_comp_download_btn_ppt", name="System-Composition-Download-PPT")
  
  app$expect_values(export = TRUE, name="exportTestValues")
})