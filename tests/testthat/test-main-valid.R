test_script_name <- "test-main-valid"
test_dataset_folder <- "tests/temp/"
test_dataset <- "FY24-ICF-main-valid.zip"
helper_datasets <- c(
  "non_ascii_files_detail", 
  "client_count_download_detail", 
  "dq_main_reactive",
  "pdde_main",
  "universe_ppl_flags",
  "sys_comp_df"
)

test_that(paste0("{shinytest2} recording: ",test_script_name), {
  print(paste0("Running ",test_script_name))
  testthat::local_edition(3)
  
  app <- AppDriver$new(
    variant = platform_variant(os_name = FALSE), 
    name = test_script_name, 
    seed = 12345,
    width = 1920,
    height = 1080,
    # shiny_args=list(host="172.19.46.18"),
    load_timeout = 2e+05,
    options = list(
      shiny.testmode = TRUE
    ))
  
  print(paste0("About to click in ",test_script_name))
  
  app$set_inputs(Go_to_upload = "click")
  app$wait_for_idle(timeout = 2e+05)
  app$upload_file(imported = paste0(here(test_dataset_folder),test_dataset))
  
  print(paste0("Just uploaded in ",test_script_name))
  app$wait_for_idle(timeout = 1e+06)
  app$click(selector="#shiny-modal")
  app$expect_values(name="just-uploaded", input=TRUE, output=TRUE)

  app$set_inputs(sidebarmenuid = "tabClientCount")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "client-count", input=TRUE, output=TRUE)
  
  app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
  app$set_inputs(sidebarmenuid = "tabPDDE")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "pdde", input=TRUE, output=TRUE)
  
  app$set_inputs(sidebarmenuid = "tabDQSystem")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "dq-system", input=TRUE, output=TRUE)
  
  app$set_inputs(sidebarmenuid = "tabDQOrg")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "dq-org", input=TRUE, output=TRUE)

  app$set_inputs(sidebarmenuid = "tabSystemOverview", input=TRUE, output=TRUE)
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-flow-summary", input=TRUE, output=TRUE)
  
  app$set_inputs(sys_inflow_outflow_subtabs = "Detail Chart")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-flow-detail", input=TRUE, output=TRUE)
  
  # change universe filters
  app$set_inputs(syso_hh_type = "AO", syso_project_type = "Residential")
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(name = "sys-flow-detail-w-AO-Residential", input=TRUE, output=TRUE)
  
  # go back to summary tab
  app$set_inputs(sys_inflow_outflow_subtabs = "Summary Chart")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-flow-summary-w-AO-Residential", input=TRUE, output=TRUE)
  
  # go to information
  app$set_inputs(sys_inflow_outflow_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-flow-information", input=TRUE, output=TRUE)
  
  app$set_inputs(syso_tabbox = "Client System Status")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-status-chart", input=TRUE, output=TRUE)
  
  app$set_inputs(sys_status_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-status-information", input=TRUE, output=TRUE)

  app$set_inputs(syso_tabbox = "System Demographics")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-comp-chart-default", input=TRUE, output=TRUE)
  
  app$set_inputs(system_composition_selections = c("All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+05)
  app$expect_values(name = "sys-comp-all-re", input=TRUE, output=TRUE)
  
  app$set_inputs(system_composition_selections = c("Gender", "All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(name = "sys-comp-all-re-gender", input=TRUE, output=TRUE)
  
  app$set_inputs(sys_comp_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-comp-information", input=TRUE, output=TRUE)
  
  customDownload(app, "downloadFileStructureAnalysis", "File-Structure-Analysis-Download.xlsx")
  customDownload(app, "downloadClientCountsReport", "Client-Counts-Download.xlsx")
  customDownload(app, "downloadPDDEReport", "PDDE-Download.xlsx")
  customDownload(app, "downloadSystemDQReport", "System-DQ-Download.xlsx")
  customDownload(app, "downloadOrgDQReport", "Org-DQ-Download.xlsx")
  customDownload(app, "sys_inflow_outflow_download_btn", "System-Flow-Download.xlsx")
  customDownload(app, "sys_inflow_outflow_download_btn_ppt", "System-Flow-Download-PPT.pptx")
  customDownload(app, "sys_status_download_btn", "System-Status-Download.xlsx")
  customDownload(app, "sys_status_download_btn_ppt", "System-Status-Download-PPT.pptx")
  customDownload(app, "sys_comp_download_btn", "System-Composition-Download.xlsx")
  customDownload(app, "sys_comp_download_btn_ppt", "System-Composition-Download-PPT.pptx")
  
  # export non-large/helper datasets
  all_export_names <- names(app$get_values(export=TRUE)$export)
  exports_to_keep <- setdiff(all_export_names, helper_datasets)
  app$expect_values(export = exports_to_keep, name = "exportTestValues")
  
  # handle large/helper datasets
  lapply(helper_datasets, function(df_name) {
    handle_helper_data(app, test_script_name, df_name)
  })
})