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
  app$expect_values(
    name = "client-count",
    input = c(
      "currentProviderList",
      "dateRangeCount",
      inputs_no_bindings(DTs = c("clientCountData", "clientCountSummary"))
    ),
    output = c(
      "headerClientCounts",
      "headerClientCounts_supp",
      "clientCountData",
      "clientCountSummary",
      "downloadClientCountsReportButton"
    )
  )
  
  app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
  app$set_inputs(sidebarmenuid = "tabPDDE")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "pdde",
    input = c(
      "sidebarCollapsed",
      "sidebarItemExpanded",
      "sidebarmenuid",
      inputs_no_bindings(DTs = c("pdde_guidance_summary", "pdde_summary_table"))
    ),
    output = c(
      "headerPDDE",
      "pdde_guidance_summary",
      "pdde_summary_table",
      "downloadPDDEReportButton"
    )
  )
  
  app$set_inputs(sidebarmenuid = "tabDQSystem")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "dq-system",
    input = c(
      "sidebarCollapsed",
      "sidebarItemExpanded",
      "sidebarmenuid"
    ),
    output = c(
      "headerSystemDQ",
      "systemDQErrorByIssue",
      "systemDQErrorsByIssue_ui",
      "systemDQHighPriorityErrorsByIssue",
      "systemDQHighPriorityErrorsByIssue_ui",
      "systemDQWarningByIssue",
      "systemDQWarningsByIssue_ui",
      "downloadSystemDQReportButton"
    )
  )
  
  app$set_inputs(sidebarmenuid = "tabDQOrg")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "dq-org",
    input = c(
      "sidebarCollapsed",
      "sidebarItemExpanded",
      "sidebarmenuid",
      "orgList",
      inputs_no_bindings(DTs = c("dq_org_guidance_summary", "dq_organization_summary_table"))
    ),
    output = c(
      "headerDataQuality",
      "dq_org_guidance_summary",
      "dq_organization_summary_table",
      "orgDQErrorByIssue",
      "orgDQErrorsByIssue_ui",
      "orgDQHighPriorityErrorsByIssue",
      "orgDQHighPriorityErrorsByIssue_ui",
      "orgDQWarningByIssue",
      "orgDQWarningsByIssue_ui",
      "downloadOrgDQReportButton"
    )
  )
  
  # System Flow
  sys_universe_filters <- c(
    "syso_age",
    "syso_spec_pops",
    "syso_gender",
    "syso_race_ethnicity"
  )
  
  sys_flow_filters <- c(
    "syso_hh_type",
    "syso_level_of_detail",
    "methodology_type",
    "syso_project_type"
  )
  
  sys_other_inputs <- c(
    "syso_tabbox"
  )
  
  sys_act_inputs <- c(
    "sidebarmenuid",
    "sys_inflow_outflow_subtabs",
    sys_universe_filters,
    sys_flow_filters,
    sys_other_inputs
  )
  sys_act_summary_outputs <- c(
    "headerSystemOverview",
    "sys_act_summary_filter_selections",
    "sys_act_summary_ui_chart"
  )
  
  app$set_inputs(sidebarmenuid = "tabSystemOverview")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-flow-summary",
    input = sys_act_inputs,
    output = sys_act_summary_outputs
  )
  
  sys_act_detail_outputs <- c(
    "headerSystemOverview",
    "sys_act_detail_filter_selections",
    "sys_act_detail_ui_chart"
  )
  app$set_inputs(sys_inflow_outflow_subtabs = "Detail Chart")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-flow-detail",
    input = sys_act_inputs,
    output = sys_act_detail_outputs
  )
  
  # change universe filters
  app$set_inputs(syso_hh_type = "AO", syso_project_type = "Residential")
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(
    name = "sys-flow-detail-w-AO-Residential",
    input = sys_act_inputs,
    output = sys_act_detail_outputs
  )
  
  # go back to summary tab
  app$set_inputs(sys_inflow_outflow_subtabs = "Summary Chart")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-flow-summary-w-AO-Residential",
    input = sys_act_inputs,
    output = sys_act_summary_outputs
  )
  
  # go to information
  app$set_inputs(sys_inflow_outflow_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-flow-information",
    input = c(
      "sidebarmenuid",
      "sys_inflow_outflow_subtabs"
    )
  )
  
  # System Status/Sankey
  sys_status_inputs <- c(
    "sidebarmenuid",
    "sys_status_subtabs",
    sys_universe_filters,
    sys_flow_filters,
    sys_other_inputs
  )
  sys_status_outputs <- c(
    "headerSystemOverview",
    "sankey_filter_selections",
    "sankey_ui_chart"
  )
  app$set_inputs(syso_tabbox = "Client System Status")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-status-chart",
    input = sys_status_inputs,
    output = sys_status_outputs
  )
  
  app$set_inputs(sys_status_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-status-information",
    input = sys_status_inputs,
    output = sys_status_outputs
  )
  
  # System Composition/Demographics
  sys_comp_inputs <- c(
    "sidebarmenuid",
    "sys_comp_subtabs",
    sys_universe_filters,
    # even though sys_flow_filters are hidden for System Demographics, 
    # include to make sure they aren't changing
    sys_flow_filters, 
    sys_other_inputs,
    "system_composition_selections"
  )
  
  sys_comp_outputs <- c(
    "headerSystemOverview",
    "sys_comp_summary_selections",
    "sys_comp_summary_ui_chart"
  )
  
  app$set_inputs(syso_tabbox = "System Demographics")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-comp-chart-default",
    input = sys_comp_inputs,
    output = sys_comp_outputs
  )
  
  app$set_inputs(system_composition_selections = c("All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+05)
  app$expect_values(
    name = "sys-comp-all-re",
    input = sys_comp_inputs,
    output = sys_comp_outputs
  )
  
  app$set_inputs(system_composition_selections = c("Gender", "All Races/Ethnicities"))
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(
    name = "sys-comp-all-re-gender",
    input = sys_comp_inputs,
    output = sys_comp_outputs
  )
  
  app$set_inputs(sys_comp_subtabs = "Information")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "sys-comp-information",
    input = sys_comp_inputs,
    output = sys_comp_outputs
  )
  
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