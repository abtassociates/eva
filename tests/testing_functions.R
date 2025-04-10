library(glue)
customDownload <- function(app, downloadHandler, fname) {
  print(paste("downloading", downloadHandler))
  app$get_download(downloadHandler, fname)
  file.remove(fname)
}

initially_invalid_test_script <- function(test_script_name, test_dataset) {
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))
    
    app <- AppDriver$new(
      variant = platform_variant(), 
      name = test_script_name, 
      seed = 12345,
      screenshot_args = FALSE,
      expect_values_screenshot_args = FALSE,
      load_timeout = 5e+05)
    
    app$set_window_size(width = 1920, height = 1080)
    app$set_inputs(Go_to_upload = "click")
    app$wait_for_idle(timeout = 2e+05)
    app$upload_file(imported = here(test_dataset))
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

handle_helper_data <- function(app, test_script_name, datasetname) {
  helper_data_dir <- here(glue("tests/helper_data/{gsub('test-','',test_script_name)}"))
  print(paste0("helper data folder = ", helper_data_dir))
  if(!dir.exists(helper_data_dir)) {
    print("creating helper data folder")
    dir.create(helper_data_dir)
  }
  
  old_path <- glue("{helper_data_dir}/{datasetname}.csv")
  new_path <- gsub(".csv", ".new.csv", old_path)
  
  new_df <- app$get_value(export=datasetname)
  
  if(is.null(new_df)) return(NULL)
  
  new_df <- as.data.table(new_df)
  fwrite(new_df, file = new_path)
  
  # if no csv yet, create one at the original filepath
  if(!file.exists(old_path)) {
    message(glue("A csv for {datasetname} does not yet exist. Creating one now!"))
    fwrite(new_df, file = old_path)
    file.remove(new_path)
  } 
  else {
    # otherwise, if new and old are different warn user
    if(!identical(fread(old_path), fread(new_path))) {
      warning(
        paste0(
          "Difference detected in ",
          datasetname,
          ". Use snapshot_review to visualize diff."
        )
      )
      fwrite(new_df, file = new_path)
    } 
    # otherwise, remove the new one
    else {
      file.remove(new_path)
    }
  }
}

inputs_no_bindings <- function(DTs=NULL, plotlys=NULL, htmlWidgets=NULL) {
  DT_suffixes <- c(
    "_cell_clicked",
    "_cells_selected",
    "_columns_selected",
    "_rows_current",
    "_rows_all",
    "_rows_selected",
    "_search",
    "_search_columns",
    "_state"
  )
  
  plotly_suffixes <- NULL
  htmlWidget_suffixes <- NULL
  
  combine_with_suffixes <- function(strings, suffixes) {
    if (is.null(strings) || is.null(suffixes)) return(character(0)) 
    c(outer(strings, suffixes, paste0))
  }
  
  c(
    combine_with_suffixes(DTs, DT_suffixes),
    combine_with_suffixes(plotlys, plotly_suffixes),
    combine_with_suffixes(htmlWidgets, htmlWidget_suffixes)
  )
}

main_test_script <- function(test_script_name, test_dataset) {
  non_download_exports <- c()
  
  helper_datasets <- c(
    "non_ascii_files_detail", 
    "client_count_download_detail", 
    "dq_main_reactive",
    "pdde_main",
    "universe_ppl_flags",
    "sys_comp_df",
    "dq_overlaps"
  )
  if(Sys.info()["sysname"] != "ubuntu")
    helper_datasets <- c(helper_datasets, "client_level_export_details")
  
  is_gha <- Sys.info()["user"] == "runner"
  if(!is_gha)
    helper_datasets <- c(helper_datasets, "client_level_export_details")
  
  
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))
    testthat::local_edition(3)
    
    app <- AppDriver$new(
      variant = platform_variant(),
      name = test_script_name, 
      screenshot_args = FALSE,
      expect_values_screenshot_args = FALSE,
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
    app$upload_file(imported = here(test_dataset))
    
    print(paste0("Just uploaded in ", test_script_name))
    app$wait_for_idle(timeout = 1e+06)
    app$click(selector="#shiny-modal")
    app$expect_values(name="just-uploaded", input=TRUE, output=TRUE)
    
    app$wait_for_idle(timeout = 1e+06)
    customDownload(app, "downloadImpermissibleCharacterDetail", "Impermissible-Character-Detail.xlsx")
    
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
    
    non_download_dq_org_exports <- c(
      "dq_org_guidance_summary",
      "dq_organization_summary_table"
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
      ),
      export = non_download_dq_org_exports
    )
    non_download_exports <- c(
      non_download_exports,
      non_download_dq_org_exports
    )
    
    # System Flow
    sys_universe_filters <- c(
      "syso_age",
      "syso_spec_pops",
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
    
    app$set_inputs(system_composition_selections = c("VeteranStatus", "All Races/Ethnicities"))
    app$wait_for_idle(timeout = 2e+06)
    app$expect_values(
      name = "sys-comp-all-re-veteran",
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
    if(!is_gha) {
      customDownload(app, "client_level_download_btn", "Client-Level-Download.xlsx")
    }
    
    # export non-large/helper datasets
    all_export_names <- names(app$get_values(export=TRUE)$export)
    exports_to_keep <- setdiff(all_export_names, c(helper_datasets, non_download_exports))
    app$expect_values(export = exports_to_keep, name = "exportTestValues")
    
    print("handling helper datasets")
    # handle large/helper datasets
    lapply(helper_datasets, function(df_name) {
      print(paste0("handling ", df_name))
      handle_helper_data(app, test_script_name, df_name)
    })
  })
}


# This is equivalent to snapshot_review(), but for the helper csv files

review_helper <- function(datasetname, test_script_name, comparison_type = 1) {
  helper_data_dir <- glue(
    here("tests/helper_data/{gsub('test-','',test_script_name)}")
  )
  
  old_path <- glue("{helper_data_dir}/{datasetname}.csv")
  new_path <- glue("{helper_data_dir}/{datasetname}.new.csv")
  old <- fread(old_path)
  new <- fread(new_path)
  
  
  if(comparison_type == 1) {
    # For simple differences, view more visually with one of these methods
    diffviewer::visual_diff(old_path, new_path)
  } else if(comparison_type == 2) {
    # Full records if at all different
    records_in_one_or_another(old, new)
  } else if(comparison_type == 3) {
    # summary of differences
    waldo::compare(old, new) 
  } else if(comparison_type == 4) {
    # Summary of differences
    diffobj::diffObj(old, new)
  }
}

records_in_one_or_another <- function(old, new) {
  # Isolate the records that are different
  # (Requires identical columns - if not, use waldo)
  only_in_old <- fsetdiff(old, new)
  if(nrow(only_in_old) > 0) {
    print("Viewing records only in the old dataset")
    view(only_in_old)
  }

  only_in_new <- fsetdiff(new, old)
  if(nrow(only_in_new) > 0) {
    print("Viewing records only in the new dataset")
    view(only_in_new)
  }
}
# This is equivalent to snapshot_accept(), but for the helper csv files

accept_helper <- function(datasetname, test_script_name) {
  helper_data_dir <- glue(
    here("tests/helper_data/{gsub('test-','',test_script_name)}")
  )
  
  old_path <- glue("{helper_data_dir}/{datasetname}.csv")
  new_path <- glue("{helper_data_dir}/{datasetname}.new.csv")
  
  # delete old
  file.remove(old_path)
  
  # rename new to old
  file.rename(new_path, old_path)
}
