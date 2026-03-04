system_exits_tests <- function(app, test_script_name = "system-exits", test_dataset = "tests/temp/FY26-test-main-valid.zip"){
  # System Exits by Type
  syse_universe_filters <- c(
    "syse_age",
    "syse_spec_pops",
    "syse_race_ethnicity"
  )
  
  syse_project_filters <- c(
    "syse_hh_type",
    "syse_level_of_detail",
    "syse_methodology_type",
    "syse_project_type"
  )
  
  syse_other_inputs <- c(
    "syse_tabbox"
  )
  
  syse_types_inputs <- c(
    "pageid",
    "syse_types_subtabs",
    syse_universe_filters,
    syse_project_filters,
    syse_other_inputs
  )
  syse_types_outputs <- c(
    "headerSystemExit",
    "syse_types_filter_selections",
    "syse_types_ui_chart"
  )
  
  app$set_inputs(pageid = "tabSystemExits")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "syse-types-chart-default",
    input = syse_types_inputs,
    output = syse_types_outputs
  )
  
  # change universe filters
  app$set_inputs(syse_hh_type = "AO", syse_project_type = "LHRes")
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(
    name = "syse-types-w-AO-Residential",
    input = syse_types_inputs,
    output = syse_types_outputs
  )
  
  # change universe filters
  app$set_inputs(syse_project_type = "PHRes")
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(
    name = "syse-types-w-AO-Residential-PH",
    input = syse_types_inputs,
    output = syse_types_outputs
  )
  
  # change client filter to Hispanic/Latino. This should lead to < 11 people to check validation/redacting
  # AS TODO: Add Demographic output?
  app$set_inputs(syse_race_ethnicity = "LatinoAloneMethod1Detailed")
  app$wait_for_idle(timeout = 2e+06)
  app$expect_values(
    name = "syse-types-w-AO-Residential-PH-hisp",
    input = syse_types_inputs,
    output = syse_types_outputs
  )
  
  ## reset enrollment filters
  app$set_inputs(syse_hh_type = "All", syse_project_type = "LHRes", syse_race_ethnicity = "All")
  app$wait_for_idle(timeout = 2e+06)
  
  ## System Exits by Year
  syse_time_inputs <- c(
    "pageid",
    "syse_time_subtabs",
    syse_universe_filters,
    syse_project_filters,
    syse_other_inputs
  )
  syse_time_outputs <- c(
    "headerSystemExit",
    "syse_compare_time_filter_selections",
    "syse_compare_time_ui_chart"
  )
  app$set_inputs(syse_tabbox = '<h4>Exits by Year</h4>')
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "syse-time-chart-default",
    input = syse_time_inputs,
    output = syse_time_outputs
  )
  
  ## System Exits by Subpopulation
  syse_subpop_inputs <- c(
    "pageid",
    "syse_subpop_subtabs",
    syse_universe_filters,
    syse_project_filters,
    syse_other_inputs
  )
  syse_subpop_outputs <- c(
    "headerSystemExit",
    "syse_compare_subpop_filter_selections",
    "syse_compare_subpop_ui_chart"
  )
  
  app$set_inputs(syse_tabbox = '<h4>Exits by Subpopulation</h4>')
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "syse-subpop-chart-default",
    input = syse_subpop_inputs,
    output = syse_subpop_outputs
  )
  app$set_inputs(syse_spec_pops = "Veteran")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "syse-subpop-veteran",
    input = syse_subpop_inputs,
    output = syse_subpop_outputs
  )
  ## System Exit PH Demographics
  syse_phd_inputs <- c(
    "pageid",
    "syse_phd_subtabs",
    # even though syse_universe_filters are hidden for System Demographics, 
    # include to make sure they aren't changing
    syse_universe_filters,
    syse_project_filters,
    syse_other_inputs
  )
  syse_phd_outputs <- c(
    "headerSystemExit",
    "syse_phd_filter_selections",
    "syse_phd_ui_chart"
  )
  app$set_inputs(syse_tabbox = '<h4>Permanent Housing Demographics</h4>')
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "syse-phd-chart-default",
    input = syse_phd_inputs,
    output = syse_phd_outputs
  )
  app$set_inputs(syse_phd_selections = "Age")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(
    name = "syse-phd-age",
    input = syse_phd_inputs,
    output = syse_phd_outputs
  )
  return(app)
  
}
library(here)
library(glue)
system_exits_test_script <- function(test_script_name = "system-exits", test_dataset = "tests/temp/FY26-test-main-valid.zip"){
  non_download_exports <- c()
  
  
  test_that(paste0("{shinytest2} recording: ",test_script_name), {
    print(paste0("Running ",test_script_name))
    testthat::local_edition(3)
    is_gha <- Sys.info()["user"] == "runner"
    
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
    app$click(selector="#shiny-modal .btn-default")
    app$expect_values(name="just-uploaded", input=TRUE, output=TRUE)
    
    app <- system_exits_tests(app, test_script_name = test_script_name,
                              test_dataset = test_dataset) 
    
    customDownload(app, "syse_types_download_btn", "System-Exit-Types-Download.xlsx")
    customDownload(app, "syse_types_download_btn_ppt", "System-Exit-Types-Download-PPT.pptx")
    customDownload(app, "syse_compare_download_btn", "System-Exit-Comparison-Download.xlsx")
    customDownload(app, "syse_compare_download_btn_ppt", "System-Exit-Comparison-Download-PPT.pptx")
    customDownload(app, "syse_phd_download_btn", "System-Exit-Demographics-Download.xlsx")
    customDownload(app, "syse_phd_download_btn_ppt", "System-Exit-Demographics-Download-PPT.pptx")
    
    if(!is_gha) {
      customDownload(app, "syse_client_level_download_btn", "System-Exits-Client-Level-Download.xlsx")
    }
    #browser()
    # export non-large/helper datasets
    # all_export_names <- names(app$get_values(export=TRUE)$export)
    # exports_to_keep <- setdiff(all_export_names, c(helper_datasets, non_download_exports))
    # app$expect_values(export = exports_to_keep, name = "exportTestValues")
    # 
    # print("handling helper datasets")
    # # handle large/helper datasets
    # lapply(helper_datasets, function(df_name) {
    #   print(paste0("handling ", df_name))
    #   handle_helper_data(app, test_script_name = test_script_name, df_name)
    # })
    
    # print("saving shiny log")
    # view(app$get_logs())
  })
}

system_exits_test_script(test_script_name = "system-exits", test_dataset = "tests/temp/FY26-test-main-valid.zip")
