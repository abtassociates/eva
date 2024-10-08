library(shinytest2)
library(here)
test_script_name <- "test-main-valid"
test_dataset_folder <- "tests/temp/"
test_dataset <- "FY24-ICF-main-valid.zip"

# test_that(paste0("{shinytest2} recording: ",test_script_name), {
#   print(paste0("Running ",test_script_name))
  
  app <- AppDriver$new(
    variant = platform_variant(os_name = FALSE), 
    name = test_script_name, 
    seed = 12345,
    width = 1920,
    height = 1080,
    load_timeout = 2e+05,
    options = list(
      shiny.testmode = TRUE
    ))
  
  print(paste0("About to click in ",test_script_name))
  
  # app$upload_file(imported = here("tests/temp/FY24-ICF-main-valid.zip"))
  app$upload_file(imported = here("demo.zip"))

  app$wait_for_idle(timeout = 1e+06)
  # app$set_inputs(sidebarmenuid = "tabSystemOverview")
  app$wait_for_idle(timeout = 1e+06)
  app$set_inputs(syso_hh_type = "AO", syso_project_type = "Residential")
  app$wait_for_idle(timeout = 1e+06)
  app$expect_values(name = "sys-flow-summary")
# })