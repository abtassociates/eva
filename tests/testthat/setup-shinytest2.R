library(here)

if (create_datasets || Sys.info()["user"] == "runner") {
  # create reduced-sized versions of initially invalid test datasets
  unlink(here("tests/temp"), recursive = TRUE)
  source(here("tests/create_test_datasets.R"), local = TRUE)
}

source(here("tests/testing_functions.R"))
in_dev_mode <<- FALSE
shinytest2::load_app_env()
