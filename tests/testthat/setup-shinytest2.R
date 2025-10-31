library(here)

create_datasets <- if(exists("create_datasets")) create_datasets else Sys.info()["user"] == "runner"

if (create_datasets || Sys.info()["user"] == "runner") {
  # create reduced-sized versions of initially invalid test datasets
  unlink(here("tests/temp"), recursive = TRUE)
  source(here("tests/create_test_datasets.R"), local = TRUE)
}

source(here("tests/testing_functions.R"))

shinytest2::load_app_env()
