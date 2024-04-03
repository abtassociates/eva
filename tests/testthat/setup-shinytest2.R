library(here)
source(here("tests/testing_functions.R"), local = TRUE)
# create reduced-sized versions of initially invalid test datasets
source(here("tests/create_test_datasets.R"), local = TRUE)
shinytest2::load_app_env()
