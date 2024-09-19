library(here)
library(shinytest2)
unlink(here("tests/temp"), recursive = TRUE)
# create reduced-sized versions of initially invalid test datasets
source(here("tests/create_test_datasets.R"), local = TRUE)
test_app(".")
# testthat::snapshot_review()