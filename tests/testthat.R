library(here)
library(shinytest2)

# create reduced-sized versions of initially invalid test datasets
unlink(here("tests/temp"), recursive = TRUE)
source(here("tests/create_test_datasets.R"), local = TRUE)

test_app(".")
# testthat::snapshot_review()