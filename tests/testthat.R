library(shinytest2)
test_app(".")
unlink(here("tests/temp"), recursive = TRUE)
# testthat::snapshot_review()