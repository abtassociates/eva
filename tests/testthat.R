library(shinytest2)

# testthat::snapshot_review()

test_app(".")
unlink(here("tests/temp"), recursive = TRUE)
