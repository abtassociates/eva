library(shinytest2)
test_app()

testthat::snapshot_review()

unlink(here("tests/temp"), recursive = TRUE)
