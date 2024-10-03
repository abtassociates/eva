library(shinytest2)
test_app(".", filter="wrong-file-type") # example filter: filter="main-valid"
# testthat::snapshot_review()