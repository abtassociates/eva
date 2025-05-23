library(shinytest2)
testthat::set_max_fails(100)
test_app(".", filter="main-valid") # example filter: filter="main-valid"
# testthat::snapshot_review()
# testthat::snapshot_review(files="main-valid/test-main-valid-exportTestValues.json")
# testthat::snapshot_accept(files="fsa/test-fsa-001.json")
daemons(0)