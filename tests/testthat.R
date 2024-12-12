library(shinytest2)
test_app(".") # example filter: filter="main-valid"
# testthat::set_max_fails(100) --> in case you expect all tests to fail, you can make sure it gets through all of them
# testthat::snapshot_review()
# testthat::snapshot_review(files="main-valid/test-main-valid-exportTestValues.json")
# testthat::snapshot_accept(files="fsa/test-fsa-001.json")
