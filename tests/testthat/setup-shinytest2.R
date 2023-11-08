# Load application support files into testing environment
shinytest2::load_app_env()
source("tests/testing_functions.R", local = TRUE)
# create the various test datasets (will be saved in a temp directory)
source("tests/create_test_datasets.R", local = TRUE)
