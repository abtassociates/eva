# https://rstudio.github.io/shinytest/articles/in-depth.html
library(shinytest)

# create the various test datasets (will be saved in a temp directory)
source("tests/create_test_datasets.R", local = TRUE)

source("tests/testing_functions.R", local = TRUE)

# run the tests
expect_pass(testApp(".", compareImages = FALSE, quiet = FALSE))

# remove the temp directory
unlink("tests/temp", recursive = TRUE)