# https://rstudio.github.io/shinytest/articles/in-depth.html
library(shinytest)

# create the various test datasets (will be saved in a temp directory)
source("tests/create_test_datasets.R", local = TRUE)

# run the tests
testApp(".", compareImages = FALSE, quiet = FALSE)

# remove the temp directory
unlink("tests/temp", recursive = TRUE)