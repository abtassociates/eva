# Load application support files into testing environment
library(shinytest2)
source("testing_functions.R", local = TRUE)
shinytest2::load_app_env()
