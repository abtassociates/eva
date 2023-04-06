library(shinytest)
library(readxl)
testApp(".", compareImages = grepl("^Ubuntu", utils::osVersion))

