# https://rstudio.github.io/shinytest/articles/in-depth.html
library(shinytest)
library(readxl)
expect_pass(testApp(".", compareImages = FALSE))