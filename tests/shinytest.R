# https://rstudio.github.io/shinytest/articles/in-depth.html
library(shinytest)
library(readxl)
expectPass(testApp(".", compareImages = FALSE))
