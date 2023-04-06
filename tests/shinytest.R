# https://rstudio.github.io/shinytest/articles/in-depth.html
library(shinytest)
library(readxl)
testApp(".", compareImages=FALSE, quiet=TRUE)
viewTestDiff(".", interactive = FALSE)

