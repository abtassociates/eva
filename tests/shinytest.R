library(shinytest)
library(readxl)
testApp(".", compareImages=FALSE, quiet=TRUE)
viewTestDiff(".", interactive = FALSE)

