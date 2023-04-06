library(shinytest)
library(readxl)
expect_pass(testApp(".", compareImages=FALSE, quiet=TRUE))
viewTestDiff(".", interactive = FALSE)

