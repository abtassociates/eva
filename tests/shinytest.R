library(shinytest)
library(readxl)
print(utils::osVersion)
testApp(".", quiet=TRUE)
viewTestDiff(".", interactive = FALSE)

