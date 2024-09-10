library(shinytest2)
unlink(here("tests/temp"), recursive = TRUE)
test_app(".")
# unlink(here("tests/temp"), recursive = TRUE)
