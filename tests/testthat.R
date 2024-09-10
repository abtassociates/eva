library(shinytest2)
unlink(here("tests/temp"), recursive = TRUE)
test_app(".", filter="fsa")
# unlink(here("tests/temp"), recursive = TRUE)
