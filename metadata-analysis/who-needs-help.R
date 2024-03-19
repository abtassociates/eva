library(tidyverse)
library(here)
x <- read_csv(here("metadata-analysis/metadata/metadata.csv"))
y <- read_csv(here("metadata-analysis/metadata/sessiondata.csv"))

z <- x %>%
  filter(Details == "Successful upload") %>%
  right_join(y, by = "SessionToken") %>%
  count(SoftwareName, Details) %>%
  mutate(Details = if_else(is.na(Details), "Unsuccessful", "Successful")) %>%
  pivot_wider(names_from = Details, values_from = n, values_fill = 0) %>%
  mutate(SuccessRate = scales::percent(Successful/(Unsuccessful + Successful)))
