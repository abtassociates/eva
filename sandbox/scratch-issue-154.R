library(tidyverse)
library(lubridate)
library(janitor)

shiny_log <- 
  read_log("metadata-analysis/metadata/01-25-2023_pm/eva-shiny-20230125-003304-43997.log")

metadata <- 
  read_csv("metadata-analysis/metadata/01-25-2023_pm/metadata.csv")

session_data <- 
  read_csv("metadata-analysis/metadata/01-25-2023_pm/sessiondata.csv")
