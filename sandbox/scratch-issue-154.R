library(tidyverse)
library(lubridate)
library(janitor)

shiny_log <- 
  read_log("www/metadata/01-25-2023_pm/eva-shiny-20230125-003304-43997.log")

metadata <- 
  read_csv("www/metadata/01-25-2023_pm/metadata.csv")

session_data <- 
  read_csv("www/metadata/01-25-2023_pm/sessiondata.csv")
