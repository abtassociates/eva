library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)
library(scales)

updatedate <- file.info("data/Client.csv")$mtime

load("data/Utilization.RData")

load("data/QPR_SPDATs.RData")

ProviderAverages <- SPDATsByProject %>%
  filter(served_between(SPDATsByProject, ReportStart, ReportEnd)) %>%
  select(EnrollmentID, ProjectName, ScoreAdjusted) %>%
  group_by(ProjectName) %>%
  summarise(AverageScore = round(mean(ScoreAdjusted), 1),
            EnrollmentCount = n())

