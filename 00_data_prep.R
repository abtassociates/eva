# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

# ************
# Run this whenever the data has been refreshed (usually every weekday morning)

# Each script here creates an image file which is copied to both R minor and R 
# minor elevated. Running this after updating the data files should be all 
# that's necessary in order to be sure the apps are getting the most recent data
# and code. This script checks that you've downloaded all the correct files in 
# the correct way, runs them all, then copies the images to Rm/Rme.

library(tidyverse)
library(here)

# the Hennepin-redacted dataset cries bc the DOBs are all nulled out, so using San Diego

dataset_directory <- "San-Diego3/" 
directory <- paste0("data/", dataset_directory)

source("00_dates.R")

if (calc_data_goes_back_to != meta_HUDCSV_Export_Start){
  cat("Export Start Date may not be correct.")
} else {}
  
# if there's not already an images directory, create it
if (!dir.exists("images")) dir.create("images")

# Start running scripts ---------------------------------------------------

cat("Importing raw HMIS data\n")
source("00_get_Export.R")

cat("working on Cohorts\n")
source("01_cohorts.R") 

cat("working on Bed_Unit_Utilization\n")
source("02_Bed_Unit_Utilization.R")  

cat("working on Data Quality")
source("03_DataQuality.R")

cat("Saving what we need out to a .Rdata file, almost done!")

save( # from functions, cohorts, dates, get_export, data_quality, bed_unit_utilization
  living_situation, # functions
  enhanced_yes_no_translator, # cohorts
  validation, # cohorts
  hc_check_dq_back_to, # dates
  meta_HUDCSV_Export_Date, # dates
  meta_HUDCSV_Export_End, # dates
  meta_HUDCSV_Export_Start, # dates
  Client, # get_export
  HUD_specs, # get_export
  Organization, # get_export
  Enrollment, # get_export
  Exit, # get_export
  Export, # get_export
  Assessment, # get_export
  Event, # get_export
  Project, # get_export
  Inventory, # get_export
  dq_main, # data_quality
  dq_past_year, # data_quality
  detail_eligibility, # data_quality
  dq_overlaps, # data_quality
  dq_providers, # data_quality
  dq_plot_eligibility, # data_quality
  dq_plot_errors, # data_quality
  dq_plot_hh_errors, # data_quality
  dq_plot_projects_errors, # data_quality
  dq_plot_projects_warnings, # data_quality
  dq_plot_warnings, # data_quality
  utilizers_clients, # bed_unit_utilization
  Beds, # bed_unit_utilization
  utilization_bed, # bed_unit_utilization
  utilization_unit, # bed_unit_utilization
  note_bed_utilization, # bed_unit_utilization
  note_calculation_utilization, # bed_unit_utilization
  note_unit_utilization, # bed_unit_utilization
  utilization, # bed_unit_utilization
  compress = FALSE,
  file = here("images/DQ_Shiny.RData")
)

cat("All necessary dfs have been saved to images/DQ_Shiny.RData.")

