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

dataset_directory <- c("Hennepin-Redacted/")

source("00_dates.R")

if (calc_data_goes_back_to != meta_export_start){
  cat("Export Start Date may not be correct.")
} else {}
  
# if there's not already an images directory, create it
if (!dir.exists("images")) dir.create("images")

# Start running scripts ---------------------------------------------------

cat("Importing raw HMIS data\n")
source("00_get_Export_and_ART.R")

cat("working on Cohorts\n")
source("00_cohorts.R") 

cat("working on Bed_Unit_Utilization\n")
source("01_Bed_Unit_Utilization.R")  

cat("working on QPR_SPDATs")
source("02_QPR_SPDATs.R") 

cat("working on QPR_EEs")
source("02_QPR_EEs.R")

cat("working on Veterans data")
source("03_Veterans.R")  

cat("working on Data Quality")
source("04_DataQuality.R")

cat("working on Veterans Active List")
source("05_Veterans_Active_List.R")

cat("working on SPMs")
source("07_SPMs.R")

cat("working on Active List")
source("08_Active_List.R")

cat("copying images to app directories")
source("00_copy_images.R")

cat("Done! All images are updated.")

