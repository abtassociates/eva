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

cat("Importing raw HMIS data\n")
source("00_get_Export_and_ART.R")

cat("working on Cohorts")
Cohorts <- rlang::env(COHHIO_HMIS) # creating child environment
# rlang::env_binding_lock(COHHIO_HMIS, ls(COHHIO_HMIS)) # locking COHHIO_HMIS
source("00_cohorts.R", local = Cohorts) # populating Cohorts env
# rlang::env_binding_lock(Cohorts, ls(Cohorts)) # locking Cohorts

cat("working on Bed_Unit_Utilization")
source("01_Bed_Unit_Utilization.R", local = rlang::env(Cohorts)) # running inside a 
# child environment OF Cohorts

increment("working on QPR_SPDATs")
source("02_QPR_SPDATs.R", local = rlang::env(COHHIO_HMIS)) # doesn't need Cohorts here

increment("working on QPR_EEs")
source("02_QPR_EEs.R", local = rlang::env(Cohorts)) # these envs don't get saved in memory

increment("working on Veterans data")
source("03_Veterans.R", local = rlang::env(Cohorts)) # 

increment("working on Data Quality")
DataQuality <- rlang::env(Cohorts) # creating a child env inside Cohorts env
source("04_DataQuality.R", local = DataQuality)
# rlang::env_binding_lock(DataQuality, ls(DataQuality))

increment("working on Veterans Active List")
source("05_Veterans_Active_List.R", local = rlang::env(Cohorts))

increment("working on SPMs")
source("07_SPMs.R", local = new.env())

increment("working on Active List")
source("08_Active_List.R", local = rlang::env(Cohorts))

increment("getting covid vaccine data together")
source("09_covid.R", local = new.env())

dir <- "pe_dataset_final"
# files <- freeze_pe(dir) # run on freeze day ONLY
pe <- rlang::new_environment(list(dir = dir), parent = .BaseNamespaceEnv)

load("pe_dataset_final/images/COHHIOHMIS.RData", envir = pe)
load("pe_dataset_final/images/Data_Quality.RData", envir = pe)
load("pe_dataset_final/images/cohorts.RData", envir = pe)

increment("working on Project Evaluation")
source("06_Project_Evaluation.R", local = pe)

increment("copying images to app directories")
rm(Cohorts, COHHIO_HMIS)
source("00_copy_images.R", local = new.env())

increment("Done! All images are updated.")

