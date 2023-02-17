# start here
library(tidyverse)
library(lubridate)
library(janitor)
library(HMIS)

# Instructions ------------------------------------------------------------

# Run this, it will create a data directory here in case you don't already have one

if(dir.exists("sandbox/mini-non-shiny-environment/data/")) {
  "All good"
} else {
  dir.create("sandbox/mini-non-shiny-environment/data/")
}

directory <- "sandbox/mini-non-shiny-environment/"

# Copy a sample dataset into the mini-non-shiny-environment/data directory

if_else(file.exists(paste0(directory, "data/Enrollment.csv")),
        "All good",
        "Please unzip a sample data set into your mini-non-shiny-environment/data
        directory before proceeding.")

# Run the scratch files in order. As you're running them, it's helpful to have
# the Environment tab open so you can see what is going on


# Functions ---------------------------------------------------------------

source(paste0(directory, "01-functions-scratch.R"))

# Get Export --------------------------------------------------------------

source(paste0(directory, "02-get_export-scratch.R"))

# File Structure Analysis -------------------------------------------------

source(paste0(directory, "03-integrity_checker-scratch.R"))

if_else(structural_issues == 0,
        "Eva would not reject this export.",
        "Eva would reject this export.")

# Dates -------------------------------------------------------------------

source(paste0(directory, "05-dates-scratch.R"))

# Data prep! --------------------------------------------------------------

source(paste0(directory, "04-initial_data_prep-scratch.R"))

# Cohorts -----------------------------------------------------------------

source(paste0(directory, "06-cohorts-scratch.R"))


