# Instructions ------------------------------------------------------------

# Run this, it will create a data directory here in case you don't already have one

if(dir.exists("sandbox/mini-non-shiny-environment/data/")) {
  "All good"
} else {
  dir.create("sandbox/mini-non-shiny-environment/data/")
}

directory <- "sandbox/mini-non-shiny-environment/"

# Copy a sample dataset into the mini-non-shiny-environment/data directory

ifelse(file.exists(paste0(directory, "data/Enrollment.csv")),
        "All good",
        "Please unzip a sample data set into your mini-non-shiny-environment/data
        directory before proceeding.")

# Run the scratch files in order. As you're running them, it's helpful to have
# the Environment tab open so you can see what is going on

# Functions ---------------------------------------------------------------
library(here)

source(here("global.R"))

source(paste0(directory, "helper_functions.R"))

# Hard codes --------------------------------------------------------------

source(paste0(directory, "hardcodes.R"))
  
# Get Export --------------------------------------------------------------

source(paste0(directory, "01_get_Export.R"))

# Export Dates ------------------------------------------------------------

source(paste0(directory, "02_export_dates.R"))
  
# File Structure Analysis -------------------------------------------------

source(paste0(directory, "03_file_structure_analysis.R"))

if_else(structural_issues == 0,
        "Eva would not reject this export.",
        "Eva would reject this export.")

# Data prep! --------------------------------------------------------------

source(paste0(directory, "04_initial_data_prep.R"))
