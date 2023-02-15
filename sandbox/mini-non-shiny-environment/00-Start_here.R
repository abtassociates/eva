# start here
library(tidyverse)
library(lubridate)
library(janitor)
library(HMIS)

# Instructions ------------------------------------------------------------

# Run this, it will create a data directory here in case you don't already have one

if(dir.exists("sandbox/mini-non-shiny-environment/data/")) {
  cat("All good")
} else {
  dir.create("sandbox/mini-non-shiny-environment/data/")
}

# Copy a sample dataset into the mini-non-shiny-environment/data directory

# Run the scratch files in order. As you're running them, it's helpful to have
# the Environment tab open so you can see what is going on

# Functions
source("sandbox/mini-non-shiny-environment/00-functions-scratch.R")

# Get Export

source("sandbox/mini-non-shiny-environment/01-get_export-scratch.R")
