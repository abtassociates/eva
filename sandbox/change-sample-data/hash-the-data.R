# Instructions ------------------------------------------------------------

# Run this, it will create a data directory here in case you don't already have one

if(dir.exists("sandbox/mini-non-shiny-environment/data/")) {
  "All good"
} else {
  dir.create("sandbox/mini-non-shiny-environment/data/")
}

directory <- "sandbox/mini-non-shiny-environment/"

library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(readxl)
library(writexl)
library(glue)
library(janitor)
library(here)

# Copy a sample dataset into the mini-non-shiny-environment/data directory

ifelse(file.exists(paste0(directory, "data/Enrollment.csv")),
       "All good",
       "Please unzip a sample data set into your mini-non-shiny-environment/data
        directory before proceeding.")

# Run the scratch files in order. As you're running them, it's helpful to have
# the Environment tab open so you can see what is going on

# Functions ---------------------------------------------------------------
library(here)
library(digest)

source(here("global.R"))

source(paste0(directory, "helper_functions.R"))

# Hard codes --------------------------------------------------------------

source(paste0(directory, "hardcodes.R"))

# Get Export --------------------------------------------------------------
col_types <- cols_and_data_types %>%
  mutate(DataType = data_type_mapping[as.character(DataType)])

Export <- read_csv(here(paste0(directory, "data/Export.csv")),
                   col_types = paste(col_types %>%
                     filter(File == "Export") %>%
                     pull(DataType), collapse = ""))
Client <- read_csv(here(paste0(directory, "data/Client.csv")),
                   col_types = paste(col_types %>%
                                       filter(File == "Client") %>%
                                       pull(DataType), collapse = ""))

# Edits -------------------------------------------------------------------

Export$HashStatus <- 4

Client_hashed <- Client %>%
  mutate(FirstName = digest(FirstName, algo = "md5"),
         MiddleName = digest(MiddleName, algo = "md5"),
         LastName = digest(LastName, algo = "md5"),
         NameSuffix = digest(NameSuffix, algo = "md5"),
         SSN = digest(SSN, algo = "md5"))

# Write it back out -------------------------------------------------------

write.csv(
  Export,
  here(paste0(directory, "data/Export.csv")),
  na = "",
  row.names = FALSE
)

write.csv(
  Client_hashed,
  here(paste0(directory, "data/Client.csv")),
  na = "",
  row.names = FALSE
)

zip(zipfile = "hashed-short-date-range", paste0(directory, "data/"))

