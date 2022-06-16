#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Lab Data Aggregator
# Coder: James Maze
# Date: 16 June 2022
# Purpose: Aggregating Lab Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# Drops some meta-data specific to analyses


# 1. Libraries and Packages -----------------------------------------------

remove(list = ls())

library(tidyselect)
library(stringr)
library(readr)
library(readxl)

data_dir <- "data/lab_data_combo/"


# 2. Read in the data ----------------------------------------------------------------------

isotope_data <- list.files(paste0(data_dir, "Isotopes"), full.names = TRUE)
  
  
isotope_data <- isotope_data[ , !str_detect("Precip")]










