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
library(purrr)
library(dplyr)

data_dir <- "data/lab_data_combo/"


# 2. Read in the data ----------------------------------------------------------------------

# Download function for Isotopes, Nutrients, DOC (quantity), SO4-, Cl-

download_fun1 <- function(file_paths){
  
  
  trash <- read_xlsx(file_paths,
                     col_types = "text") 
  
  unit <- trash %>% 
    select(`Units`) %>%
    slice_head(n = 1) %>% 
    pull(1)
  
  rm(trash)
    
  temp <- read_xlsx(file_paths,
                   skip = 10, 
                   col_types = "text") %>% 
    as_tibble() %>% 
    mutate("Sample_ID" = paste0(Site_ID, "-", Bottle, "-", Rep, "-", Sample_Date)) %>% 
    mutate("Observation_ID" = paste0(Sample_ID, "-", Analyte)) %>% 
    mutate("Units" = print(unit)) %>% 
    select(c(Sample_ID, Observation_ID, Value, Flag, Flag_notes, Units))

  (temp)
  
}

####### Test space 



isotope_files <- list.files(paste0(data_dir, "Isotopes"), full.names = TRUE) 
isotope_files <- isotope_files[!str_detect(isotope_files, "Precip")]

anion_files <- list.files(paste0(data_dir, "Anions"), full.names = TRUE)

NPOC_files <- 

file_paths <- c(anion_files, isotope_files)
rm(anion_files, isotope_files)


df <- file_paths %>% 
  map(download_fun1) %>% 
  reduce(bind_rows)


hmm <- df %>% 
  filter(is.na(Units))












