#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Lab Data Aggregator
# Coder: James Maze
# Date: 16 June 2022
# Purpose: Aggregating Lab Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Drops some meta-data specific to analyses. Granularity sacrficed for maleability. 
# - Eliminated all CBL data to avoid duplicates. If someone wants to do comps between CBL & VT,
# then be my guest. 
# _ 


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

#Isotope Data
isotope_files <- list.files(paste0(data_dir, "Isotopes"), full.names = TRUE) 
isotope_files <- isotope_files[!str_detect(isotope_files, "Precip")]

#Anion Data
anion_files <- list.files(paste0(data_dir, "Anions"), full.names = TRUE)

#NPOC Data
NPOC_files <- list.files(paste0(data_dir, "DOC Concentration"), full.names = TRUE)
NPOC_files <- NPOC_files[str_detect(NPOC_files, "NPOC")]
#Remove the CBL data to eliminate duplicates
NPOC_files <- NPOC_files[!str_detect(NPOC_files, "CBL")]

#Nutrient Data
nutrient_files <- list.files(paste0(data_dir, "Nutrients"), full.names = TRUE)
#Remove the CBL data to eliminate duplicates
nutrient_files <- nutrient_files[!str_detect(nutrient_files, "CBL")]

#Concatonate file paths
file_paths <- c(anion_files, isotope_files, NPOC_files, nutrient_files)
rm(anion_files, isotope_files, NPOC_files)

#Run the download function
df <- file_paths %>% 
  map(download_fun1) %>% 
  reduce(bind_rows)

# 3. Read in the ICPMS metals data ----------------------------------------


# 4. Read in the GHG data ----------------------------------------------------


# 5. Combine all the data -------------------------------------------------


# 6. Reformat everything --------------------------------------------------


# 7. Export to new csv --------------------------------------------------------












