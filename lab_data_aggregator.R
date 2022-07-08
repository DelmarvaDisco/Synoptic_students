#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Lab Data Aggregator
# Coder: James Maze
# Date: 16 June 2022
# Purpose: Aggregating Lab Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Still need to incorperate GHG, talk to Carla about formatting.
# - Drops some meta-data specific to analyses. Granularity sacrificed for malleability. 
# - Eliminated all CBL data to avoid duplicates. If someone wants to do comps between CBL & VT,
# then be my guest. 
# - !!!Syntax is still screwed for Sample_IDs and Observation_IDs (i h8 g-sheets)!!!



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
  
  #Read in the "trash" version for sole purpose of extracting units from header
  trash <- read_xlsx(file_paths,
                     col_types = "text") 
  #Extract units
  unit <- trash %>% 
    select(`Units`) %>%
    slice_head(n = 1) %>% 
    pull(1)
  #Remove "trash" (throw-away) version
  rm(trash)
  
  #Read in the file
  temp <- read_xlsx(file_paths,
                   skip = 10, 
                   col_types = "text") %>% 
    as_tibble() %>% 
    mutate("Sample_ID" = paste0(Site_ID, "-", Bottle, "-", Rep, "-", Sample_Date)) %>%
    #Multiple analytes for some sample bottles requires additional distinction
    mutate("Observation_ID" = paste0(Sample_ID, "-", Analyte)) %>% 
    mutate("Units" = print(unit)) %>% 
    select(c(Sample_ID, Observation_ID, Value, Flag, Site_ID, 
             Sample_Date, Flag_notes, Units))

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

#Concatenate file paths
file_paths <- c(anion_files, isotope_files, NPOC_files, nutrient_files)
rm(anion_files, isotope_files, NPOC_files, nutrient_files)

#Run the download function
df <- file_paths %>% 
  map(download_fun1) %>% 
  reduce(bind_rows)

#Can't figure out what is wrong with the 202007_NPOC file?!?!?
hmm <- df %>% 
  filter(is.na(Units))
  
# 3. Read in the ICPMS metals data ----------------------------------------

Spectroscopy_files <- list.files(paste0(data_dir, "Spectroscopy"), full.names = TRUE) 
Spectroscopy_files <- Spectroscopy_files[str_detect(Spectroscopy_files,".xlsx")]

#Download function for ICPMS data (slight modifacation)
download_fun_spec <- function(file_paths){
  
  trash <- read_xlsx(file_paths,
                     col_types = "text") 
  
  unit <- trash %>% 
    select(`Units`) %>%
    slice_head(n = 1) %>% 
    pull(1)
  
  rm(trash)
  
  temp <- read_xlsx(paste0(file_paths),
                    skip = 10,
                    col_types = "text") %>% 
    as_tibble() %>% 
    mutate("Sample_ID" = paste0(Site_ID, "-", Bottle, "-", Rep, "-", Sample_Date)) %>%
    mutate("Units" = print(unit)) %>% 
    select(-c(Bottle, Rep))
    
  (temp)
}

#Run the download function
dt <- Spectroscopy_files %>% 
  map(download_fun_spec) %>% 
  reduce(bind_rows)
  
#Wide to Long format
dt <- dt %>% 
  tidyr::pivot_longer(cols = -c(Sample_ID, Flag, Flag_Notes, Units, 
                                Sample_Date, Site_ID), 
               names_to = "Analyte", 
               values_to = "Value")

dt <- dt %>% 
  mutate("Observation_ID" = paste0(Sample_ID, "-", Analyte)) %>% 
  select(-c(Analyte)) 
  #filter(!is.na(Value))

rm(Spectroscopy_files)

# 4. Read in the GHG data ----------------------------------------------------
GHG_files <- list.files(paste0(data_dir, "Dissolved Gases/Complete"), full.names = TRUE) 

# download_fun_ghg <- function(file_paths) {
#   
#   read_xlsx(file_paths, 
#             col_types = "text") %>% 
#     as_tibble() %>% 
#     select(c(Sample_ID, Flag, ))
# }

# dx <- GHG_files %>% 
#   map(download_fun_ghg) %>% 
#   reduce(bind_rows)

# 5. Combine & reformat the data -------------------------------------------------

dt <- dt %>% 
  rename("Flag_notes" = `Flag_Notes`)

data <- rbind(dt, df)
rm(dt,df)

data <- data %>% 
  #Sample dates were in scientific notation, because Google Sheets is dumb 
  #Removes period from dates read as sci notation
  mutate(Date = str_replace(Sample_Date, 
                            pattern = "([.])",
                            replacement = "")) %>% 
  #Removes the e^x portion of dates read as sci notation
  mutate(Dates = str_trunc(Date, width = 8, side = "right", ellipsis = "")) %>%
  mutate(Year = str_sub(Dates, 1, 4),
         Month = str_sub(Dates, 5, 6),
         Day = str_sub(Dates, 7, 8)) %>% 
  #For dates that end in 0, E replaced 0 when date was sci notation. 
  #This line replaces the E with correct value (e.g. 2021092E becomes 20210920)
  mutate(Day = str_replace(Day, "E", "0")) %>% 
  mutate(Sample_Date = lubridate::ymd(paste0(Year, "-", Month, "-", Day))) %>% 
  select(-c(Date, Dates, Day)) 

#Dates failed to parse on a few calibration chk samples
hmm <- data %>% 
  filter(is.na(Sample_Date))


# 6. Export to new csv --------------------------------------------------------

write_csv(data, file = paste0(data_dir, "lab_data_aggregated_JM.csv"))









