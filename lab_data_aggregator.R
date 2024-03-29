#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Lab Data Aggregation
# Coder: James Maze
# Date: 16 June 2022
# Purpose: Aggregating Lab Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Drops some meta-data specific to analyses. Granularity sacrificed for malleability. 
# - Eliminated all CBL data to avoid duplicates. If someone wants to do comps between CBL & VT,
# then be my guest. 
# - In the wide file version specific MDL info wasn't feasible in format, 
# used 0.5 * MDL for all BD values. 

# 1. Libraries and Packages -----------------------------------------------

remove(list = ls())

library(tidyselect)
library(tidyverse)
library(readxl)

data_dir <- "data/lab_data_combo/"

# 2. Read in the WQL data ----------------------------------------------------------------------

# Download function for Isotopes, Nutrients, DOC (quantity), SO4-, Cl-
download_fun1 <- function(file_paths){
  
  #Read in the "trash" version as a placeholder with sole purpose of extracting units from header
  trash <- read_xlsx(file_paths,
                     col_types = "text") 
  
  #Extract units from placeholder
  unit <- trash %>% 
    select(`Units`) %>%
    slice_head(n = 1) %>% 
    pull(1)
  
  #Extract the MDL from the placeholder
  MDL <- trash %>% 
    select(`MDL`) %>% 
    slice_head(n = 1) %>% 
    pull(1)
  
  #Clean up environment
  rm(trash)
  
  #Read in the file
  temp <- read_xlsx(file_paths,
                   skip = 10, 
                   col_types = "text") %>% 
    as_tibble() %>% 
    #Sample dates were in scientific notation, because Google Sheets has dumb formatting
    #Removes period from dates read as sci notation
    mutate(Date = str_replace(Sample_Date, 
                              pattern = "([.])",
                              replacement = "")) %>% 
    #Removes the e^x portion of dates read as sci notation
    mutate(Dates = str_trunc(Date, width = 8, side = "right", ellipsis = "")) %>%
    #Pull Day, Month, and Year
    mutate(Year = str_sub(Dates, 1, 4),
           Month = str_sub(Dates, 5, 6),
           Day = str_sub(Dates, 7, 8)) %>% 
    #For dates that end in 0, E replaced 0 when date was sci notation. 
    #This line replaces the E with correct value (e.g. 2021092E becomes 20210920)
    mutate(Day = str_replace(Day, "E", "0")) %>% 
    #Make a new sample date column using correctly modified dates
    mutate(Sample_Date = lubridate::ymd(paste0(Year, "-", Month, "-", Day))) %>% 
    select(-c(Date, Dates)) %>% 
    mutate("Sample_ID" = paste0(Site_ID, "-", Bottle, "-", Rep, "-", Year, Month, Day),
    #Add units column
           "Units" = print(unit),
    #Add MDL column
           "MDL" = print(MDL)) %>% 
    #Create Observation ID column to distinguish multiple analytes per sample
    mutate("Observation_ID" = paste0(Sample_ID, "-", Analyte)) %>%
    #I screwed up the naming convention
    rename("Flag_Notes" = Flag_notes) %>% 
    select(c(Site_ID, Sample_Date, Sample_ID, Observation_ID, Units, Value, 
             Flag, Flag_Notes, MDL, Analyte, Month))

  (temp)
  
}

#Select isotope Data
isotope_files <- list.files(paste0(data_dir, "Isotopes"), full.names = TRUE) 
isotope_files <- isotope_files[!str_detect(isotope_files, "Precip")]

#Select anion Data
anion_files <- list.files(paste0(data_dir, "Anions"), full.names = TRUE)

#NPOC Data
NPOC_files <- list.files(paste0(data_dir, "DOC Concentration"), full.names = TRUE)
NPOC_files <- NPOC_files[str_detect(NPOC_files, "NPOC")]
#Remove the CBL data to eliminate duplicate NPOC values.
NPOC_files <- NPOC_files[!str_detect(NPOC_files, "CBL")]

#Nutrient Data
nutrient_files <- list.files(paste0(data_dir, "Nutrients"), full.names = TRUE)
#Remove the CBL data to eliminate duplicates
nutrient_files <- nutrient_files[!str_detect(nutrient_files, "CBL")]
#Remove the other duplicate nutrient file
nutrient_files <- nutrient_files[!str_detect(nutrient_files, "TOC-V")]

#Concatenate file paths for download function
file_paths <- c(anion_files, isotope_files, NPOC_files, nutrient_files)
rm(anion_files, isotope_files, NPOC_files, nutrient_files)

#Run the download function
anion_npoc_iso_nut_data <- file_paths %>% 
  map(download_fun1) %>% 
  reduce(bind_rows)

rm(file_paths)
  
# 3. Read in the ICPMS metals data ----------------------------------------

#Select the spectroscopy files from the directory
Spectroscopy_files <- list.files(paste0(data_dir, "Spectroscopy"), full.names = TRUE) 
Spectroscopy_files <- Spectroscopy_files[str_detect(Spectroscopy_files,".xlsx")]

#Download function for ICPMS data (slight modification from other file's download fun)
download_fun_spec <- function(spec_file_paths){
  
  #This "trash" object is just a place-holder to get units
  trash <- read_xlsx(spec_file_paths,
                     col_types = "text") 
  #Remove the units
  unit <- trash %>% 
    select(`Units`) %>%
    slice_head(n = 1) %>% 
    pull(1)
  
  #Extract the MDL from the placeholder
  MDL <- trash %>% 
    select(`MDL`) %>% 
    slice_head(n = 1) %>% 
    pull(1)
  
  #clean up environment
  rm(trash)
  
  
  temp <- read_xlsx(paste0(spec_file_paths),
                    skip = 10,
                    col_types = "text") %>% 
    as_tibble() %>% 
    #Sample dates were in scientific notation, because Google Sheets is dumb 
    #Removes period from dates that are read as scientific notation
    mutate(Date = str_replace(Sample_Date, 
                              pattern = "([.])",
                              replacement = "")) %>% 
    #Removes the "e^x" portion of dates read as scientific notation
    mutate(Dates = str_trunc(Date, width = 8, side = "right", ellipsis = "")) %>%
    mutate(Year = str_sub(Dates, 1, 4),
           Month = str_sub(Dates, 5, 6),
           Day = str_sub(Dates, 7, 8)) %>% 
    #For dates that end in 0, E replaced 0 when date was sci notation. 
    #This line replaces the E with correct value (e.g. 2021092E becomes 20210920)
    mutate(Day = str_replace(Day, "E", "0")) %>% 
    #Create a new date column using the reformatted dates. 
    mutate(Sample_Date = lubridate::ymd(paste0(Year, "-", Month, "-", Day))) %>% 
    #Get rid of the placeholder columns
    select(-c(Date, Dates)) %>% 
    #Redo the Sample_ID column based on the correct dates
    mutate("Sample_ID" = paste0(Site_ID, "-", Bottle, "-", Rep, "-", Year, Month, Day)) %>%
    #Add a units column
    mutate("Units" = print(unit),
           "MDL" = print(MDL)) %>% 
    select(-c(Bottle, Rep, Year, Day))

  #Return clean data frame.     
  (temp)
  
}

#Run the download function
spec_data <- Spectroscopy_files %>% 
  map(download_fun_spec) %>% 
  reduce(bind_rows)
  
#Wide to Long format in order to merge ICP-MS metals with other data
spec_data <- spec_data %>% 
  tidyr::pivot_longer(cols = -c(Sample_ID, Flag, Flag_Notes, Units, MDL,
                                Sample_Date, Site_ID, Month), 
               names_to = "Analyte", 
               values_to = "Value")

#Clean up the column names
spec_data <- spec_data %>% 
  #Create Observation_ID column because there are multiple analytes per Sample_ID
  mutate("Observation_ID" = paste0(Sample_ID, "-", Analyte)) 

#Clean up environmnet 
rm(Spectroscopy_files)

# 4. Read in the GHG data ----------------------------------------------------
#Since the field flags were not included in the clean files, I had to get them from the sumary files. 
#Two download functions 1. Flag download 2. Data download. Not efficient, but oh well. 

# 4.1 Read in GHG Flags ---------------------------------------------------

#Isolate the files in the GHG summary folder
ghg_files_flags <- list.files(paste0(data_dir, "Dissolved Gases/Complete"), full.names = TRUE) 

#Filter the extra unwanted files from the list
ghg_files_flags <- ghg_files_flags[str_detect(ghg_files_flags, "_GHG_NEON")]

#Download function specific for the GHG files
download_fun_ghg_flags <- function(ghg_files_flags){
  
  temp <- read_csv(paste0(ghg_files_flags)) %>% 
    as_tibble() %>% 
    #Columns of interest
    select(c("Site_ID", "Sample_Date",
             "Flag", "Flag_field", "Flag_Lab")) %>%
    #Combine the field and lab flag columns into a single column with notes.
    mutate(Flag_Notes = paste0(Flag_field, ", ", Flag_Lab)) %>% 
    #Gets rid of the repeated "NA, NA" from pasting the flag notes column
    mutate(Flag_Notes = if_else(Flag_Notes == "NA, NA",
                                "NA",
                                Flag_Notes)) %>% 
    #Drop the extra "NA, " from when flags got pasted together
    mutate(Flag_Notes = str_replace(Flag_Notes, "NA, ", "")) %>% 
    #Drop the extra ", NA" from when flags got pasted together
    mutate(Flag_Notes = str_replace(Flag_Notes, ", NA", "")) %>% 
    #Remove field and lab flag columns
    select(-c("Flag_field", "Flag_Lab"))
  
  #Return tibble with flags
  (temp)
  
}

#Apply download function to files with flags and bind the rows. 
ghg_flags <- ghg_files_flags %>% 
  map(download_fun_ghg_flags) %>% 
  reduce(bind_rows)  

#There are some samples where one rep is flagged, but others aren't 
#(e.g. for DK-SW 20220101 R1 and R2 Flag = 0, but R3 Flag = 1)
#Since data across reps is averaged, list flagged reps together. 
#Group by Sited_ID and Sample date to get flags
ghg_flags <- ghg_flags %>% 
  group_by(Site_ID, Sample_Date) %>% 
  summarise(Flag_Notes = print(unique(Flag_Notes)),
            Flag = mean(Flag)) 
  #If one rep is flagged denote all of them is flagged
  # mutate(Flag = as.numeric(if_else(0.1 < Flag < 0.9, 
  #                       "1",
  #                       Flag))). 

rm(ghg_files_flags)

# 4.2 Read in GHG data ----------------------------------------------------

#List GHG summary file directory
ghg_files_data <- list.files(paste0(data_dir, "Dissolved Gases/Summary"), full.names = TRUE)

#Be sure to only pull summary files
ghg_files_data <- ghg_files_data[str_detect(ghg_files_data, "_GHG_clean")]

#Quick and easy download function for GHG data
download_fun_ghg_data <- function(ghg_files_data) {
  
  read_csv(paste0(ghg_files_data)) %>% 
    as_tibble() %>% 
    select(c("Site_ID", "Sample_Date", "CO2_uM", "CH4_uM"))
  
}

#Apply GHG download function to files list
ghg_data <- ghg_files_data %>% 
  map(download_fun_ghg_data) %>% 
  reduce(bind_rows)

rm(ghg_files_data)

# 4.3 Combine GHG flags and data & reformat to match other data ---------------------------------------------------------------------

ghg_data <- left_join(ghg_data, ghg_flags, by = c("Site_ID", "Sample_Date")) %>%
  #Not sure why there's NA Site_IDs
  filter(!is.na(Site_ID)) %>%
  tidyr::pivot_longer(
    cols = -c(Flag_Notes, Flag, Sample_Date, Site_ID),
    names_to = "Analyte",
    values_to = "Value") %>%
  #Generate Sample_ID column to match other data
  mutate(Sample_ID = paste0(Site_ID, "-GHG-AllRep-", Sample_Date),
    #Convert Sample_Date to date
        Day = str_sub(Sample_Date, 7, 8),
        Month = str_sub(Sample_Date, 5, 6),
        Year = str_sub(Sample_Date, 1, 4)) %>%
  mutate(Sample_Date = lubridate::ymd(paste0(Year, "-", Month, "-", Day))) %>%
  select(-c(Day, Year)) %>%
  #Make Analyte and units columns
  rename("Units" = Analyte) %>%
  mutate(Analyte = str_sub(Units, 1, 3)) %>%
  #Generate Observation_ID to match other data
  mutate(Observation_ID = paste0(Sample_ID, "-", Analyte),
         #Add MDL column
         MDL = "NA") 

rm(ghg_flags)

# 5. Combine the data and make final changes -------------------------------------------------

data <- rbind(anion_npoc_iso_nut_data, spec_data, ghg_data) %>% 
  #Create a well type column
  mutate(well_type = str_sub(Site_ID, 4, 5)) %>% 
  #Some random other samples such as miscellaneous outflows don't have a Site_ID
  #Marked them as "other"
  mutate(well_type = if_else(well_type %in% c("SW", "UW", "CH"),
                             well_type,
                             "other"))


#Verbage denoting dry sites to check against the Flag_Notes column. 
key_words <- paste0(c("no standing water", "No standing water", "No standing wtr", "Pumped sample from well",
                      "pumped samples from the well", "pumped sample from well", "Pumped water", "Pumped well",
                      "Sampled from well", "Sampled from the well", "Sample from well",
                      "Sample pumped from wetland well", "Sample pumped from the well",
                      "Sample pumped from well", "Well dry, sample pumped", "sample pumped from well"),
                    collapse = "|")


#Create a "Site_dry" column based on the key words. 
data <- data %>% 
  #If Flag_Notes are listed as NA values instead of having the characters "NA", 
  #then the keyword search produces NA values for the Site_dry column (These values should actually be "No").
  #Before searching keywords, need to change NA values to "NA" characters. 
  mutate(Flag_Notes = if_else(is.na(Flag_Notes),
                              "NA",
                              Flag_Notes)) %>% 
  #Check keywords against Flag Notes column. 
  mutate("Site_dry" = if_else(str_detect(Flag_Notes, pattern = key_words),
                              "Yes",
                              "No")) %>% 
  #Create a new MDL column with text describing units
  mutate(MDL_number = if_else(str_detect(MDL, 
                                         #The precision levels for d18O and d2H mess up MDL searching, 
                                         # so I left them out. 
                                         pattern = "Precision: d18O"),
                              "NA",
                              #Pattern extracts the numbers before and after a decimal point
                              as.character(str_extract_all(MDL, pattern = "\\d+\\.*\\d+")))) %>%
  #Convert MDL_number to numeric generating NA's data wit missing MDL values
  mutate(MDL_number = as.double(MDL_number)) %>% 
  #Remove tangentially related samples from side-projects
  filter(!Site_ID %in% c("Corline - Algal Leachate 6/21/21", "Corline - Leaf Leachate 6/21/21",
                         "DK-Sond-R1", "DK-Sond-R2", "MW-1", "MW-2", "MW-3", "MW-4",
                         "MW-5", "MW-6", "MW-7", "MW-8", "ND-sond-R1", "ND-sond-R2",
                         "FR-SW-MW-R2", "FR-SW-MW-R1", "SAV Flux Rest 11am",
                         "Flux Nat (no time)", "FN 12:15pm", 	
                         "FR OW 11:15am")) %>% 
  #Remove data with NA Analyte or Values
  filter(!is.na(Analyte)) %>% 
  filter(!str_detect(Value, "NA"))

#Check the other Flag Notes to make sure all the dry sites are noted. 
other_flags <- data %>% 
  filter(Site_dry == "No") %>%
  select(Flag_Notes) %>% 
  unique()

#Check if string manipulation worked to get MDL_number. 
MDL_types <- data %>% 
  select(MDL_number, MDL) %>% 
  unique()

#Clean up environment
rm(anion_npoc_iso_nut_data, spec_data, ghg_data, other_flags, MDL_types, key_words)


# 6. Create a wide version of the data ------------------------------------

#!!! Used 0.5 the MDL for BD values on the wide version of file. 
#!!! If there was no MDL on the file and Values were BD, set the MDL to zero and changed values to zero.

#Create a wide version of data
data_wide <- data %>% 
  #Some MDL numbers are missing (~60 observations). In that case, I changed the missing MDLs to 0.
  mutate(MDL_number = if_else(is.na(MDL_number),
                              0,
                              MDL_number)) %>% 
  #BD values changed to 0.5 MDL
  mutate(Value = if_else(str_detect(as.character(Value), pattern = "BD"), 
                          MDL_number * 0.5,
                          as.numeric(Value))) %>% 
  select(-c(MDL, Flag, Flag_Notes, MDL_number, Units, Sample_ID, Observation_ID)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  pivot_wider(id_cols = c("Sample_Date", "Site_ID", "Month", "well_type", "Site_dry"),
              names_from = "Analyte",
              values_from = "Value",
              #Had some duplicate CO2 and CH4 samples (n = 6), not sure why. 
              #Just took the mean of the duplicated GHGs.
              values_fn = mean)


# 7. Export to new csv --------------------------------------------------------


write_csv(data_wide, file = paste0(data_dir, "lab_data_aggregated_JM_summary_wide.csv"))

write_csv(data, file = paste0(data_dir, "lab_data_aggregated_JM_long.csv"))


