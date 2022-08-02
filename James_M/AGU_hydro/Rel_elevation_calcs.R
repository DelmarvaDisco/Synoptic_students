#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Relative Elevation Calcs
# Coder: James Maze
# Date: July 18th 2022
# Purpose: Calculate Relative Elevations Based on Water Level and Survey data. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#  - Cut out the modeled data from gap-filling???

# 1.0 Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)

data_dir <- "data\\Head_gradients\\"

# 2.0 Read in the data ----------------------------------------------------

#Waterlevel dataset 
water_levels <- read_csv(paste0(data_dir, "input\\dly_mean_output_JM_2019_2022.csv")) #%>% 
  #The modeled data (used for gap filling) is not appropriate for this analysis
  # filter(!Flag == 1) %>%
  # #No longer need flag column
  # select(-Flag)

#Survey data from Mar 2022
survey_data <- read_xlsx(paste0(data_dir, "input\\Relative_elevations.xlsx")) %>% 
  #Convert elevation data from cm to meters
  mutate(Elevation_m = Elevation_cm/100) %>% 
  select(-Elevation_cm)

#Since the site data is on multiple sheets, its a pain to read. 
site_data_path <- paste0(data_dir, "input\\Site_directory_core.xlsx")
#Separate sheets converted into a list.
sheet_names <- excel_sheets(path = site_data_path)  
sheet_names <- sheet_names[1:3] %>% 
  as.list()

#Apply read_excel to sheets list. 
site_data <- lapply(sheet_names, 
                    function(x) read_excel(path = site_data_path, 
                                           sheet = x)) %>% 
  reduce(rbind) %>% 
  # These columns not important
  select(-c(`Wetland Name`, `Site Name`, `Description`)) %>% 
  # Filter sites of interest
  filter(Catchment %in% c("Baltimore Corner", "Jackson Lane"))

#Clean up environment
rm(site_data_path, sheet_names)

# 3.0 Combine the files ----------------------------------------------

#Join relative elevations to waterLevel data.
df <- left_join(water_levels, survey_data, by = "Site_ID") %>% 
  filter(!is.na(Catchment))

#Join GPS points to df
df <- left_join(df, site_data, by = c("Site_ID", "Catchment")) %>% 
  #Remove notes column explaining relative elevations.
  select(-Notes) %>% 
  mutate(site_type = str_sub(Site_ID, 4, 6)) %>% 
  mutate(well_type = str_sub(Site_ID, 4, 5)) %>% 
  mutate(wetland = str_sub(Site_ID, 1, 2)) %>% 
  #Only want data after the installation of Catchment Wells
  filter(Date >= "2021-03-01")

#Clean up the environment
rm(water_levels, survey_data, site_data)

# 4.0 Calculate the elevation head to datum --------------------------------------------------------------------

# 4.1 Jackson Lane --------------------------------------------------------
JL_rel_wtrlvl <- df %>% 
  filter(Catchment == "Jackson Lane") %>% 
  mutate(Wtrlvl_rel_datum = dly_mean_wtrlvl + Elevation_m) %>%
  group_by(Date) %>% 
  mutate(dly_mean_wtrlvl_allsites = mean(dly_mean_wtrlvl)) %>% 
  ungroup() %>% 
  group_by(well_type, Date) %>% 
  mutate(dly_mean_welltype_wtrlvl = mean(dly_mean_wtrlvl)) %>% 
  ungroup()

# 4.2 Baltimore Corner ----------------------------------------------------
BC_rel_wtrlvl <- df %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  mutate(Wtrlvl_rel_datum = dly_mean_wtrlvl + Elevation_m) %>% 
  group_by(Date) %>% 
  mutate(dly_mean_wtrlvl_allsites = mean(dly_mean_wtrlvl)) %>% 
  ungroup() %>% 
  group_by(well_type, Date) %>% 
  mutate(dly_mean_welltype_wtrlvl = mean(dly_mean_wtrlvl)) %>% 
  ungroup() 

df <- rbind(JL_rel_wtrlvl, BC_rel_wtrlvl)

write_csv(df, file = paste0(data_dir, "output//Rel_wtr_lvls"))

# 5.0 Calculate head gradients between sites ---------------------------------------------------------------------

# 5.1 Jackson Lane Calcs --------------------------------------------------------

JL_heads <- JL_rel_wtrlvl %>% 
#Pivot wider to calculate head gradients between site. 
  pivot_wider(id_cols = Date, 
              names_from = Site_ID, 
              values_from = Wtrlvl_rel_datum) %>% 
#Calculate head gradients between the sites. 
  mutate(BDSW_BDCH = `BD-SW` - `BD-CH`,
         BDSW_TSSW = `BD-SW` - `TS-SW`,
         BDSW_DKSW = `BD-SW` - `DK-SW`,
         BDCH_TSSW = `BD-CH` - `TS-SW`,
         TSSW_BDCH = `TS-SW` - `BD-CH`,
         TSSW_TSCH = `TS-SW` - `TS-CH`,
         TSSW_TSUW1 = `TS-SW` - `TS-UW1`,
         TSSW_DKSW = `TS-SW` - `DK-SW`,
         TSSW_NDSW = `TS-SW` - `ND-SW`,
         DKSW_DKUW1 = `DK-SW` - `DK-UW1`,
         DKSW_DKUW2 = `DK-SW` - `DK-UW2`,
         DKSW_DKCH = `DK-SW` - `DK-CH`, 
         NDSW_DKSW = `ND-SW` - `DK-SW`,
         NDSW_DKCH = `ND-SW` - `DK-CH`,
         NDSW_NDUW1 = `ND-SW` - `ND-UW1`,
         NDSW_NDUW2 = `ND-SW` - `ND-UW2`,
         NDSW_NDUW3 = `ND-SW` - `ND-UW3`,
         NDUW1_NDUW2 = `ND-UW1` - `ND-UW2`,
         NDUW3_TSUW1 = `ND-UW3` - `TS-UW1`) %>% 
  select(-c("DK-SW", "DK-CH", "DK-UW1", "DK-UW2", "TS-CH", "TS-SW", "TS-UW1", 
            "BD-SW", "BD-CH", "ND-SW", "ND-UW1", "ND-UW2", "ND-UW3"))

JL_heads_long <- JL_heads %>% 
  pivot_longer(cols = -c(Date),
               names_to = "Site_IDs",
               values_to = "Head_diff_m")

# 6.2 Baltimore Corner calcs ----------------------------------------------------

BC_heads <- BC_rel_wtrlvl %>% 
  #Need to pivot_wider to calculate gradients
  pivot_wider(id_cols = Date, 
              names_from = Site_ID,
              values_from = Wtrlvl_rel_datum) %>% 
  #Calculate head differences between sites of interest. 
  mutate(TPCH_HBSW = `TP-CH`- `HB-SW`,
         TPCH_MBSW = `TP-CH` - `MB-SW`,
         TPCH_OBSW = `TP-CH` - `OB-SW`, 
         TPCH_XBSW = `TP-CH` - `XB-SW`, 
         TPCH_HBCH = `TP-CH` - `HB-CH`,
         HBCH_HBSW = `HB-CH` - `HB-SW`,
         HBSW_HBUW1 = `HB-SW` - `HB-UW1`, 
         HBSW_MBCH = `HB-SW` - `MB-CH`, 
         MBCH_MBSW = `MB-CH` - `MB-SW`,
         MBSW_MBCH = `MB-SW` - `MB-CH`,
         MBSW_MBUW1 = `MB-SW` - `MB-UW1`,
         MBSW_XBUW1 = `MB-SW` - `XB-UW1`,
         MBSW_XBSW = `MB-SW` - `XB-SW`,
         MBSW_OBCH = `MB-SW` - `OB-CH`,
         OBCH_OBSW = `OB-CH` - `OB-SW`,
         OBSW_OBCH = `OB-SW` - `OB-CH`,
         OBSW_OBUW1 = `OB-SW` - `OB-UW1`,
         XBSW_MBSW = `XB-SW` - `MB-SW`,
         XBSW_XBUW1 = `XB-SW` - `XB-UW1`,
         XBSW_XBCH = `XB-SW` - `XB-CH`) %>% 
  select(-c("TP-CH", "HB-CH", "HB-SW", "HB-UW1", "MB-CH", "MB-SW", "MB-UW1", "OB-CH",
            "OB-SW", "OB-UW1", "XB-SW", "XB-UW1", "XB-CH"))

#Pivot data longer for ggplot
BC_heads_long <- BC_heads %>% 
  pivot_longer(cols = -c(Date),
               names_to = "Site_IDs",
               values_to = "Head_diff_m")

# 7.0 Export data ---------------------------------------------------------

write_csv(df, file = paste0(data_dir, "output//Rel_wtr_lvls.csv"))

write_csv(JL_heads_long, file = paste0(data_dir, "output//JL_head_diffs.csv"))

write_csv(BC_heads_long, file = paste0(data_dir, "output//BC_head_diffs.csv"))


