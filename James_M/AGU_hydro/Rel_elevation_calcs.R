#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Relative Elevation Calcs
# Coder: James Maze
# Date: July 18th 2022
# Purpose: Calculate Relative Elevations Based on Water Level and Survey data. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#  - Cut out the modeled data from gap-filling? YES
#  - Need to talk to Nate about geospatial work. Did I use the right projection?

# 1.0 Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(stringr)
library(lubridate)
library(sf)
library(tidyverse)

data_dir <- "data\\AGU_hydro\\"

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

#Since the site data is on multiple sheets, its a little extra work to read. 
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
  filter(Date >= "2021-03-01") %>% 
  #Make Flag a factor not numeric
  mutate(Flag = as.factor(Flag))

#Clean up the environment
rm(water_levels)

# 4.0 Calculate the elevation head to datum and aggregate wtr lvls--------------------------------------------------------------------

#Need to calculate the elevation heads separately for each catchment since they have different datum

# 4.1 Jackson Lane --------------------------------------------------------
JL_rel_wtrlvl <- df %>% 
  filter(Catchment == "Jackson Lane") %>% 
  #Apply elevation offset
  mutate(Wtrlvl_rel_datum = dly_mean_wtrlvl + Elevation_m) %>%
  #Calculate daily mean water level for all sites. Could be helpful later. 
  group_by(Date) %>% 
  mutate(dly_mean_wtrlvl_allsites = mean(dly_mean_wtrlvl)) %>% 
  ungroup() %>% 
  #Calculate daily mean water level for all sites by well type (SW, CH, GW) in case different behavior is interesting. 
  group_by(well_type, Date) %>% 
  mutate(dly_mean_welltype_wtrlvl = mean(dly_mean_wtrlvl)) %>% 
  ungroup()

# 4.2 Baltimore Corner ----------------------------------------------------
BC_rel_wtrlvl <- df %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  #Apply elevation offset
  mutate(Wtrlvl_rel_datum = dly_mean_wtrlvl + Elevation_m) %>% 
  #Calculate daily mean waterlevel for all sites.
  group_by(Date) %>% 
  mutate(dly_mean_wtrlvl_allsites = mean(dly_mean_wtrlvl)) %>% 
  ungroup() %>% 
  #Calculate daily mean water level for all sites by well type. 
  group_by(well_type, Date) %>% 
  mutate(dly_mean_welltype_wtrlvl = mean(dly_mean_wtrlvl)) %>% 
  ungroup() 

#Get rid of generic variable name
rm(df)
#Recombine data frames after calculations.
Rel_wtr_lvls <- rbind(JL_rel_wtrlvl, BC_rel_wtrlvl)

# 5.0 Calculate head differences between sites ---------------------------------------------------------------------

# 5.1 Jackson Lane Head Difference Calcs --------------------------------------------------------

JL_heads <- JL_rel_wtrlvl %>% 
#Remove modeled data since its not appropriate for head differences. 
  filter(!Flag == 1) %>%
#Pivot wider to calculate head gradients between sites. 
  pivot_wider(id_cols = Date, 
              names_from = Site_ID, 
              values_from = Wtrlvl_rel_datum) %>% 
#Calculate head gradients between the sites. 
  mutate(BDSW_BDCH = `BD-SW` - `BD-CH`,
         BDSW_TSSW = `BD-SW` - `TS-SW`,
         BDSW_DKSW = `BD-SW` - `DK-SW`,
         BDSW_TSUW1 = `BD-SW` - `TS-UW1`,
         BDCH_TSSW = `BD-CH` - `TS-SW`,
         TSSW_BDCH = `TS-SW` - `BD-CH`,
         TSSW_TSCH = `TS-SW` - `TS-CH`,
         TSSW_TSUW1 = `TS-SW` - `TS-UW1`,
         TSSW_DKSW = `TS-SW` - `DK-SW`,
         TSSW_NDSW = `TS-SW` - `ND-SW`,
         TSSW_NDUW3 = `TS-SW` - `ND-UW3`,
         TSSW_DKUW1 = `TS-SW` - `DK-UW1`,
         TSSW_BDSW = `TS-SW` - `BD-SW`,
         DKSW_DKUW1 = `DK-SW` - `DK-UW1`,
         DKSW_DKUW2 = `DK-SW` - `DK-UW2`,
         DKSW_DKCH = `DK-SW` - `DK-CH`, 
         DKSW_TSCH = `DK-SW` - `TS-CH`,
         NDSW_DKSW = `ND-SW` - `DK-SW`,
         NDSW_TSUW1 = `ND-SW` - `TS-UW1`,
         NDSW_NDUW1 = `ND-SW` - `ND-UW1`,
         NDSW_NDUW2 = `ND-SW` - `ND-UW2`,
         NDSW_NDUW3 = `ND-SW` - `ND-UW3`,
         NDUW1_NDUW2 = `ND-UW1` - `ND-UW2`,
         NDUW1_NDUW3 = `ND-UW1` - `ND-UW3`,
         NDUW3_TSUW1 = `ND-UW3` - `TS-UW1`,
         DKSW_NDSW = `DK-SW` - `ND-SW`,
         DKSW_TSSW = `DK-SW` - `TS-SW`,
      #Looking at only GW head gradients
         DKUW2_DKUW1 = `DK-UW2` - `DK-UW1`,
         DKUW2_TSUW1 = `DK-UW2` - `TS-UW1`,
         DKUW2_NDUW3 = `DK-UW2` - `ND-UW3`,
         DKUW2_NDUW2 = `DK-UW2` - `ND-UW2`,
         DKUW2_NDUW1 = `DK-UW2` - `ND-UW1`,
         DKUW2_TSCH = `DK-UW2` - `TS-CH`,
         DKUW2_BDCH = `DK-UW2` - `BD-CH`,
         DKSW_BDSW = `DK-SW` - `BD-SW`) %>% 
  #Remove individual site waterlevels so that its only head differences
  select(-c("DK-SW", "DK-CH", "DK-UW1", "DK-UW2", "TS-CH", "TS-SW", "TS-UW1", 
            "BD-SW", "BD-CH", "ND-SW", "ND-UW1", "ND-UW2", "ND-UW3"))

#Pair daily mean water level at Jackson Lane to the head gradients data frame.
temp <- Rel_wtr_lvls %>% 
  #Select only JL daily mean water level data
  filter(Catchment == "Jackson Lane") %>%
  select(Date, dly_mean_wtrlvl_allsites) 

JL_heads <- left_join(JL_heads, temp)


#Pivot to the long format
JL_heads_long <- JL_heads %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_IDs",
               values_to = "Head_diff_m")

rm(JL_heads, temp, JL_rel_wtrlvl)

# 5.2 Baltimore Corner head diff calcs ----------------------------------------------------

BC_heads <- BC_rel_wtrlvl %>% 
  #Remove modeled data since its not appropriate for head differences. 
  filter(!Flag == 1) %>%
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
         MBSW_HBSW = `MB-SW` - `HB-SW`,
         OBSW_MBSW = `OB-SW` - `MB-SW`,
         OBCH_OBSW = `OB-CH` - `OB-SW`,
         OBSW_OBCH = `OB-SW` - `OB-CH`,
         OBSW_OBUW1 = `OB-SW` - `OB-UW1`,
         OBSW_MBUW1 = `OB-SW` - `MB-UW1`,
         OBSW_TPCH = `OB-SW` - `TP-CH`,
         OBSW_HBSW = `OB-SW` - `HB-SW`,
         XBSW_MBSW = `XB-SW` - `MB-SW`,
         XBSW_XBUW1 = `XB-SW` - `XB-UW1`,
         XBSW_XBCH = `XB-SW` - `XB-CH`) %>% 
  select(-c("TP-CH", "HB-CH", "HB-SW", "HB-UW1", "MB-CH", "MB-SW", "MB-UW1", "OB-CH",
            "OB-SW", "OB-UW1", "XB-SW", "XB-UW1", "XB-CH"))

#Pair daily mean water level to the head gradients
temp <- Rel_wtr_lvls %>%
  filter(Catchment == "Baltimore Corner") %>%
  select(Date, dly_mean_wtrlvl_allsites)

BC_heads <- left_join(BC_heads, temp)

#Pivot data to the long format
BC_heads_long <- BC_heads %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_IDs",
               values_to = "Head_diff_m") 

#Clean up the environment!
rm(BC_heads, temp, BC_rel_wtrlvl)

# 6.0 Calculate distance between sites for head gradients ---------------------

#Set a projection for the distance calculation. 
#!!! Might need to think more about this. Just used same projection that Nate uses. 
projection <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Join site data to the survey data for elevation differentials
survey_data <- survey_data %>% 
  select(-c(Catchment, Notes))

site_data <- left_join(site_data, survey_data, by = "Site_ID")

#Use the projection to get geospatial objects from site data
points <- site_data %>% 
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude)) %>% 
  #Need to group rowwise to apply st_point to each row
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(., projection) 

#function matching notation for head gradients to Site IDs. It ain't pretty, but it gets the job done. 
gradient_site_matcher <- function(head_differences_long) {
  
  temp <- head_differences_long %>% 
    #Get the 1st Site ID
    #Subset the 1st site to the left of the _ underscore
    mutate(SiteID_1st = str_extract(string = Site_IDs, 
                                    pattern = "(.+)[\\_]")) %>% 
    #Remove the _ from the end of the string
    mutate(SiteID_1st = str_extract(string = SiteID_1st,
                                    pattern = "[:alnum:]+")) %>% 
    #Add the "-" to match the lat/long data
    mutate(SiteID_1st = paste0(str_sub(SiteID_1st, 1, 2), "-", str_sub(SiteID_1st, 3, -1L))) %>% 
    #Get the 2nd Site ID
    #Subset the 2nd site to the right of the _ underscore
    mutate(SiteID_2nd = str_extract(string = Site_IDs, 
                                    pattern = "[\\_](.+)")) %>% 
    #Remove the _ from the end of the string
    mutate(SiteID_2nd = str_extract(string = SiteID_2nd,
                                    pattern = "[:alnum:]+")) %>% 
    #Add the "-" to match the lat/long data
    mutate(SiteID_2nd = paste0(str_sub(SiteID_2nd, 1, 2), "-", str_sub(SiteID_2nd, 3, -1L))) 
    
  (temp)
  
}

# 6.1 JL Distance Calculations --------------------------------------------

#Select the Jackson Lane sites.
JL_points <- points %>% 
  filter(Catchment == "Jackson Lane") %>% 
  #Clean up and rename columns
  select(-c(Catchment)) %>% 
  rename("point" = geometry,
         "Site_ID_temp" = Site_ID)

#Pull 1st and 2nd Site_IDs from each head gradient value using matching function
JL_dist_sites <- JL_heads_long %>% 
  #Get unique site ID's so you don't calculate distance for entire time series
  select(Site_IDs) %>% 
  unique() %>% 
  #Use site matching function to get 1st and 2nd sites
  gradient_site_matcher()

#Match the lat/long points to the 1st and 2nd site IDs
JL_dist_sites <- JL_dist_sites %>%
  #Match lat/long to 1st Site ID
  rename("Site_ID_temp" = SiteID_1st) %>%
  left_join(., JL_points, by = "Site_ID_temp") %>%
  #Rename columns accordingly
  rename("SiteID_1st" = Site_ID_temp,
         "SiteID_1st_latlong" = point,
         "SiteID_1st_elevation" = Elevation_m) %>%
  #Match lat/long to 2nd Site ID
  rename("Site_ID_temp" = SiteID_2nd) %>%
  left_join(., JL_points, by = "Site_ID_temp") %>%
  #Rename columns accordingly
  rename("SiteID_2nd" = Site_ID_temp,
         "SiteID_2nd_latlong" = point,
         "SiteID_2nd_elevation" = Elevation_m) 

#Use st_dist to calculate distance between 1st and 2nd site
JL_dist_sites <- JL_dist_sites %>%
  rowwise() %>% 
  #Calculate distance between the wells from Lat and Longs
  mutate(distance_m = st_distance(SiteID_1st_latlong, SiteID_2nd_latlong, by_element = T)) %>% 
  #Calculate elevation change between points
  mutate(distance_m = as.numeric(distance_m),
  #Calculate elevation change between points
         elevation_diff_m = as.numeric(SiteID_1st_elevation - SiteID_2nd_elevation)) %>% 
  select(c(Site_IDs, distance_m, elevation_diff_m))

#Join the distance values to the head differences 
JL_heads_long <- left_join(JL_heads_long, JL_dist_sites, by = "Site_IDs") 

#Calculate the head gradient (dh/dL)
JL_heads_long <- JL_heads_long %>% 
  mutate(head_gradient = (Head_diff_m / distance_m),
         elevation_gradient = (elevation_diff_m / distance_m))

#Clean up environment
rm(JL_dist_sites, JL_points)

# 6.2 BC Distance Calculations ----------------------------------------------------

#Select Baltimore Corner Sites
BC_points <- points %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  #Clean up and rename columns
  select(-c(Catchment)) %>% 
  rename("point" = geometry,
         "Site_ID_temp" = Site_ID)

#Pull 1st and 2nd Site_IDs from each head gradient value using matching function
BC_dist_sites <- BC_heads_long %>% 
  #Get unique site ID's so you don't calculate distance for entire time series
  select(Site_IDs) %>% 
  unique() %>% 
  #Use site matching function to get 1st and 2nd sites
  gradient_site_matcher()

#Match the lat/long points to the 1st and 2nd site IDs
BC_dist_sites <- BC_dist_sites %>%
  #Match lat/long to 1st Site ID
  rename("Site_ID_temp" = SiteID_1st) %>%
  left_join(., BC_points, by = "Site_ID_temp") %>%
  #Rename columns accordingly
  rename("SiteID_1st" = Site_ID_temp,
         "SiteID_1st_latlong" = point,
         "SiteID_1st_elevation" = Elevation_m) %>%
  #Match lat/long to 2nd Site ID
  rename("Site_ID_temp" = SiteID_2nd) %>%
  left_join(., BC_points, by = "Site_ID_temp") %>%
  #Rename columns accordingly
  rename("SiteID_2nd" = Site_ID_temp,
         "SiteID_2nd_latlong" = point,
         "SiteID_2nd_elevation" = Elevation_m) 

#Use st_distance to calculate distance between sites
BC_dist_sites <- BC_dist_sites %>%
  rowwise() %>% 
  #Calculate the distance from the lat and longs
  mutate(distance_m = st_distance(SiteID_1st_latlong, SiteID_2nd_latlong, by_element = T)) %>% 
  mutate(distance_m = as.numeric(distance_m),
  #Calculate elevation difference between points
         elevation_diff_m = (as.numeric(SiteID_1st_elevation - SiteID_2nd_elevation))) %>% 
  #Select columns of interest
  select(c(Site_IDs, distance_m, elevation_diff_m))

#Join the distance values to the head differences 
 BC_heads_long <- left_join(BC_heads_long, BC_dist_sites, by = "Site_IDs") 
 
 #Calculate the head gradient (dh/dL)
 BC_heads_long <- BC_heads_long %>% 
   #Divide by distance to get the gradients. 
   mutate(head_gradient = (Head_diff_m / distance_m),
          elevation_gradient = (elevation_diff_m / distance_m)) 
 
#Clean up the environment
rm(gradient_site_matcher, points, BC_points, BC_dist_sites)

# 7.0 Designate relationships for head types ------------------------------

# !!! Get some input from hydro group before writing the intricate function. 

# 8.0 Export data ---------------------------------------------------------

#Relative water levels csv
write_csv(Rel_wtr_lvls, file = paste0(data_dir, "output//rel_wtr_lvls.csv"))

#Combine files clean up NA's and duplicates before writing hydro_heads.csv
JL_heads_long <- JL_heads_long %>% 
  #Add column to designate catchment
  add_column(Catchment = "Jackson Lane")
BC_heads_long <- BC_heads_long %>% 
  #Add column to designate catchment
  add_column(Catchment = "Baltimore Corner")

hydro_heads <- bind_rows(JL_heads_long, BC_heads_long) %>% 
  filter(!is.na(Head_diff_m)) %>% 
  unique()

#Write hydro heads csv
write_csv(hydro_heads, file = paste0(data_dir, "output//hydro_heads.csv"))  
  
