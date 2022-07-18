#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Head gradients analysis
# Coder: James Maze
# Date: July 18th 2022
# Purpose: Relate water level data to survey data at catchment sites. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#  - Cut out the modeled data from gap-filling
#  - How to incorperate distance between wells with "sf" package
#  - Any fun packages to model the potentio-metric surface?


# 1.0 Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)

data_dir <- "data\\Head_gradients\\"
plots_dir <- "data\\Head_gradients\\plots\\"

# 2.0 Read in the data ----------------------------------------------------

#Waterlevel dataset 
water_levels <- read_csv(paste0(data_dir, "dly_mean_output_JM_2019_2022.csv")) %>% 
  #The modeled data (used for gap filling) is not appropriate for this analysis
  filter(!Flag == 1) %>% 
  #No longer need flag column
  select(-Flag)

#Survey data from Mar 2022
survey_data <- read_xlsx(paste0(data_dir, "Relative_elevations.xlsx")) %>% 
  #Convert elevation data from cm to meters
  mutate(Elevation_m = Elevation_cm/100) %>% 
  select(-Elevation_cm)

#Since the site data is on multiple sheets, its a pain to read. 
site_data_path <- paste0(data_dir, "Site_directory_core.xlsx")
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
  select(-Notes)

#Clean up the environment
rm(water_levels, survey_data, site_data)

# 4.0 Calculate the head gradient relative to the catchment bottom --------------------------------------------------------------------

# 4.1 Jackson Lane --------------------------------------------------------
JL_heads <- df %>% 
  filter(Catchment == "Jackson Lane") %>% 
  select(-Catchment) %>% 
  mutate(Wtrlvl_rel_DKSW_bottom = dly_mean_wtrlvl + Elevation_m)

# 4.2 Baltimore Corner ----------------------------------------------------
BC_heads <- df %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  select(-Catchment) %>% 
  mutate(Wtrlvl_rel_TPCH_bottom = dly_mean_wtrlvl + Elevation_m)

# 5.0 Quick plot of site's relative water levels ---------------------------------

#Quick time series ggplot function might be helpful later
ts_quick <- function(data, y_var, color_var, title) {
  
  data <- data
  
  ts_plot <- ggplot(data = data, 
                    aes(x = Date,
                        y = {{y_var}},
                        color = {{color_var}})) +
    geom_line(size = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 28))
    ggtitle({{title}})
  
  return(ts_plot)
  (ts_plot)
    
}

#Look at time series of head gradients relative to outlets. 
JL_ts <- ts_quick(data = JL_heads %>% filter(Date >= "2021-03-04",
                                             Site_ID %in% c("DK-SW", "TS-SW", "TS-CH", 
                                                            "BD-CH", "BD-SW")), 
                 y_var = Wtrlvl_rel_DKSW_bottom,
                 color_var = Site_ID, 
                 title = "Elevation heads (cm) relative to DK-SW")
(JL_ts)

BC_ts <- ts_quick(data = BC_heads %>% filter(Date >= "2021-03-04"), 
                  y_var = Wtrlvl_rel_TPCH_bottom,
                  color_var = Site_ID, 
                  title = "Elevation heads (cm) relative to TP-CH")
(BC_ts)











