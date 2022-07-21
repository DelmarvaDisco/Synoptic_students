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
  # filter(!Flag == 1) %>% 
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
  select(-Notes) %>% 
  mutate(site_type = str_sub(Site_ID, 4, 6)) %>% 
  mutate(wetland = str_sub(Site_ID, 1, 2))

#Clean up the environment
rm(water_levels, survey_data, site_data)

# 4.0 Calculate the elevation head to datum --------------------------------------------------------------------

# 4.1 Jackson Lane --------------------------------------------------------
JL_rel_wtrlvl <- df %>% 
  filter(Catchment == "Jackson Lane") %>% 
  select(-Catchment) %>% 
  mutate(Wtrlvl_rel_DKSW_bottom = dly_mean_wtrlvl + Elevation_m)

# 4.2 Baltimore Corner ----------------------------------------------------
BC_rel_wtrlvl <- df %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  select(-Catchment) %>% 
  mutate(Wtrlvl_rel_TPCH_bottom = dly_mean_wtrlvl + Elevation_m)

# 5.0 Quick plot of site's relative water levels ---------------------------------

#Quick time series ggplot function might be helpful later
ts_quick_plot <- function(data, y_var, color_var, title) {
  
#This step adds na's to missing timesteps.
#Prevents geom_line from arbitrarily drawing lines between data gaps. 
  data <- data %>% 
    pivot_wider(id_cols = Date, 
                names_from = {{color_var}},
                values_from = {{y_var}}) %>% 
    pivot_longer(cols = -c(Date), 
                 names_to = "Site", 
                 values_to = "meters")

#Done with data wrangling, time to plot!!!
  ts_plot <- ggplot(data = data, 
                    aes(x = Date,
                        y = meters,
                        color = Site)) +
    geom_line(size = 1,
              na.rm = TRUE) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 28)) +
    ggtitle({{title}})
  
  return(ts_plot)
  (ts_plot)
    
}

# 5.1 Jackson Lane --------------------------------------------------------

#Look at time series of head gradients relative to outlets. 
JL_SW_CH_elheads <- ts_quick_plot(data = JL_rel_wtrlvl %>% 
                                    #Filter data with full well installations
                                    filter(Date >= "2021-03-04",
                                    #Filter sites of interest
                                           Site_ID %in% c("DK-SW", "TS-SW", "TS-CH", 
                                                          "BD-CH", "BD-SW")), 
                 y_var = Wtrlvl_rel_DKSW_bottom,
                 color_var = Site_ID, 
                 title = "Elevation heads (m) relative to datum at DK-SW")
#Print plot
(JL_SW_CH_elheads)

JL_UW_elheads <- ts_quick_plot(data = JL_rel_wtrlvl %>% 
                                 filter(Date >= "2021-03-04", 
                                      #Filter out UW sites
                                        site_type %in% c("UW1", "UW2", "UW3"),
                                      !(Site_ID == "TS-UW1" &
                                          Wtrlvl_rel_DKSW_bottom <= 0.045)),
                               y_var = Wtrlvl_rel_DKSW_bottom,
                               color_var = Site_ID, 
                               title = "Elevation heads (m) relative to datum at DK-SW")

#Print the plot
(JL_UW_elheads)

# 5.2 Baltimore Corner ----------------------------------------------------

BC_SW_CH_elheads <- ts_quick_plot(data = BC_rel_wtrlvl %>% 
                                    filter(Date >= "2021-03-04",
                                           Site_ID %in% c("TP-CH", "HB-SW", "MB-SW",
                                                          "XB-SW", "OB-SW", "MB-CH",
                                                          "OB-CH"),
                                        #Filter out points where site is dry
                                          !(Site_ID == "TP-CH" &
                                             Wtrlvl_rel_TPCH_bottom <= -0.30)), 
                  y_var = Wtrlvl_rel_TPCH_bottom,
                  color_var = Site_ID, 
                  title = "Elevation heads (m) relative to datum at TP-CH")

#Print the plot
(BC_SW_CH_elheads)

BC_UW_elheads <- ts_quick_plot(data = BC_rel_wtrlvl %>% 
                                 filter(Date >= "2021-03-04",
                                        Site_ID == "TP-CH" |
                                        site_type == "UW1",
                              #Filter out points where site is dry
                              !(Site_ID == "TP-CH" &
                                 Wtrlvl_rel_TPCH_bottom <= -0.30)), 
                  y_var = Wtrlvl_rel_TPCH_bottom,
                  color_var = Site_ID, 
                  title = "Elevation heads (m) relative to datum at TP-CH")
#Print the plot
(BC_UW_elheads)

#Clean up environment
rm(BC_SW_CH_elheads, BC_UW_elheads, JL_SW_CH_elheads, JL_UW_elheads)

# 6.0 Calculate head gradients between sites ---------------------------------------------------------------------

# 6.1 Jackson Lane --------------------------------------------------------

JL_heads <- JL_rel_wtrlvl %>% 
  pivot_wider(id_cols = Date,
              names_from = Site_ID, 
              values_from = Wtrlvl_rel_DKSW_bottom) %>% 
  mutate(BDSW_BDCH = `BD-SW` - `BD-CH`,
         TSSW_TSCH = `TS-SW` - `TS-CH`,
         TSSW_TSUW1 = `TS-SW` - `TS-UW1`,
         DKSW_DKUW1 = `DK-SW` - `DK-UW1`,
         DKSW_DKUW2 = `DK-SW` - `DK-UW2`,
         DKUW1_DKUW2 = `DK-UW1` - `DK-UW2`,
         DKSW_DKCH = `DK-SW` - `DK-CH`) %>% 
  select(-c("DK-SW", "DK-CH", "DK-UW1", "DK-UW2", "TS-CH", "TS-SW", "TS-UW1", 
            "BD-SW", "BD-CH", "ND-SW", "ND-UW1", "ND-UW2", "ND-UW3"))

JL_heads <- JL_heads %>% 
  pivot_longer(cols = -c(Date),
               names_to = "Site_IDs",
               values_to = "Head_diff_m")

hmmm <- ts_quick_plot(data = JL_heads %>% 
                        filter(Date >= "2021-03-04",
                               Site_IDs %in% c("DKSW_DKUW2", "DKSW_DKUW1", "DKSW_DKCH")),
                      y_var = Head_diff_m, 
                      color_var = Site_IDs,
                      title = "hmmm")
(hmmm)

# 6.2 Baltimore Corner ----------------------------------------------------


  


