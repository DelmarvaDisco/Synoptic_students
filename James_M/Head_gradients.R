#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Head gradients analysis
# Coder: James Maze
# Date: July 18th 2022
# Purpose: Relate water level data to survey data at catchment sites. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#  - How to make head gradients at UW sites less noisy from infiltration?!? Is using zoo::rollmean appropriate??
#  - Cut out the modeled data from gap-filling
#  - How to incorporate distance between wells with "sf" package?
#  - Any fun packages to model the potentio-metric surface?

# 1.0 Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(zoo)
library(sf)
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
  select(-Notes) %>% 
  mutate(site_type = str_sub(Site_ID, 4, 6)) %>% 
  mutate(wetland = str_sub(Site_ID, 1, 2)) %>% 
  filter(Date >= "2021-03-01")

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
                                    filter(#Filter sites of interest
                                           Site_ID %in% c("DK-SW", "TS-SW", "TS-CH", 
                                                          "BD-CH", "BD-SW", "DK-CH")), 
                 y_var = Wtrlvl_rel_DKSW_bottom,
                 color_var = Site_ID, 
                 title = "SW & CH elevation heads (m) relative to datum at DK-SW")
#Print plot
(JL_SW_CH_elheads)

#Upland wells
JL_UW_elheads <- ts_quick_plot(data = JL_rel_wtrlvl %>% 
                                 filter(#Filter UW sites
                                        site_type %in% c("UW1", "UW2", "UW3"),
                                      !(Site_ID == "TS-UW1" &
                                          Wtrlvl_rel_DKSW_bottom <= 0.045)),
                               y_var = Wtrlvl_rel_DKSW_bottom,
                               color_var = Site_ID, 
                               title = "GW elevation heads (m) relative to datum at DK-SW")

#Print the plot
(JL_UW_elheads)

# 5.2 Baltimore Corner ----------------------------------------------------

BC_SW_CH_elheads <- ts_quick_plot(data = BC_rel_wtrlvl %>% 
                                    filter(Site_ID %in% c("TP-CH", "HB-SW", "MB-SW",
                                                          "XB-SW", "OB-SW", "MB-CH",
                                                          "OB-CH")), 
                  y_var = Wtrlvl_rel_TPCH_bottom,
                  color_var = Site_ID, 
                  title = "SW & CH elevation heads (m) relative to datum at TP-CH")

#Print the plot
(BC_SW_CH_elheads)

BC_UW_elheads <- ts_quick_plot(data = BC_rel_wtrlvl %>% 
                                 filter(Site_ID == "TP-CH" |
                                        site_type == "UW1"), 
                  y_var = Wtrlvl_rel_TPCH_bottom,
                  color_var = Site_ID, 
                  title = "GW elevation heads (m) relative to datum at TP-CH")
#Print the plot
(BC_UW_elheads)

#Clean up environment
rm(BC_SW_CH_elheads, BC_UW_elheads, JL_SW_CH_elheads, JL_UW_elheads)

# 6.0 Calculate head gradients between sites ---------------------------------------------------------------------

# 6.1 Jackson Lane Calcs --------------------------------------------------------

JL_heads <- JL_rel_wtrlvl %>% 
#Pivot wider to calculate head gradients between site. 
  pivot_wider(id_cols = Date,
              names_from = Site_ID, 
              values_from = Wtrlvl_rel_DKSW_bottom) %>% 
#Calculate head gradients between the sites. 
  mutate(BDSW_BDCH = `BD-SW` - `BD-CH`,
         BDSW_TSSW = `BD-SW` - `TS-SW`,
         BDSW_DKSW = `BD-SW` - `DK-SW`,
         BDCH_TSSW = `BD-CH` - `TS-SW`,
         TSSW_BDCH = `TS-SW` - `BD-CH`,
         TSSW_TSCH = `TS-SW` - `TS-CH`,
         TSSW_TSUW1 = `TS-SW` - `TS-UW1`,
         TSSW_DKSW = `TS-SW` - `DK-SW`,
         DKSW_DKUW1 = `DK-SW` - `DK-UW1`,
         DKSW_DKUW2 = `DK-SW` - `DK-UW2`,
         DKSW_DKCH = `DK-SW` - `DK-CH`, 
         NDSW_DKSW = `ND-SW` - `DK-SW`,
         NDSW_DKCH = `ND-SW` - `DK-CH`,
         NDSW_NDUW1 = `ND-SW` - `ND-UW1`,
         NDSW_NDUW2 = `ND-SW` - `ND-UW2`,
         NDSW_NDUW3 = `ND-SW` - `ND-UW3`,
         NDUW3_TSUW1 = `ND-UW3` - `TS-UW1`) %>% 
  select(-c("DK-SW", "DK-CH", "DK-UW1", "DK-UW2", "TS-CH", "TS-SW", "TS-UW1", 
            "BD-SW", "BD-CH", "ND-SW", "ND-UW1", "ND-UW2", "ND-UW3"))

#Pivot data table longer to add more data as needed. 
JL_heads_long <- JL_heads %>% 
  pivot_longer(cols = -c(Date),
               names_to = "Site_IDs",
               values_to = "Head_diff_m") 


# 6.2 JL Head diff plots --------------------------------------------------

#Plot head gradients between wetlands and the catchment

ND_heads_ts <- ts_quick_plot(data = JL_heads_long %>% 
                             filter(Site_IDs %in% c("NDSW_DKSW", "NDSW_NDUW1", "NDSW_NDUW2", 
                                                    "NDSW_NDUW3", "NDUW3_TSUW1")),
                      y_var = Head_diff_m, 
                      color_var = Site_IDs,
                      title = "Head differences between ND, Upland Wells & Outlet")
(ND_heads_ts)

TS_heads_ts <- ts_quick_plot(data = JL_heads_long %>% 
                               filter(Site_IDs %in% c("TSSW_DKSW", "TSSW_TSUW1", "TSSW_BDCH", 
                                                      "TSSW_TSCH")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between TS, Upland Wells, Channels & Outlet")

#View the plot
(TS_heads_ts)

rm(ND_heads_ts, TS_heads_ts)

# 6.3 Baltimore Corner calcs ----------------------------------------------------

BC_heads <- BC_rel_wtrlvl %>% 
  #Need to pivot_wider to calculate gradients
  pivot_wider(id_cols = Date, 
              names_from = Site_ID,
              values_from = Wtrlvl_rel_TPCH_bottom) %>% 
  #Calculate head differences between sites of interest. 
  mutate(TPCH_HBSW = `TP-CH`- `HB-SW`,
         TPCH_MBSW = `TP-CH` - `MB-SW`,
         TPCH_OBSW = `TP-CH` - `OB-SW`, 
         TPCH_XBSW = `TP-CH` - `XB-SW`, 
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

# 6.4 BC Head diff plots --------------------------------------------------



# 7.0 Correlate water level to catchment scale gradients --------------------------------------------------------------------

JL_mean_wtr_lvl <- df %>% 
  filter(Catchment == "Jackson Lane")
  group_by()

JL_heads_long <- left_join()
  
JL_heads_plot <- ggplot(data = JL_heads_long,
                        mapping = aes(x = Head_diff_m,
                                      y = wtr_lvl))
