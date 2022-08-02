#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Head Differences Timeseries and Correlation Plots
# Coder: James Maze
# Date: August 2nd 2021
# Purpose: Explore Head Changes in Catchment Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#  - Cut out the modeled data from gap-filling???

# 1.0 Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)

data_dir <- "data\\Head_gradients\\output\\"
plot_dir <- "data\\Head_gradients\\plots\\"

# 2.0 Read in the data ---------------------------------------------------------------------



# 3.0 Time series of wtrlvls relative to datum -------------------------------------



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
                                                     "BD-CH", "BD-SW", "DK-CH",
                                                     "ND-SW")), 
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
                                                          "OB-CH", "HB-CH")), 
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



# 5.2 JL Head diff plots --------------------------------------------------

#Plot head gradients between wetlands and the catchment

ND_heads_ts <- ts_quick_plot(data = JL_heads_long %>% 
                               filter(Site_IDs %in% c("NDSW_NDUW1", "NDSW_NDUW2", 
                                                      "NDSW_NDUW3", "NDUW1_NDUW2")),
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