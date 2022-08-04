#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Head Differences Timeseries and Correlation Plots
# Coder: James Maze
# Date: August 2nd 2021
# Purpose: Explore Head Changes in Catchment Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#  - Cut out the modeled data from gap-filling???
#  - Interesting swings in upland wells due to specific yield?

# 1.0 Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(RColorBrewer)
library(cowplot)
library(stringr)
library(lubridate)
library(tidyverse)

data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

# 2.0 Read in the data ---------------------------------------------------------------------

df <- read_csv(paste0(data_dir, "Rel_wtr_lvls.csv"))

JL_head_diffs <- read_csv(paste0(data_dir, "JL_head_diffs.csv"))

BC_head_diffs <- read_csv(paste0(data_dir, "BC_head_diffs.csv"))


# 2.1 Aggregated water level plots  -------------------------------------------------

#Make a quick plot with aggregated water level at JL

JL_aggregate_wtrlvl <- ggplot(data = JL_head_diffs %>% filter(Site_IDs == "dly_mean_wtrlvl_allsites"),
                              aes(x = Date,
                                  y = Head_diff_m)) +
  geom_line(color = "black",
            size = 2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16,
                                   face = "bold"),
        plot.title = element_text(size = 56, 
                                  face = "bold")) +
  ggtitle("Aggregated Water Levels All JL Sites")

(JL_aggregate_wtrlvl)

#Make a quick plot with aggregated water level at BC

BC_aggregate_wtrlvl <- ggplot(data = BC_head_diffs %>% filter(Site_IDs == "dly_mean_wtrlvl_allsites"),
                              aes(x = Date,
                                  y = Head_diff_m)) +
  geom_line(color = "black",
            size = 2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16,
                                   face = "bold"),
        plot.title = element_text(size = 56, 
                                  face = "bold")) +
  ggtitle("Aggregated Water Levels All BC Sites")

(BC_aggregate_wtrlvl)

rm(JL_aggregate_wtrlvl, BC_aggregate_wtrlvl)

# 3.0 Time Series of Heads -------------------------------------

#Quick time series ggplot function might be helpful later
ts_quick_plot <- function(data, y_var, color_var, title) {
  
  #This step adds na's to missing timesteps.
  #Prevents geom_line from arbitrarily drawing lines between data gaps. 
  data <- data %>%
    pivot_wider(id_cols = Date,
                names_from = {{color_var}},
                values_from = {{y_var}}) %>%
    pivot_longer(cols = -c(Date),
                 names_to = "Sites",
                 values_to = "meters")


  #Done with data wrangling, time to plot!!!
  ts_plot <- ggplot(data = data, 
                    aes(x = Date,
                        y = meters,
                        color = Sites)) +
    geom_line(size = 2,
              na.rm = TRUE) +
    geom_hline(yintercept = 0,
               color = "#660000",
               size = 2) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 28),
          axis.text = element_text(size = 18,
                                   face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.text = element_text(size = 14,
                                     face = "bold")) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    ggtitle({{title}})
  
  return(ts_plot)
  (ts_plot)
  
}

# 3.1 Jackson Lane Elevation Heads Time series --------------------------------------------------------

#Look at time series of head gradients relative to outlets. 
JL_SW_CH_elheads <- ts_quick_plot(data = df %>% 
                                    #Filter data with full well installations
                                    filter(#Filter sites of interest
                                      Site_ID %in% c("DK-SW", "TS-SW", "TS-CH", 
                                                     "BD-CH", "BD-SW", "DK-CH",
                                                     "ND-SW")), 
                                  
                                  y_var = Wtrlvl_rel_datum,
                                  color_var = Site_ID,
                                  title = "SW & CH elevation heads (m) relative to datum at DK-SW")
#Print plot
(JL_SW_CH_elheads)

#Upland wells
JL_UW_elheads <- ts_quick_plot(data = df %>% 
                                 filter(#Filter UW sites
                                   Catchment == "Jackson Lane",
                                   site_type %in% c("UW1", "UW2", "UW3"),
                                   !(Site_ID == "TS-UW1" &
                                       Wtrlvl_rel_datum <= 0.045)),
                               y_var = Wtrlvl_rel_datum,
                               color_var = Site_ID, 
                               title = "GW elevation heads (m) relative to datum at DK-SW")

#Print the plot
(JL_UW_elheads)

# 3.2 Baltimore Corner Elevation Head Time series ----------------------------------------------------

BC_SW_CH_elheads <- ts_quick_plot(data = df %>% 
                                    filter(Site_ID %in% c("TP-CH", "HB-SW", "MB-SW",
                                                          "XB-SW", "OB-SW", "MB-CH",
                                                          "OB-CH", "HB-CH")), 
                                  y_var = Wtrlvl_rel_datum,
                                  color_var = Site_ID, 
                                  title = "SW & CH elevation heads (m) relative to datum at TP-CH")

#Print the plot
(BC_SW_CH_elheads)

BC_UW_elheads <- ts_quick_plot(data = df %>% 
                                 filter(Catchment == "Baltimore Corner",
                                        Site_ID == "TP-CH" |
                                          site_type == "UW1"), 
                               y_var = Wtrlvl_rel_datum,
                               color_var = Site_ID, 
                               title = "GW elevation heads (m) relative to datum at TP-CH")
#Print the plot
(BC_UW_elheads)

#Clean up environment
rm(BC_SW_CH_elheads, BC_UW_elheads, JL_SW_CH_elheads, JL_UW_elheads)



# 3.4 JL Head Difference Timeseries ------------------------------------------

#North Dog Bone Individually
ND_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("NDSW_DKSW", "NDSW_NDUW1", "NDSW_NDUW2", 
                                                      "NDSW_NDUW3", "NDSW_TSUW1")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between ND-SW & Adjacent Wells")
#View the plot
(ND_heads_ts)

#Treestand Individually
TS_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("TSSW_DKSW", "TSSW_TSUW1", "TSSW_BDCH", 
                                                      "TSSW_TSCH", "TSSW_BDSW", "TSSW_NDSW",
                                                      "TSSW_NDUW3")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between TS-SW & Adjacent Wells ")

#View the plot
(TS_heads_ts)

#Dark Bay Individually
DK_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("DKSW_DKCH", "DKSW_DKUW1", "DKSW_DKUW2", 
                                                      "DKSW_TSCH")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between DK, Upland Wells & Channels")
#View plot 
(DK_heads_ts)

JLSW_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("TSSW_DKSW", "NDSW_DKSW", "BDSW_DKSW",
                                                      "dly_mean_wtrlvl_allsites")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between DK-SW and other Jackson Lane SW sites")
#View plot
(JLSW_heads_ts)

rm(TS_heads_ts, ND_heads_ts, DK_heads_ts, JLSW_heads_ts)


# 3.5 BC Head Diff Time Series Plots ---------------------------------------




# 4.0 See correlations between head gradients and water levels ------------

# 4.1 Baltimore Corner Correlations ----------------------------------------------------



# 4.2 Jackson Lane Correlations --------------------------------------------------------

JL_heads_wide <- JL_head_diffs %>% 
  pivot_wider(id_cols = "Date", names_from = "Site_IDs", values_from = "Head_diff_m")

JL_corrs <- JL_heads_wide %>% 
  pivot_longer(cols = -c("Date", "dly_mean_wtrlvl_allsites"),
               names_to = "Site_IDs",
               values_to = "Head_diff_m")

corrs <- ggplot(data = JL_corrs %>% 
                 filter(Site_IDs %in% c("TSSW_DKSW", 
                                        "NDSW_DKSW", 
                                        "BDSW_DKSW")),
               mapping = aes(x = dly_mean_wtrlvl_allsites, 
                             y = Head_diff_m)) +
  geom_point() +
  theme_bw() +
  facet_wrap(vars(Site_IDs))


(corrs)




