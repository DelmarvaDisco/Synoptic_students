#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Exploratory plots
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
library(broom)
library(ggrepel)
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

JL_aggregate_wtrlvl <- ggplot(data = JL_head_diffs,
                              aes(x = Date,
                                  y = dly_mean_wtrlvl_allsites)) +
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

BC_aggregate_wtrlvl <- ggplot(data = BC_head_diffs,
                              aes(x = Date,
                                  y = dly_mean_wtrlvl_allsites)) +
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

#Clean up workspace

rm(JL_aggregate_wtrlvl, BC_aggregate_wtrlvl)

# 3.0 Plotting function for time series data -------------------------------------

#Quick time series ggplot function might be helpful later
ts_quick_plot <- function(data, y_var, color_var, title) {
  
  #This step adds na's to missing timesteps.
  #Prevents geom_line from arbitrarily drawing lines between data gaps. 
  ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-05-01"), by = "day")
  
  ts <- format.POSIXct(ts, "%Y-%m-%d")
  
  ts <- data.frame(Date = ymd(ts))
  
  data <- full_join(ts, data)
  
  data <- {{data}} %>%
    pivot_wider(id_cols = c(Date, dly_mean_wtrlvl_allsites),
                names_from = {{color_var}},
                values_from = {{y_var}}) %>%
    select(-c("NA")) %>% 
    pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
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
  
  return(data)
  
  (ts_plot)
  
}

# 3.1 Jackson Lane elevation heads time series --------------------------------------------------------

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

rm(JL_SW_CH_elheads, JL_UW_elheads)

# 3.2 All JL sites colored by well type -----------------------------------

#Quick plot function won't work here
JL_all_eheads <- ggplot(data = df %>% filter(Catchment == "Jackson Lane"),
                        mapping = aes(x = Date, 
                                      y = Wtrlvl_rel_datum,
                                      shape = Site_ID,
                                      color = well_type)) +
  geom_line(size = 1) +
  geom_label_repel(data = df %>% filter(Date == max(Date)),
                   aes(label = Site_ID),
                   min.segment.length = 0.05,
                   direction = "y") +
  geom_label_repel(data = df %>% 
                     filter(Catchment == "Jackson Lane") %>% 
                     filter(Date == median(Date)),
                   aes(label = Site_ID),
                   min.segment.length = 0.05,
                   direction = "y") +
  geom_label_repel(data = df %>% 
                     filter(Catchment == "Jackson Lane") %>% 
                     filter(Date == "2021-04-15 12:00:00"),
                   aes(label = Site_ID),
                   min.segment.length = 0.05,
                   direction = "y") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 28),
        axis.text = element_text(size = 18,
                                 face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14,
                                   face = "bold")) +
  ggtitle("All Elevation Heads at Jackson Lane by Well Type") +
  ylab("Wtrlvl Relative to DK-SW Bottom (m)")

#View plot
(JL_all_eheads)

rm(JL_all_eheads)

# 3.3 Baltimore corner elevation head time series ----------------------------------------------------

BC_SW_CH_elheads <- ts_quick_plot(data = df %>% 
                                    filter(Site_ID %in% c("TP-CH", "HB-SW", "MB-SW",
                                                          "XB-SW", "OB-SW", "MB-CH",
                                                          "OB-CH", "HB-CH", "XB-CH")), 
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
rm(BC_SW_CH_elheads, BC_UW_elheads)

# 3.4 All BC heads by well type -------------------------------------------

BC_all_eheads <- ggplot(data = df %>% filter(Catchment == "Baltimore Corner"),
                        mapping = aes(x = Date, 
                                      y = Wtrlvl_rel_datum,
                                      shape = Site_ID,
                                      color = well_type)) +
  geom_line(size = 1) +
  geom_label_repel(data = df %>% 
                     filter(Catchment == "Baltimore Corner") %>% 
                     filter(Date == max(Date)),
                   aes(label = Site_ID),
                   min.segment.length = 0.05,
                   direction = "y") +
  geom_label_repel(data = df %>% 
                     filter(Catchment == "Baltimore Corner") %>% 
                     filter(Date == median(Date)),
                   aes(label = Site_ID),
                   min.segment.length = 0.05,
                   direction = "y") +
  geom_label_repel(data = df %>% 
                     filter(Catchment == "Baltimore Corner") %>% 
                     filter(Date == "2021-03-15 12:00:00"),
                   aes(label = Site_ID),
                   min.segment.length = 0.05,
                   direction = "y") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 28),
        axis.text = element_text(size = 18,
                                 face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14,
                                   face = "bold")) +
  ggtitle("All Elevation Heads at Baltimore Corner by Well Type") +
  ylab("Wtrlvl Relative to TP-CH Bottom (m)")

(BC_all_eheads)

#Clean up environment
rm(BC_all_eheads)

# 4.0 Head difference time series ----------------------------------------------
# 4.1 JL Head difference time series ------------------------------------------

#Baby Doll Individually
BD_head_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                              filter(Site_IDs %in% c("BDSW_DKSW", "BDSW_BDCH", "BDSW_TSSW",
                                                     "BDSW_TSUW1")),
                            y_var = Head_diff_m,
                            color_var = Site_IDs,
                            title = "Head differences between BD-SW & Adjacent Wells")

#View plot
(BD_head_ts)

#North Dog Bone Individually
ND_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("NDSW_NDUW1", "NDSW_NDUW2", 
                                                      "NDSW_NDUW3", "NDSW_TSUW1", "NDUW1_NDUW2")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between ND-SW & Adjacent Wells")
#View the plot
(ND_heads_ts)

#Treestand Individually
TS_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("TSSW_TSUW1", "TSSW_BDCH", "TSSW_TSCH", 
                                                      "TSSW_DKUW1", "TSSW_DKSW")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between TS-SW & Adjacent Wells")

#View the plot
(TS_heads_ts)

#Dark Bay Individually
DK_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("DKSW_DKCH", "DKSW_DKUW1", "DKSW_DKUW2", 
                                                      "DKSW_TSCH")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between DK-SW & adjacent sites")
#View plot 
(DK_heads_ts)

#Surface water head differences. 
JLSW_heads_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("TSSW_DKSW", "NDSW_DKSW", "BDSW_TSSW", "BDSW_DKSW")),
                             y_var = Head_diff_m, 
                             color_var = Site_IDs,
                             title = "Head differences between DK-SW and other Jackson Lane SW sites")
#View plot
(JLSW_heads_ts)

#Upland well head differences. 
JLUW_head_ts <- ts_quick_plot(data = JL_head_diffs %>% 
                                filter(Site_IDs %in% c("DKUW2_DKUW1", "DKUW2_TSUW1", "DKUW2_NDUW3", 
                                                       "DKUW2_NDUW2", "DKUW2_NDUW1", "DKUW2_TSCH", "DKUW2_BDCH")),
                              y_var = Head_diff_m, 
                              color_var = Site_IDs, 
                              title = "Head diff btwn DK-UW2 and other UW & CH sites")

(JLUW_head_ts)

rm(TS_heads_ts, ND_heads_ts, DK_heads_ts, JLSW_heads_ts, JLUW_head_ts, BD_head_ts)


# 4.2 BC head diff time series plots ---------------------------------------

OB_SW_head_ts <- ts_quick_plot(data = BC_head_diffs %>% 
                                 filter(Site_IDs %in% c("OBSW_OBUW1", "OBSW_OBCH", "OBSW_MBSW",
                                                        "OBSW_MBUW1", "OBSW_HBSW")),
                               y_var = Head_diff_m, 
                               color_var = Site_IDs, 
                               title = "Head diff btwn OB-SW and adjacent wells")
#View the plot 
(OB_SW_head_ts)

MB_SW_head_ts <- ts_quick_plot(data = BC_head_diffs %>% 
                                 filter(Site_IDs %in% c("MBSW_MBCH", "MBSW_MBUW1", "MBSW_OBCH", 
                                                        "MBSW_XBSW", "MBSW_HBSW", "MBSW_XBUW1")),
                               y_var = Head_diff_m,
                               color_var = Site_IDs,
                               title = "Head diff btwn MB-SW and adjacent wells")

#view
(MB_SW_head_ts)


#Clean up workspace
rm(OB_SW_head_ts, MB_SW_head_ts)

  
# 5.0 See correlations between head gradients and water levels ------------

#Read in the head relationships file
Head_relationships <- read_xlsx(paste0(data_dir, "Head_relationships.xlsx"))

JL_head_diffs <- left_join(JL_head_diffs, Head_relationships, by = "Site_IDs")


#Ploting function for cross-site correlations
corr_plot_fun <- function(data) {
  
  data <- {{data}} 
  
  models <- data %>% 
    group_by(Site_IDs) %>% 
    nest() %>% 
    mutate(gradient_models = map(.x = data, 
                                 ~lm(gradient ~ dly_mean_wtrlvl_allsites,
                                     data = .x) %>% 
                                   tidy())) %>% 
    unnest(gradient_models) %>% 
    select(-c(data)) %>% 
    as_tibble() %>% 
    filter(term == "dly_mean_wtrlvl_allsites")
  
  stats <- data %>% 
    group_by(Site_IDs) %>% 
    nest() %>% 
    mutate(gradient_stats = map(.x = data, 
                                ~lm(gradient ~ dly_mean_wtrlvl_allsites, 
                                    data = .x) %>% 
                                  glance())) %>%
    unnest(gradient_stats) %>% 
    select(-c(data)) %>% 
    as_tibble()
  
  data <- data %>% 
    mutate(month_yr = as.numeric(str_sub(Date, 6, 7)))
  
  corr_plot <- ggplot() +
    geom_point(data = data,
               mapping = aes(x = dly_mean_wtrlvl_allsites,
                             y = gradient,
                             color = month_yr)) +
    geom_text(data = stats,
               aes(label = paste0("r^2 = ", round(r.squared, digits = 2))),
               x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2,
               inherit.aes = FALSE,
              color = "red",
              size = 8) +
    geom_text(data = models,
               aes(label = paste0("slope = ", round(estimate, digits = 5))),
               x = -Inf, y = Inf, hjust = -0.1, vjust = 2.5,
               inherit.aes = FALSE,
              color = "red",
              size = 8) +
    geom_hline(yintercept = 0, 
               size = 2, 
               color = "#993300") +
    theme_bw() +
    xlab("Daily mean water lvl (m)") +
    ylab("dh/dL") +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 18,
                                   face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.text = element_text(size = 14,
                                     face = "bold"),
          strip.text = element_text(size = 18,
                                    face = "bold"),
          title = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    facet_wrap(vars(Site_IDs),
               scales = "free") +
    ggtitle("Aggregate water level and head gradient correlations")
  
  return(corr_plot)
  
}

# 5.1 Jackson Lane correlations ----------------------------------------------------

#Correlations between SW sites
JL_SW_corrs <- corr_plot_fun(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("BDSW_DKSW", "BDSW_TSSW", 
                                                        "NDSW_DKSW", "TSSW_DKSW")))

(JL_SW_corrs)

#Correlations between UW sites
JL_UW_corrs <- corr_plot_fun(data = JL_head_diffs %>% 
                               filter(Site_IDs %in% c("DKUW2_BDCH", "DKUW2_DKUW1", "DKUW2_NDUW1",
                                                      "DKUW2_NDUW2", "DKUW2_NDUW3", "DKUW2_TSCH", 
                                                      "DKUW2_TSUW1")))

(JL_UW_corrs)

#Correlations around BD-SW
BDSW_corrs <- corr_plot_fun(data =  JL_head_diffs %>%
                              filter(Site_IDs %in% c("BDSW_BDCH", "BDSW_DKSW", 
                                                     "BDSW_TSSW", "BDSW_TSUW1")))

(BDSW_corrs)

#Correlations around ND-SW
NDSW_corrs <- corr_plot_fun(JL_head_diffs %>% 
                               filter(Site_IDs %in% c("NDSW_NDUW1", "NDSW_NDUW2",
                                                      "NDSW_NDUW3", "NDSW_TSUW1")))

(NDSW_corrs)

#Correlations around TS-SW
TSSW_corrs <- corr_plot_fun(JL_head_diffs %>% 
                              filter(Site_IDs %in% c("TSSW_BDCH", "TSSW_DKUW1", 
                                                     "TSSW_TSCH", "TSSW_TSUW1")))

(TSSW_corrs)

#Correlations around DK-SW
DKSW_corrs <- corr_plot_fun(JL_head_diffs %>% 
                              filter(Site_IDs %in% c("DKSW_DKCH", "DKSW_DKUW1", "DKSW_DKUW2",
                                                     "DKSW_TSCH", "DKSW_BDSW")))

(DKSW_corrs)

#Clean up environment 
rm(JL_SW_corrs, JL_UW_corrs, BDSW_corrs, NDSW_corrs, TSSW_corrs, DKSW_corrs)


# 5.2 Baltimore Corner correlations --------------------------------------------------------





