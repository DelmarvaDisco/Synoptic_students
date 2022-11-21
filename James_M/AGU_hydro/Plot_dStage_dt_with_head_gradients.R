#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Stage vs. Hydrograph Direction
# Coder: James Maze
# Date: November 2022
# Purpose: Analyze whether dStage/dt has an impact on head gradients. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# -


# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(broom)
library(cowplot)
library(RColorBrewer)
library(lubridate)
library(tidyverse)


data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

# 2. Read the data --------------------------------------------------------

rel_wtr_lvl <- read_csv(paste0(data_dir, "rel_wtr_lvls.csv"))

hydro_heads <- read_csv(paste0(data_dir, "hydro_heads.csv"))


# 6.0 Boxplots based on rising/falling/neutral hydrograph ------------------------------

catchment_scale_head_list <- c("BDSW_DKSW", "NDSW_DKSW",
                               "OBSW_HBSW", "XBSW_HBSW",
                               "NDUW1_DKUW2", "OBUW1_MBUW1",
                               "OBCH_HBCH", "XBCH_HBCH",
                               "BDCH_DKCH", "TSUW1_DKUW2",
                               "OBUW1_HBUW1", "TSCH_DKCH")

#Select catchment scale gradients
temp <- hydro_heads %>% 
  filter(Site_IDs %in% catchment_scale_head_list) 


# 6.1 Calculate dStage/dt metrics -----------------------------------------

temp_dS_dt <- temp %>% 
  dplyr::select(c(Catchment, Date, dly_mean_wtrlvl_allsites)) %>% 
  #Need to pivot data wider to deal with data gaps.
  #Also wide data makes calculations more straightforward.
  pivot_wider(names_from = "Catchment", 
              values_from = "dly_mean_wtrlvl_allsites",
              #Since there are duplicate values across site_ID pairs
              values_fn = mean) %>% 
  pivot_longer(cols = c("Jackson Lane", "Baltimore Corner"),
               names_to = "Catchment",
               values_to = "dly_mean_wtrlvl_allsites") 

#??? There should be an easier way to calculate these???
#Calculate dS/dt for Jackson Lane
JLtemp_dS_dt <- temp_dS_dt %>% 
  filter(Catchment == "Jackson Lane") %>% 
  mutate(lag3_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 3),
         lag5_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 5),
         lag10_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 10),
         lag15_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 15),
         lag25_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 25)) %>%  
  mutate(lag3_dStage_dt = ((dly_mean_wtrlvl_allsites - lag3_agg_wtrlvl)/3),
         lag5_dStage_dt = ((dly_mean_wtrlvl_allsites - lag5_agg_wtrlvl)/5),
         lag10_dStage_dt = ((dly_mean_wtrlvl_allsites - lag10_agg_wtrlvl)/10),
         lag15_dStage_dt = ((dly_mean_wtrlvl_allsites - lag15_agg_wtrlvl)/15),
         lag25_dStage_dt = ((dly_mean_wtrlvl_allsites - lag25_agg_wtrlvl)/25))

#Calculate dS/dt for Baltimore Corner
BCtemp_dS_dt <- temp_dS_dt %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  mutate(lag3_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 3),
         lag5_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 5),
         lag10_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 10),
         lag15_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 15),
         lag25_agg_wtrlvl = lag(dly_mean_wtrlvl_allsites, n = 25)) %>%  
  mutate(lag3_dStage_dt = ((dly_mean_wtrlvl_allsites - lag3_agg_wtrlvl)/3),
         lag5_dStage_dt = ((dly_mean_wtrlvl_allsites - lag5_agg_wtrlvl)/5),
         lag10_dStage_dt = ((dly_mean_wtrlvl_allsites - lag10_agg_wtrlvl)/10),
         lag15_dStage_dt = ((dly_mean_wtrlvl_allsites - lag15_agg_wtrlvl)/15),
         lag25_dStage_dt = ((dly_mean_wtrlvl_allsites - lag25_agg_wtrlvl)/25))

#Combine data with dS/dt calculations
temp_dS_dt <- rbind(JLtemp_dS_dt, BCtemp_dS_dt) #%>% 
  #Don't need this column
  # dplyr::select(-c(dly_mean_wtrlvl_allsites, lag3_agg_wtrlvl, lag5_agg_wtrlvl, lag10_agg_wtrlvl, 
  #                  lag15_agg_wtrlvl, lag25_agg_wtrlvl))

temp_dS_dt <- temp_dS_dt %>% 
  dplyr::select(-c(dly_mean_wtrlvl_allsites))

#Join dS/dt data to head gradient data
temp1 <- temp %>% 
  left_join(., temp_dS_dt, by = c("Catchment", "Date"))

rm(JLtemp_dS_dt, BCtemp_dS_dt)

# 6.2 Plot distributions of dStage/dt -------------------------------------

# temp_dS_dt_p <- temp_dS_dt %>%
#   pivot_longer(cols = c("lag3_dStage_dt","lag5_dStage_dt", "lag10_dStage_dt",
#                         "lag15_dStage_dt", "lag25_dStage_dt"),
#                names_to = "metric",
#                values_to = "value") %>%
#   mutate(metric = recode(metric,
#                          `lag3_dStage_dt` = "A. Lag 3 Day",
#                          `lag5_dStage_dt` = "B. Lag 5 Day",
#                          `lag10_dStage_dt`= "C. Lag 10 Day",
#                          `lag15_dStage_dt`= "D. Lag 15 Day",
#                          `lag25_dStage_dt`= "E. Lag 25 Day"))
# 
# 
# #Look at desity plots to evaluate distribution of stage changes for analysis
# dStage_dt_density_plot <- ggplot(data = temp_dS_dt_p %>%
#                                    filter(value <= 0.05),
#                                  mapping = aes(x = value,
#                                                color = Catchment)) +
#   geom_density(size = 2) +
#   theme_bw() +
#   xlab("dStage/dt value in (m/d)") +
#   geom_rect(xmin = -Inf, xmax = -0.01, ymin = 0, ymax = 150,
#             alpha = 0.005, color = "black", fill = "orange") +
#   geom_rect(xmin = -0.01, xmax = 0.01, ymin = 0, ymax = 150,
#             alpha = 0.005, color = "black", fill = "grey20") +
#   geom_rect(xmin = 0.01, xmax = 20, ymin = 0, ymax = 150,
#             alpha = 0.005, color = "black", fill = "green") +
#   scale_color_brewer(palette = "Set2") +
#   theme(legend.position = "bottom") +
#   facet_wrap(vars(metric))
# 
# #Print and save the plot
# (dStage_dt_density_plot)
# 
# ggsave("dStage_dt_density_plot.png",
#        plot = dStage_dt_density_plot,
#        path = paste0(plot_dir))
# 
# #Clean up environment
# rm(dStage_dt_density_plot, temp_dS_dt_p, temp_dS_dt,
#    BCtemp_dS_dt, JLtemp_dS_dt)

# 6.3 Classify rising, falling and neutral hydrographs---------------------------------------------------------------------

upper <- 0.005
lower <- -0.005

temp2 <- temp1 %>% 
  #Create new columns for hydrograph regime based on dStage/dt
  mutate(hydro_regime_3lag = if_else(lag3_dStage_dt < lower,
                                     "Falling",
                                     if_else(lag3_dStage_dt >= lower & lag3_dStage_dt <= upper,
                                             "Neutral",
                                             if_else(lag3_dStage_dt > upper,
                                                     "Rising",
                                                     "NA"))),
          hydro_regime_5lag = if_else(lag5_dStage_dt < lower,
                                           "Falling",
                                           if_else(lag5_dStage_dt >= lower & lag5_dStage_dt <= upper,
                                                   "Neutral",
                                                   if_else(lag5_dStage_dt > upper,
                                                           "Rising",
                                                           "NA"))),
         hydro_regime_10lag = if_else(lag10_dStage_dt < lower,
                                      "Falling",
                                      if_else(lag10_dStage_dt >= lower & lag10_dStage_dt <= upper,
                                              "Neutral",
                                              if_else(lag10_dStage_dt > upper,
                                                      "Rising",
                                                      "NA"))),
         hydro_regime_15lag = if_else(lag15_dStage_dt < lower,
                                      "Falling",
                                      if_else(lag15_dStage_dt >= lower & lag15_dStage_dt <= upper,
                                              "Neutral",
                                              if_else(lag15_dStage_dt > upper,
                                                      "Rising",
                                                      "NA"))),
         hydro_regime_25lag = if_else(lag25_dStage_dt < lower,
                                      "Falling",
                                      if_else(lag25_dStage_dt >= lower & lag25_dStage_dt <= upper,
                                              "Neutral",
                                              if_else(lag25_dStage_dt > upper,
                                                      "Rising",
                                                      "NA")))) 

temp3 <- temp2 %>% 
  dplyr::select(-c(elevation_gradient, Head_diff_m, distance_m, elevation_diff_m)) %>% 
  pivot_longer(cols = c("lag3_dStage_dt", "lag5_dStage_dt", "lag10_dStage_dt", "lag15_dStage_dt", "lag25_dStage_dt"),
               names_to = "lag_window_num",
               values_to = "dStage_dt") %>% 
  pivot_longer(cols = c("hydro_regime_3lag", "hydro_regime_5lag", "hydro_regime_10lag", "hydro_regime_15lag", "hydro_regime_25lag"),
               names_to = "lag_window_regime", 
               values_to = "hydro_regime") %>% 
  filter(!is.na(hydro_regime)) %>% 
  unique() %>%  
  mutate(Relationship = if_else(str_detect(Site_IDs, "UW"),
                                "Upland to Upland",
                                if_else(str_detect(Site_IDs, "CH"),
                                        "Channel to Channel",
                                        "Wetland to Wetland"))) 



# 6.4 Plot timeseries of dS/dt for each catchment -----------------------------
# 
# JL_dStage_dt_timeseries <- ggplot(data = temp1 %>% 
#                                     filter(Catchment == "Jackson Lane"), 
#                                   mapping = aes(x = Date,
#                                                 y = dStage_dt,
#                                                 color = lag_window)) +
#   geom_line() +
#   geom_hline(yintercept = 0, color = "black") +
#   geom_hline(yintercept = -0.005, color = "red") +
#   geom_hline(yintercept = 0.005, color = "red") +
#   ylab("Jackson Lane dStage/dt (m/day)") +
#   theme_bw()
# 
# #Print plot
# (JL_dStage_dt_timeseries)
# 
# #!!!Wrangling to keep ggplot from arbitrarily drawing lines between missing data
# temp2 <- temp1 %>% 
#   filter(Catchment == "Baltimore Corner") %>% 
#   mutate(dStage_dt = as.character(dStage_dt)) %>% 
#   mutate(dStage_dt = if_else(("2021-11-17" >= Date & Date >= "2021-10-21"),
#                              "NA",
#                              dStage_dt)) %>% 
#   mutate(dStage_dt = as.numeric(dStage_dt))
# 
# BC_dStage_dt_timeseries <- ggplot(data = temp2,
#                                   mapping = aes(x = Date,
#                                                 y = dStage_dt,
#                                                 color = lag_window)) +
#   geom_line() +
#   geom_hline(yintercept = 0, color = "black") +
#   geom_hline(yintercept = -0.005, color = "red") +
#   geom_hline(yintercept = 0.005, color = "red") +
#   ylab("Baltimore Corner dStage/dt (m/day)") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_blank()) 
# 
# #Print plot
# (BC_dStage_dt_timeseries)
# 
# rm(temp2)
# 
# #!!!Wrangling to keep ggplot from arbitrarily drawing lines between missing data
# temp3 <- temp1 %>% 
#   mutate(dly_mean_wtrlvl_allsites = as.character(dly_mean_wtrlvl_allsites)) %>% 
#   mutate(dly_mean_wtrlvl_allsites = if_else(condition = (Catchment == "Baltimore Corner" &
#                                                            "2021-11-17" >= Date & 
#                                                            Date >= "2021-10-21"),
#                                             "NA",
#                                             dly_mean_wtrlvl_allsites)) %>% 
#   mutate(dly_mean_wtrlvl_allsites = as.numeric(dly_mean_wtrlvl_allsites))
# 
# Stage_timeseries <- ggplot(data = temp3,
#                            mapping = aes(x = Date, 
#                                          y = dly_mean_wtrlvl_allsites,
#                                          color = Catchment)) +
#   geom_line() +
#   theme_bw() +
#   ylab("Aggregated water level (m) all sites") +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_blank()) 
# 
# #Print the plot
# (Stage_timeseries)
# 
# rm(temp3)
# 
# dStage_dt_timeseries <- plot_grid(Stage_timeseries,
#                                   BC_dStage_dt_timeseries,
#                                   JL_dStage_dt_timeseries,
#                                   ncol = 1, axis = "b", align = "v")
# #View the combination
# (dStage_dt_timeseries)
# 
# rm(dStage_dt_timeseries, JL_dStage_dt_timeseries, 
#    BC_dStage_dt_timeseries, Stage_timeseries)

# 6.5 Box plots on hydrograph state vs head gradient ----------------------

temp_boxplot <- temp3 %>% 
  filter(lag_window_regime %in% 
           c("hydro_regime_3lag", "hydro_regime_10lag", "hydro_regime_25lag")) %>% 
  filter(head_gradient >= -0.0025 
         & head_gradient <= 0.0100) %>% 
  #Make head-gradients more readable
  mutate(head_gradient_cm_m = head_gradient * 100) %>% 
  #Recode and rename variables for the plot
  mutate(hydro_regime = recode(hydro_regime,
                               `Falling` = "Falling (< -1cm/d)",
                               `Neutral` = "Neutral",
                               `Rising` = "Rising (> 1cm/d)"),
         lag_window_regime = recode(lag_window_regime,
                                    `hydro_regime_3lag` = "3-day Lookback Window",
                                    `hydro_regime_10lag` = "10-day Lookback Window", 
                                    `hydro_regime_25lag` = "25-day Lookback Window")) %>% 
  mutate(lag_window_regime = factor(lag_window_regime,
                                    levels = c("3-day Lookback Window", 
                                               "10-day Lookback Window", 
                                               "25-day Lookback Window")))


gradient_hydro_regime_box <- ggplot(data = temp_boxplot,
                                    mapping = aes(x = hydro_regime,
                                                  y = head_gradient_cm_m,
                                                  color = lag_window_regime)) +
  geom_boxplot() +
  ylab("dh/dL (cm/m)") +
  xlab("Hydrograph Direction") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  facet_wrap(vars(Relationship),
             scales = "fixed") +
  scale_color_brewer(palette = "Accent")

(gradient_hydro_regime_box)



