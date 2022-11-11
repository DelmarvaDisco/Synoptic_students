#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Delmarva Disco Catchment Hydro
# Coder: James Maze
# Date: September 23rd 2022
# Purpose: Synthesized info/update for team members
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# -


# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(tidyverse)
library(RColorBrewer)

data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

# 2. Read the data --------------------------------------------------------

rel_wtr_lvl <- read_csv(paste0(data_dir, "rel_wtr_lvls.csv"))

hydro_heads <- read_csv(paste0(data_dir, "hydro_heads.csv"))

Head_relationships <- read_xlsx(paste0(data_dir, "Head_relationships.xlsx"))

heads_list_JL <- hydro_heads %>% 
  filter(Catchment == "Jackson Lane") %>% 
  select(Site_IDs, elevation_gradient, elevation_diff_m) %>% 
  unique()

heads_list_BC <- hydro_heads %>% 
  filter(Catchment == "Jackson Lane") %>% 
  select(Site_IDs, elevation_gradient, elevation_diff_m) %>% 
  unique()


# 3. Show relative water level time series ----------------------------------------------------------------------

# 3.1 Baltimore Corner SW & CH rel wtr lvl -----------------------------

#Illustrate elevation head time series using Baltimore Corner surface and channels
temp <- rel_wtr_lvl %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  filter(site_type %in% c("SW", "CH"))

#Need a dummy POSIXct column for every date, 
#which prevents geom_line from arbitrarily drawing lines between gaps. 
ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-05-01"), by = "day")
ts <- format.POSIXct(ts, "%Y-%m-%d")
ts <- data.frame(Date = ymd(ts))

temp <- full_join(ts, temp)

#Pivoting between wide and long populates the data gaps with "NA", 
#which prevents ggplot from drawing lines between missing data.
temp <- temp %>% 
  pivot_wider(id_cols = c(Date, dly_mean_wtrlvl_allsites),
              names_from = Site_ID,
              values_from = Wtrlvl_rel_datum) %>%
  select(-c("NA")) %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_ID",
               values_to = "Wtrlvl_rel_datum")

relwtrlvl_SWCH_BaltimoreCorner <- ggplot(data = temp,
                                         mapping = aes(x = Date,
                                                   y = Wtrlvl_rel_datum,
                                                   color = Site_ID)) +
  ggtitle("Water level (m) relative to Terminal Outlet bottom") +
  ylab("meters") +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1")

#Print and clean up
(relwtrlvl_SWCH_BaltimoreCorner)

rm(relwtrlvl_SWCH_BaltimoreCorner, ts, temp)

# 3.2 Jackson Lane UW elevation heads ------------------------------------------------------------

#Illustrate elevation head timeseries using Jackson Lane data
temp <- rel_wtr_lvl %>% 
  filter(Catchment == "Jackson Lane") %>% 
  filter(well_type %in% c("UW", "CH"))

#Need a dummy POSIXct column for every date, 
#which prevents geom_line from arbitrarily drawing lines between gaps. 
ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-05-01"), by = "day")
ts <- format.POSIXct(ts, "%Y-%m-%d")
ts <- data.frame(Date = ymd(ts))

temp <- full_join(ts, temp)

#Pivoting between wide and long populates the data gaps with "NA", 
#which prevents ggplot from drawing lines between missing data.
temp <- temp %>% 
  pivot_wider(id_cols = c(Date, dly_mean_wtrlvl_allsites),
              names_from = Site_ID,
              values_from = Wtrlvl_rel_datum) %>%
  select(-c("NA")) %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_ID",
               values_to = "Wtrlvl_rel_datum")


relwtrlvl_UW_JacksonLane <- ggplot(data = temp,
                                   mapping = aes(x = Date,
                                                 y = Wtrlvl_rel_datum,
                                                 color = Site_ID)) +
  geom_line(size = 1.2) +
  ggtitle("Water level (m) relative to DK-SW wetland bottom") +
  ylab("meters") +
  theme_bw() +
  scale_color_brewer(palette = "Spectral")

(relwtrlvl_UW_JacksonLane)

rm(relwtrlvl_UW_JacksonLane, ts, temp)

# 4.0 Explore elevation gradients vs. head gradients correlation ------------------------------------------------------------

# Month <- c(1:12)
# ET_guess <- c("low", "low", "low", "medium", "high", "high", "high", "high", "high", "medium", "low", "low")
# 
# temp <- data.frame(Month, ET_guess) %>% 
#   mutate(Month = as.character(Month)) %>% 
#   as_tibble()
# 
# df <- hydro_heads %>% 
#   mutate(Month = as.character(as.numeric(str_sub(Date, 6, 7))))
# 
# df <- left_join(df, temp, by = "Month")
                
ET_elevation_head_gradient_correlation <- ggplot(data = df,
                                              mapping = aes(x = abs(elevation_gradient),
                                                            y = abs(head_gradient),
                                                            color = ET_guess)) +
  geom_point() +
  scale_color_brewer(palette = "Spectral")

(ET_elevation_head_gradient_correlation)

rm(temp)


# #Head gradients correlated with aggregate wtr lvl ---------------------------------------

# DK_head_gradients_agg_wtr_lvl <- ggplot(data = df %>% 
#                                        filter(Catchment == "Jackson Lane") %>% 
#                                        filter(Site_IDs %in% c("DKSW_DKCH", "DKSW_DKUW1", "DKSW_DKUW2",
#                                                               "DKSW_TSCH", "DKSW_BDSW")), 
#                                      mapping = aes(x = dly_mean_wtrlvl_allsites,
#                                                    y = head_gradient,
#                                                    color = ET_guess)) +
#   facet_wrap(vars(Site_IDs),
#              scales = "free") +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "tomato", size = 3) +
#   theme_bw() +
#   ylab("dh/dL") +
#   xlab("Daily mean wtrlvl (m)") +
#   scale_color_brewer(palette = "YlGnBu")
# 
# (DK_head_gradients_agg_wtr_lvl)
# rm(DK_head_gradients_agg_wtr_lvl)
# 
# #UW 
# JLUW_head_gradients_agg_wtr_lvl <- ggplot(data = df %>% 
#                                           filter(Site_IDs %in% c("DKUW2_DKUW1", "DKUW2_TSUW1", "DKUW2_NDUW3", 
#                                                                  "DKUW2_TSCH", "DKUW2_NDUW2", "DKUW2_NDUW1")), 
#                                         mapping = aes(x = dly_mean_wtrlvl_allsites,
#                                                       y = head_gradient,
#                                                       color = ET_guess)) +
#   facet_wrap(vars(Site_IDs),
#              scales = "free") +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "tomato", size = 3) +
#   theme_bw() +
#   ylab("dh/dL") +
#   xlab("Daily mean wtrlvl (m)") +
#   scale_color_brewer(palette = "YlGnBu")
# 
# (JLUW_head_gradients_agg_wtr_lvl)
# rm(JLUW_head_gradients_agg_wtr_lvl)
# 
# #SW and Channels
# JLSWCH_head_gradients_agg_wtr_lvl <- ggplot(data = df %>% 
#                                             filter(Site_IDs %in% c("DKSW_DKCH", "DKSW_TSSW", "DKSW_BDSW", 
#                                                                    "DKSW_NDSW")), 
#                                           mapping = aes(x = dly_mean_wtrlvl_allsites,
#                                                         y = head_gradient,
#                                                         color = ET_guess)) +
#   facet_wrap(vars(Site_IDs),
#              scales = "free") +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "tomato", size = 3) +
#   theme_bw() +
#   ylab("dh/dL") +
#   xlab("Daily mean wtrlvl (m)") +
#   scale_color_brewer(palette = "YlGnBu")
# 
# (JLSWCH_head_gradients_agg_wtr_lvl)
# 
# 

# 5.0 Correlation Matrix between head gradients ---------------------------



# Scratch space -----------------------------------------------------------

hmm <- hydro_heads %>% 
  filter(Catchment == "Baltimore Corner") %>% 
  mutate(head_gradient_magnitude = abs(head_gradient)) %>% 
  filter(head_gradient_magnitude <= 0.018) %>% 
  select(c(head_gradient_magnitude, Date, dly_mean_wtrlvl_allsites))


hmm_plot <- ggplot(data = hmm, 
                   mapping = aes(x = dly_mean_wtrlvl_allsites,
                                 y = head_gradient_magnitude)) +
  geom_point() +
  geom_smooth()

(hmm_plot)

hmm_model <- lm(data = hmm)



display.brewer.all()
