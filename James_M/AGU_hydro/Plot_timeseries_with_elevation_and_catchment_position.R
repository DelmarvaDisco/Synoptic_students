#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Delmarva Disco Catchment Hydro
# Coder: James Maze
# Date: November 2022
# Purpose: Synthesized info/update for team members
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# -


# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(broom)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(lubridate)
library(tidyverse)


data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

# 2. Read the data --------------------------------------------------------

rel_wtr_lvl <- read_csv(paste0(data_dir, "rel_wtr_lvls.csv"))

hydro_heads <- read_csv(paste0(data_dir, "hydro_heads.csv"))

# Head_relationships <- read_xlsx(paste0(data_dir, "Head_relationships.xlsx"))

# heads_list_JL <- hydro_heads %>% 
#   filter(Catchment == "Jackson Lane") %>% 
#   select(Site_IDs, elevation_gradient, elevation_diff_m) %>% 
#   unique()
# 
# heads_list_BC <- hydro_heads %>% 
#   filter(Catchment == "Jackson Lane") %>% 
#   select(Site_IDs, elevation_gradient, elevation_diff_m) %>% 
#   unique()


# 3. Show relative water level time series ----------------------------------------------------------------------

# 3.1 Baltimore Corner SW & CH rel wtr lvl -----------------------------

#Assign catchment positions for the figure. 
Site_ID_list <- c("HB-CH", "HB-SW", "MB-CH", "MB-SW", "OB-CH", 
                "OB-SW", "TP-CH", "XB-CH", "XB-SW")

catchment_pos <- c("2", "3", "4", "5", "8", "9", "1", "7", "6")

catchment_pos <- data.frame(Site_ID_list, catchment_pos) %>% 
  rename(Site_ID = Site_ID_list)

#Filter out sites of interest
temp <- rel_wtr_lvl %>%
  filter(Catchment == "Baltimore Corner") %>%
  filter(Site_ID %in% Site_ID_list)

#Need a dummy POSIXct column for every date, which 
# prevents geom_line from arbitrarily drawing lines between gaps. 
ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-11-01"), by = "day")
ts <- format.POSIXct(ts, "%Y-%m-%d")
ts <- data.frame(Date = ymd(ts))

temp <- full_join(ts, temp)

#Need to prevent ggplot from drawing lines between missing data.

#First need to pull off elevation data, add it back later.
elevation_temp <- temp %>% 
  select(c(Site_ID, Elevation_m)) %>% 
  unique()

#Pivoting between wide and long populates the data gaps with "NA"
temp <- temp %>% 
  pivot_wider(id_cols = c(Date, dly_mean_wtrlvl_allsites),
              names_from = Site_ID,
              values_from = Wtrlvl_rel_datum) %>% 
  select(-c("NA")) %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_ID",
               values_to = "Wtrlvl_rel_datum") 

#Rejoin the elevation and position attributes
temp <- left_join(temp, elevation_temp, by = "Site_ID")
temp <- left_join(temp, catchment_pos, by = "Site_ID") %>% 
  mutate(catchment_pos_Site_ID = paste0(catchment_pos, " ", Site_ID))

#Make plot colored by elevation. 
relwtrlvl_SWCH_BaltimoreCorner_elevation <- ggplot(data = temp,
                                                   mapping = aes(x = Date,
                                                                 y = Wtrlvl_rel_datum,
                                                                 color = Elevation_m)) +
  ylab("Relative water level (meters)") +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "orange") 

#Print the plot 
(relwtrlvl_SWCH_BaltimoreCorner_elevation)

#Make plot colored by Site_ID
relwtrlvl_SWCH_BaltimoreCorner_Site_ID <- ggplot(data = temp, 
                                                 mapping = aes(x = Date, 
                                                               y = Wtrlvl_rel_datum,
                                                               color = catchment_pos_Site_ID)) +
  ylab("Relative water level (meters)") +
  geom_line(size = 1) +
  theme_bw() + 
  scale_color_brewer(palette = "Spectral", direction = -1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 10, 
                                   face = "bold")) +
  labs(col = "Catchment Position") +
  ggtitle("BC wetland and chanel relative water level by Site ID (top) and Elevation (bottom)")

(relwtrlvl_SWCH_BaltimoreCorner_Site_ID)

#Combine the plots 
relwtrlvl_SWCH_BaltimoreCorner <- plot_grid(relwtrlvl_SWCH_BaltimoreCorner_Site_ID,
                                            relwtrlvl_SWCH_BaltimoreCorner_elevation,
                                            ncol = 1, axis = "b", align = "v")
#View the combination
(relwtrlvl_SWCH_BaltimoreCorner)

#Save to the plot directory
ggsave(filename = "Relative_wtrlvl_SWCH_BaltimoreCorner.png", 
       plot =  relwtrlvl_SWCH_BaltimoreCorner,
       path = paste0(plot_dir))

rm(relwtrlvl_SWCH_BaltimoreCorner, relwtrlvl_SWCH_BaltimoreCorner_Site_ID, 
   relwtrlvl_SWCH_BaltimoreCorner_elevation, ts, temp, catchment_pos, elevation_temp,
   Site_ID_list)

# 3.2 ??? Baltimore Corner UW and CH rel wtr lvl ??? ------------------------------

# 3.4 Jackson Lane UW elevation heads ------------------------------------------------------------

#Illustrate elevation head time series using Jackson Lane data
temp <- rel_wtr_lvl %>% 
  filter(Catchment == "Jackson Lane") %>% 
  filter(well_type %in% c("UW", "CH"))

#Need a dummy POSIXct column for every date, 
#which prevents geom_line from arbitrarily drawing lines between gaps. 
ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-11-01"), by = "day")
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

#Print the plot
(relwtrlvl_UW_JacksonLane)

rm(relwtrlvl_UW_JacksonLane, ts, temp)


