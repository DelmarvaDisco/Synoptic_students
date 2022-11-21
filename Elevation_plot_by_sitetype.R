#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Elevation by site type
# Coder: James Maze
# Date: November 2022
# Purpose: Plot distributions of SW, CH and GW elevations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Libraries and data ---------------------------------------------------

remove(list = ls())

library(cowplot)
library(RColorBrewer)
library(lubridate)
library(tidyverse)


data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

rel_wtr_lvl <- read_csv(paste0(data_dir, "rel_wtr_lvls.csv"))


# 2. Make a data table for plotting ---------------------------------------

temp <- rel_wtr_lvl %>% 
  select(c(Catchment, Elevation_m, Site_ID, well_type)) %>% 
  unique()


# 3. Plot the data --------------------------------------------------------

Plot_elevation_by_site_type <- ggplot(data = rel_wtr_lvl, 
                                      mapping = aes(x = well_type, 
                                                    y = Elevation_m,
                                                    color = Catchment)) +
  geom_boxplot() +
  theme_bw() +
  scale_color_brewer(palette = "Set2")

(Plot_elevation_by_site_type)
