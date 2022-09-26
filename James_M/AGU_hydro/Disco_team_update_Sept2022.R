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

heads_list_JL <- hydro_heads %>% 
  filter(Catchment == "Jackson Lane") %>% 
  select(Site_IDs) %>% 
  unique()

# 3. Show relative water level time series ----------------------------------------------------------------------

# 3.1 Baltimore Corner SW & CH rel wtr lvl -----------------------------

#Illustrate elevation heads using Baltimore Corner
relwtrlvl_SWCH_BaltimoreCorner <- ggplot(data = rel_wtr_lvl %>% 
                                       filter(Catchment == "Baltimore Corner") %>% 
                                       filter(site_type %in% c("SW", "CH")),
                                     mapping = aes(x = Date,
                                                   y = Wtrlvl_rel_datum,
                                                   color = Site_ID)) +
  ggtitle("Water level (m) relative to Terminal Outlet bottom") +
  ylab("meters") +
  geom_line(size = 1.2) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

#Print and clean up
(relwtrlvl_SWCH_BaltimoreCorner)

rm(relwtrlvl_SWCH_BaltimoreCorner)

# 3.2 Jackson Lane UW elevation heads ------------------------------------------------------------

relwtrlvl_UW_JacksonLane <- ggplot(data = rel_wtr_lvl %>% 
                                     filter(Catchment == "Jackson Lane"), #%>% 
                                    # filter(well_type == "UW"),
                                   mapping = aes(x = Date,
                                                 y = Wtrlvl_rel_datum,
                                                 color = Site_ID)) +
  geom_line(size = 1.2) +
  ggtitle("Water level (m) relative to DK-SW wetland bottom") +
  ylab("meters") +
  theme_bw() +
  scale_color_brewer(palette = "Spectral")

(relwtrlvl_UW_JacksonLane)
rm(relwtrlvl_UW_JacksonLane)

# Correlate head gradients to mean waterlevel ------------------------------------------------------------

JL_gradient_corr_plot <- ggplot(data = hydro_heads,
                          mapping = aes(x = dly_mean_wtrlvl_allsites,
                                        y = gradient,
                                        color = elevation_m)) +
  geom_point() +
  scale_color_brewer(palette = "RdYlGn")

(JL_gradient_corr_plot)


# Scratch space -----------------------------------------------------------

display.brewer.all()
