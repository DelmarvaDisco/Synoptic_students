#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Property Waterlevel Comp
# Coder: James Maze
# Date: 7/9/2022
# Purpose: Comparing waterLevel regimes between Jackson Lane and Baltimore Corner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Should be fun!


# 1. Libraries and workspace -----------------------------------------------

rm(list = ls())

library(tidyverse)
library(lubridate)
library(dplyr)

data_dir <- "data/"


# 2. Read waterLevel and Site Directory data ---------------------------------------------------------------------

wtr_data <- read_csv(paste0(data_dir, "dly_mean_output_JM_2019_2022.csv")) %>% 
  dplyr::rename(Site_ID = Site_Name)

site_data_path <- paste0(data_dir, "Site_Directory_Core.xlsx")

sheet_names <- excel_sheets(path = site_data_path)  

sheet_names <- sheet_names[1:3] %>% 
  as.list()

Site_data <- lapply(sheet_names, 
                    function(x) read_excel(path = site_data_path, 
                                           sheet = x)) %>% 
  reduce(rbind) %>% 
  select(c(Site_ID, Catchment, Latitude, Longitude)) 

Site_data <- Site_data %>% 
  mutate(Property = if_else(str_detect(Catchment, "Baltimore Corner"),
                            "Baltimore Corner",
                            if_else(str_detect(Catchment, "Jackson Lane|Beetree Rd"),
                                               "Jackson Lane",
                                               "Not matched")))

rm(sheet_names)

# 3. Match Site Directory to waterLevel (look at counts) ----------------------------------------------------------------------

df <- left_join(wtr_data, Site_data) %>% 
  filter(Property %in% c("Jackson Lane", "Baltimore Corner"))

Site_numbers <- Site_data %>% 
  group_by(Property) %>% 
  dplyr::summarise(counts = n())

rm(Site_numbers, Site_data, wtr_data)

# 4. Compare SW hydrographs between properties ---------------------------------------------------------------------

SW_means <- df %>% 
  filter(str_detect(Site_ID, "SW")) %>%
  filter(Date >= "2021-03-18") %>% 
  group_by(Property, Date) %>% 
  dplyr::summarise(avg = mean(dly_mean_wtrlvl),
                   low = avg - sd(dly_mean_wtrlvl)/sqrt(n()),
                   up = avg + sd(dly_mean_wtrlvl)/sqrt(n()))


JL <- SW_means %>% filter(Property == "Jackson Lane")
BC <- SW_means %>% filter(Property == "Baltimore Corner")

band_alpha <- 0.5

plot <- ggplot() +
  geom_line(data = SW_means,
            aes(x = Date,
                y = avg, 
                color = Property)) +
  geom_ribbon(data = SW_means,
              aes(ymin = low,
                  ymax = up,
                  x = Date,
                  fill = Property,
                  alpha = band_alpha)) +
  theme_bw()

(plot)

