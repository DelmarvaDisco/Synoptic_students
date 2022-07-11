#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Property Waterlevel Comp
# Coder: James Maze
# Date: 7/9/2022
# Purpose: Comparing waterLevel regimes between Jackson Lane and Baltimore Corner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Not much difference between UW at catchments, but big difference in SW.
# - Is SW differences just due to elevation, or are there drainage dynamics at play?


# 1. Libraries and workspace -----------------------------------------------

rm(list = ls())

library(tidyverse)
library(lubridate)
library(stringr)
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
  mutate(site_type = str_sub(Site_ID, 4, 5)) %>% 
  group_by(Property, site_type) %>% 
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

SW_time_plot <- ggplot() +
  geom_line(data = SW_means,
            aes(x = Date,
                y = avg, 
                color = Property)) +
  geom_ribbon(data = SW_means,
              aes(ymin = low,
                  ymax = up,
                  x = Date,
                  fill = Property,
                  alpha = 0.42)) +
  labs(y = "Average surface water level (meters)") +
  ggtitle("Mean SW water level comparing BC & JL") +
  theme_bw()

(SW_time_plot)

SW_corr <- SW_means %>% 
  select(c(avg, Property, Date)) %>% 
  pivot_wider(names_from = Property, 
              values_from = avg) %>% 
  mutate(month = as.integer(as.factor(str_sub(Date, 6, 7))))

SW_plot_corr <- ggplot(data = SW_corr,
                    mapping = aes(x = `Jackson Lane`,
                                  y = `Baltimore Corner`,
                                  color = month)) +
  geom_point() +
  theme_bw() +
  ggtitle("Correlation of SW water levels between TNC properties (Spring 2021-Spring 2022)") +
  labs(x = "Mean SW waterlevel for JL sites (meters)",
       y = "Mean SW waterlevel for BC sites (meters)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

(SW_plot_corr)

rm(SW_means, SW_corr, SW_time_plot, SW_plot_corr)

# 5. Compare UW hydrographs between properties ----------------------------------------

UW_means <- df %>% 
  filter(!str_detect(Site_ID, "SW")) %>%
  filter(Date >= "2021-03-18") %>% 
  group_by(Property, Date) %>% 
  dplyr::summarise(avg = mean(dly_mean_wtrlvl),
                   low = avg - sd(dly_mean_wtrlvl)/sqrt(n()),
                   up = avg + sd(dly_mean_wtrlvl)/sqrt(n()))

UW_plot <- ggplot() +
  geom_line(data = UW_means,
            aes(x = Date,
                y = avg, 
                color = Property)) +
  geom_ribbon(data = UW_means,
              aes(ymin = low,
                  ymax = up,
                  x = Date,
                  fill = Property,
                  alpha = 0.42)) +
  labs(y = "Average groundwater level (meters)") +
  ggtitle("Mean UW water level comparing BC & JL") +
  theme_bw()

(UW_plot)

UW_corr <- UW_means %>% 
  select(c(avg, Property, Date)) %>% 
  pivot_wider(names_from = Property, 
              values_from = avg) %>% 
  mutate(month = as.integer(as.factor(str_sub(Date, 6, 7))))

UW_plot_corr <- ggplot(data = UW_corr,
                    mapping = aes(x = `Jackson Lane`,
                                  y = `Baltimore Corner`,
                                  color = month)) +
  geom_point() +
  theme_bw() +
  ggtitle("Correlation of UW water levels between TNC properties (Spring 2021-Spring 2022)") +
  labs(x = "Mean UW waterlevel for JL sites (meters)",
       y = "Mean UW waterlevel for BC sites (meters)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

(UW_plot_corr)

rm(UW_corr, UW_means, UW_plot, UW_plot_corr)

