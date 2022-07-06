#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Stage SpC data exploration
# Coder: James Maze
# Date: 8 June 2022
# Purpose: Looking for relationships between stage and SC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Libraries and workspace ----------------------------------------------

remove(list = ls())

library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)

data_dir <- "data/Stage_SpC/"

# 2. Read in the data -----------------------------------------------------

Stage_data <- read_csv(paste0(data_dir, "dly_mean_output_JM_2019_2022.csv")) %>% 
  rename("Site_ID" = Site_Name)

SpC_data <- read_csv(file = paste0(data_dir, "SpC_output_2021_2022.csv")) %>% 
  mutate(Date = ymd(str_sub(Timestamp, 1, 10))) %>% 
  group_by(Date, Site_ID) %>% 
  summarize("SpC_low_range_dly" = mean(SpC_low_range),
            "Temp_C_dly" = mean(Temp_C))

# 3. Match the SpC data to Stage ------------------------------------------

#Join data
df <- full_join(SpC_data, Stage_data, by = c("Site_ID", "Date"))

#See SpC values missing stage values. Hopefully bring this number down overtime.
count_stage_nas <- df %>% filter(is.na(dly_mean_wtrlvl))

rm(SpC_data, Stage_data, count_stage_nas)

# 4. Plot relationships ---------------------------------------------------


# 4.1 Stage and SC at SW sites --------------------------------------------

temp <- df %>% 
  #Filter out the channel sites until further data QAQC descisions
  filter(Site_ID %in% c("XB-SW", "OB-SW", "MB-SW", "HB-SW", 
                         "TS-SW", "DK-SW", "ND-SW"))

#Stage SpC relationships across sites
stage_SpC <- ggplot(data = temp, 
                    mapping = aes(x = dly_mean_wtrlvl,
                                  y = SpC_low_range_dly,
                                  color = Site_ID)) +
  geom_point() +
  scale_y_continuous(name = "SpC dly mean (uS/cm)",
                     limits = c(0, 100)) +
  scale_x_continuous(name = "Stage dly mean (m)",
                     limits = c(0, 1.1)) +
  ggtitle("Correlations between Stage (m) and SpC (uS/cm)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

(stage_SpC)

# 4.2 Stage and SpC overtime at Baltimore Corner ---------------------------

temp <- df %>% 
  #Filter only BC surface water sites
  filter(Site_ID %in% c("XB-SW", "OB-SW", "MB-SW", "HB-SW"))

BC_stage_SpC <- ggplot(data = temp, 
                       aes(x = Date,
                           color = Site_ID)) +
  geom_line(aes(y = dly_mean_wtrlvl * 100)) +
  geom_point(aes(y = SpC_low_range_dly)) +
  geom_hline(yintercept = 0,
             color = "black",
             size = 2) +
  scale_y_continuous(name = "Daily SpC (uS/cm)",
                     sec.axis = sec_axis(~./100, name = "Daily stage (m)"),
                     limits = c(-15, 150)) +
  scale_x_date(limits = as.Date(c("2021-09-21", "2022-05-02"))) +
  theme_bw() +
  ggtitle("Stage (lines) & SpC (dots) at BC
          - filling stage gaps (dead PT batteries) with correlations 
          - SpC gaps from winter sensor pull") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

(BC_stage_SpC)







