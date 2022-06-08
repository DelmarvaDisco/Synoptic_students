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

StageS21 <- read_csv(file = paste0(data_dir, "output_20210525_JM.csv"))
StageF21 <- read_csv(file = paste0(data_dir, "output_20211112_JM.csv"))
StageS22 <- read_csv(file = paste0(data_dir, "output_20220410_JM.csv"))

#Combine stage and rename files
Stage_data <- rbind(StageS21, StageF21, StageS22) %>% 
  rename("Site_ID" = Site_Name) %>% 
  rename("Flag_stage" = Flag) %>% 
  rename("Notes_stage" = Notes)

rm(StageS21, StageF21, StageS22)

SpC_data <- read_csv(file = paste0(data_dir, "SpC_output_2021_2022.csv"))

# 3. Match the SpC data to Stage ------------------------------------------

#Join data
df <- left_join(SpC_data, Stage_data, by = c("Site_ID", "Timestamp"))
#See SpC values missing stage values
count_stage_nas <- df %>% filter(is.na(waterLevel))

rm(SpC_data, Stage_data)

# 4. Plot relationships ---------------------------------------------------



