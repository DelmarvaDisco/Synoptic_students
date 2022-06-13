#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Stage on sample dates
# Coder: James Maze
# Date: 25 Jan 2021
# Purpose: To enumerate hydrologic conditions durring sampling campaigns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Notes:
#   - !!! Data folder isn't on Google Drive. Reach out if you want to recreate any of this. 
#   - Need to automate sampling dates on the stage time series


# 1. Libraries and work space ----------------------------------------------

remove(list = ls())

library(readxl)
library(readr)
library(lubridate)
library(tidyverse)
library(cowplot)
library(dplyr)

data_dir <- "data/Sampledates_FR_Stage/"


# 2. Read the data --------------------------------------------------------

# Read the FR stage file 
Stage <- read_csv(paste0(data_dir, "all_data_JM.csv")) %>% 
  filter(Site_Name == "QB-SW") %>% 
  mutate(Datezzz = as.character(Date))

#Read the sampling dates file
sampling <- read_excel(paste0(data_dir, "sampling_schedule.xlsx")) %>% 
  mutate(Datezzz = as.character(Sample_date))


# 3. Join Schedule to Stage -------------------------------

#Join stage values to sample dates.
sampling <- right_join(Stage, sampling, by = "Datezzz") %>% 
  select(-Sample_date)

# 4. Make some plots ------------------------------------------------------

#Histograms of stage at sampling dates
sampling_g_synop <- sampling %>% 
  filter(Synoptic == "T")

synop_sampling_histo <- ggplot(data = sampling_g_synop,
                               mapping = aes(x = dly_mean_wtrlvl)) +
                        geom_histogram(data = Stage, 
                                       mapping = aes(x = dly_mean_wtrlvl)) +
                        geom_dotplot(color = "red",
                                     fill = "red") +
                        ggtitle("Synoptic Sampling compared to QB-SW Stage") +
                        theme(plot.title = element_text(size = 8),
                              axis.title.y = element_blank()) +
                        xlab("Stage (meters)") +
                        theme_bw()

(synop_sampling_histo)

# Same plot, but with Jackson Lane sites
sampling_g_JL <- sampling %>% 
  filter(Jackson_lane == "T")

JL_sampling_histo <- ggplot(data = sampling_g_JL, 
                            mapping = aes(x = dly_mean_wtrlvl)) +
                     geom_histogram(data = Stage, 
                                   mapping = aes(x = dly_mean_wtrlvl)) +
                     geom_dotplot(color = "blue",
                                  fill = "blue") +
                     ggtitle("JL Sampling compared to QB-SW Stage") +
                     theme(plot.title = element_text(size = 8)) +
                     xlab("Stage (meters)") +
                     theme_bw()

(JL_sampling_histo)
                        
#Don't need to make the BC histogram yet
sampling_g_BC <- sampling %>% 
  filter(Baltimore_corner == "T")

BC_sampling_histo <- ggplot(data = sampling_g_BC, 
                            mapping = aes(x = dly_mean_wtrlvl)) +
  geom_histogram(data = Stage, 
                 mapping = aes(x = dly_mean_wtrlvl)) +
  geom_dotplot(color = "green",
               fill = "green") +
  ggtitle("BC Sampling compared to QB-SW Stage") +
  theme(plot.title = element_text(size = 8)) +
  xlab("Stage (meters)") +
  theme_bw()

(BC_sampling_histo)

all_sampling <- plot_grid(BC_sampling_histo, 
                          synop_sampling_histo, 
                          JL_sampling_histo)

#Plot stage with sampling campaigns (not automated)
Stage_ts <- ggplot(data = Stage, 
       mapping = aes(x = as.POSIXct(Stage$Date), 
                     y = dly_mean_wtrlvl)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_datetime(limits = as.POSIXct(c("2020-01-01", "2022-06-01"))) +
  ### At some point, fix this manual entry
  geom_point(data = sampling_g_synop,
             mapping = aes(x = as.POSIXct(Datezzz),
                           y = dly_mean_wtrlvl), 
             size = 7,
             shape = 24,
             color = "black",
             fill = "red") +
  geom_point(data = sampling_g_JL,
             mapping = aes(x = as.POSIXct(Datezzz),
                           y = dly_mean_wtrlvl),
             size = 14,
             stroke = 1.5, 
             shape = 13,
             color = "blue") + 
  geom_point(data = sampling_g_BC,
             mapping = aes(x = as.POSIXct(Datezzz),
                           y = dly_mean_wtrlvl),
             size = 12,
             stroke = 1.5,
             shape = 3,
             color = "green") +
  ggtitle("Sampling campaigns with Stage at QB-SW")
  
(Stage_ts)






