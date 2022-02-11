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
library(lubridate)
library(tidyverse)
library(dplyr)

data_dir <- "data/Sampledates_FR_Stage/"


# 2. Read the data --------------------------------------------------------

# Read the FR stage file 
FR_Stage <- read_excel(paste0(data_dir, "FR_Stage.xlsx"),
                       skip = 4)

#Select necessary columns from FR stage file
FR_Stage <- FR_Stage[ , 1:2]

#Rename FR_Stage columns to make coding manageable
FR_Stage <- FR_Stage %>% 
  mutate(Stage_in_FR = `Stage (in)`) %>% 
  mutate(Date_time = `Date Time, GMT-05:00`) %>% 
  select(c(Date_time, Stage_in_FR))

#Read the sampling dates file
sampling <- read_excel(paste0(data_dir, "sampling_schedule.xlsx")) %>% 
  mutate(Datezzz = as.character(Sample_date))


# 3. Join Schedule to Stage -------------------------------

# Group daily stage values from FR-SW to make the join feasible
FR_Stage_daily <- FR_Stage %>% 
  mutate(Datezzz = str_sub(as.character(Date_time), 1, 10)) %>% 
  group_by(Datezzz) %>% 
  summarize(Stage_in_FR = mean(Stage_in_FR)) 

#Join stage values to sample dates.
sampling <- right_join(FR_Stage_daily, sampling, by = "Datezzz") %>% 
  select(-Sample_date)

#Add stage estimates for last data points

#Had to make a new data frame with guesses for stage
sample_guess <- tibble(Datezzz = c("2021-10-18", "2021-11-12", "2021-12-14"),
                       Stage_in_FR = c("11.5", "14", "15"),
                       Sample_type = c("BC + JL", "Synoptic", "BC + JL"),
                       Jackson_lane = c("T", "F", "T"),
                       Synoptic = c("F", "T", "F"),
                       Baltimore_corner = c("T", "F", "T"))

sampling_g <- rbind(sampling, sample_guess) %>% 
  filter(!is.na(Stage_in_FR)) %>% 
  mutate(Stage_in_FR = as.numeric(Stage_in_FR))

rm(sample_guess, sampling)

# 4. Make some plots ------------------------------------------------------

#Histograms of stage at sampling dates
sampling_g_synop <- sampling_g %>% 
  filter(Synoptic == "T")

synop_sampling_histo <- ggplot(data = sampling_g_synop,
                               mapping = aes(x = Stage_in_FR)) +
                        geom_histogram(data = FR_Stage_daily, 
                                       mapping = aes(x = Stage_in_FR)) +
                        geom_dotplot(color = "red",
                                     fill = "red") +
                        ggtitle("Synoptic Sampling compared to FR-SW stage (Nov 2019 - Oct 2021)") +
                        theme(plot.title = element_text(size = 8)) +
                        xlab("Stage (inches)") +
                        theme_bw()

(synop_sampling_histo)

# Same plot, but with Jackson Lane sites
sampling_g_JL <- sampling_g %>% 
  filter(Jackson_lane == "T")

JL_sampling_histo <- ggplot(data = sampling_g_JL, 
                            mapping = aes(x = Stage_in_FR)) +
                     geom_histogram(data = FR_Stage_daily, 
                                   mapping = aes(x = Stage_in_FR)) +
                     geom_dotplot(color = "blue",
                     fill = "blue") +
                     ggtitle("JL Sampling compared to FR-SW stage (Nov 2019 - Oct 2021)") +
                     theme(plot.title = element_text(size = 8)) +
                     xlab("Stage (inches)") +
                     theme_bw()

(JL_sampling_histo)
                        
#Don't need to make the BC histogram yet
sampling_g_BC <- sampling_g %>% 
  filter(Baltimore_corner == "T")

#Plot stage with sampling campaigns (not automated)


FR_Stage_ts <- ggplot(data = FR_Stage, 
       mapping = aes(x = Date_time, 
                     y = Stage_in_FR)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_datetime(limits = as.POSIXct(c("2020-01-01", "2022-01-01"))) +
  ### At some point, fix this manual entry
  geom_point(data = sampling_g_synop,
             mapping = aes(x = as.POSIXct(Datezzz),
                           y = Stage_in_FR), 
             size = 5,
             shape = 24,
             color = "black",
             fill = "red") +
  geom_point(data = sampling_g_JL,
             mapping = aes(x = as.POSIXct(Datezzz),
                           y = Stage_in_FR),
             size = 14,
             shape = 13,
             color = "blue") + 
  geom_point(data = sampling_g_BC,
             mapping = aes(x = as.POSIXct(Datezzz),
                           y = Stage_in_FR),
             size = 5,
             shape = 19,
             color = "green") +
  ggtitle("Sampling events with Stage at FR-SW")
  

(FR_Stage_ts)


# 5. Other stuff ----------------------------------------------------------

synop_szn <- sampling_g_synop %>%  
  select(c(Datezzz, Stage_in_FR)) %>% 
  mutate(month = as.numeric(str_sub(Datezzz, 6, 7))) %>% 
  ggplot(aes(x = month)) +
  geom_dotplot() +
  theme_bw () 

(synop_szn)  






