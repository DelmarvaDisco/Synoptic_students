#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Stage on sample dates
# Coder: James Maze
# Date: 25 Jan 2021
# Purpose: To enumerate hydrologic conditions durring sampling campaigns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Notes:
#   - !!! Data folder isn't on Google Drive. Reach out if you want to recreate any of this. 
#   - Need to automate sampling dates on the stage time series
#   - Need to finish the summary table & plot for stage on sampling dates. 


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
  mutate(Stage_inch_FR = `Stage (in)`) %>% 
  mutate(Date_time = `Date Time, GMT-05:00`) %>% 
  select(c(Date_time, Stage_inch_FR))

#Read the sampling dates file
sampling <- read_excel(paste0(data_dir, "sampling_schedule.xlsx")) %>% 
  mutate(Datez = as.character(Sample_date))


# 3. Join Schedule to Stage -------------------------------

# Group daily stage values from FR-SW to make the join feasible
FR_Stage_daily <- FR_Stage %>% 
  mutate(Datez = str_sub(as.character(Date_time), 1, 10)) %>% 
  group_by(Datez) %>% 
  summarize(Stage_in_FR = mean(Stage_inch_FR)) 

#Join stage values to sample dates.
sampling <- right_join(FR_Stage_daily, sampling) %>% 
  select(-Sample_date)

#Add stage estimates for last data points

#Had to make a new data frame
sample_guess <- data.frame(Datez = c("2021-10-18", "2021-11-12", "2021-12-14"),
                           Stage_in_FR = c("11.5", "14", "15"),
                           Sample_type = c("BC + JL", "Synoptic", "BC + JL"),
                           )

sampling <- sampling %>% 
  rows_patch()


# 4. Make some plots ------------------------------------------------------

#Histogram of stage and
Stage_sampling_histo <- ggplot(data = sampling,
                               mapping = aes(x = Stage_in_FR)) +
                        geom_histogram(aes(fill = Sample_type)) +
                        theme_bw()

(Stage_sampling_histo)
  

#Plot stage with sampling campaings (not automated)
FR_Stage_ts <- ggplot(data = FR_Stage, 
       mapping = aes(x = Date_time, 
                     y = Stage_inch_FR)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_datetime(limits = as.POSIXct(c("2020-01-01", "2022-01-01"))) +
  ### At some point, fix this manual entry
  geom_vline(xintercept = as.POSIXct(c("2020-01-15 23:00:00", 
                                       "2020-03-10 23:00:00",
                                       "2020-07-28 23:00:00",
                                       "2020-09-24 00:00:00",
                                       "2020-11-04 00:00:00",
                                       "2021-02-28 00:00:00",
                                       "2021-05-12 00:00:00",
                                       "2021-06-21 00:00:00",
                                       "2021-11-12 23:00:00")), 
             size = 3, 
             color = "darkred") +
  ### At some point, fix this manual entry
  geom_vline(xintercept = as.POSIXct(c("2020-09-20 12:00:00",
                                       "2020-11-01 12:00:00",
                                       "2021-02-25 12:00:00",
                                       "2021-05-09 12:00:00",
                                       "2021-06-18 12:00:00",
                                       "2021-09-10 12:00:00",
                                       "2021-10-15 12:00:00",
                                       "2021-12-13 12:00:00")),
             size = 3, 
             color = "seagreen") +
  ### At some point, fix this manual entry
  geom_vline(xintercept = as.POSIXct(c("2021-09-13 12:00:00",
                                       "2021-10-18 12:00:00",
                                       "2021-12-16 12:00:00")),
             size = 3, 
             color = "midnightblue") +
  ggtitle("Sampling campaigns plotted with stage at FR-SW")

(FR_Stage_ts)








