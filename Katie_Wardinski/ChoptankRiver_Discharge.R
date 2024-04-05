#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Choptank River Discharge Dynamics 
#Coder: Katie Wardinski
#Created: 2023-08-17
#Purpose: Explore record of Choptank River discharge data
#Code adapted from: Code adapted from USGS (https://waterdata.usgs.gov/blog/data-munging/)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#set working directory
setwd("C:/Workspace/Synoptic_students/Katie_Wardinski")

#load packages
library(dataRetrieval)
library(viridis)
library(lubridate)
library(tidyverse)
library(raster)
library(patchwork)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 USGS Data Retrieval -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Gage information:
#USGS 01491000 CHOPTANK RIVER NEAR GREENSBORO, MD\
#LOCATION.--Lat 38?59'49.9", long 75?47'08.9", referenced to North American Datum of 1983,\
#Caroline County, MD, Hydrologic Unit 02060005, on right bank at highway bridge (removed),\
#0.1 mi upstream from Gravelly Branch, 2.0 mi northeast of Greensboro, and 60 mi upstream from mouth\
#DRAINAGE AREA.--113 sq. mi.\

#pull data from USGS 
siteNumber <- "01491000" 
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" #discharge, cfs

#Get all data 1948-2023
startDate <- "1948-10-01" 
#startDate <- "1993-10-01"
endDate <- "2023-08-16"

allQ <- readNWISdv(siteNumber, parameterCd, startDate, endDate)
allQ <- addWaterYear(allQ)
names(allQ)[5]<- "Flow"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Cumulative discharge ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Calculate cumulative discharge by water year
cumulative_dat <- group_by(allQ, waterYear) %>%
  mutate(cumulative_dis = cumsum(Flow), 
         wy_doy = seq(1:n()))

#Pull out 2020 water year         
cum_WY_2023 <- filter(cumulative_dat,waterYear=="2023")

ggplot()+
  geom_line(data = cumulative_dat, aes(x = wy_doy, y = cumulative_dis, group = waterYear,color=waterYear),size=0.75)+
  scale_color_continuous(trans = 'reverse') +
  geom_line(data = cum_WY_2023, aes(x = wy_doy, y = cumulative_dis),size=2.5,color="Black")+
  scale_x_continuous(breaks = c(1, 93, 184, 275,366), labels = c("Oct 1", "Jan 1", "Apr 1", "July 1","Oct 1")) +
  theme_bw() +
  labs(color = "Water Year", x = "Date", y = "Cumulative Daily Discharge (cfs)")+
  theme(legend.text   = element_text(size=12),
        legend.title  = element_text(size=14),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Rank over 30 year record ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Summarize water year data 1948-2023
MeanQ_WY_1948_2020 <- allQ %>% 
  group_by(waterYear) %>% 
  summarize(Mean_Daily_Q = mean(Flow,na.rm=T))

#Pull specific dates over entire record
allQ$Mon_Day <- as.Date(allQ$Date)
allQ$Mon_Day <- format(allQ$Mon_Day,format= "%m-%d")

Aug_16 <- allQ %>% filter(Mon_Day == "08-16")

#Rank flow for those specific dates
Aug_16 <- Aug_16 %>% mutate(ranks = rank(Flow), percent_exceeded = 100*(ranks/(length(Flow)+1))) %>% arrange(ranks)  

#What percentile are the flows on those given dates compared to the entire record?
Aug16_2023 <- filter(Aug_16,waterYear=="2023") #56%
