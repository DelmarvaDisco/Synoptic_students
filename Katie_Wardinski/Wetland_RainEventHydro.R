#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Rain Event Hydrology
#Coder: Katie Wardinski
#Created: 2022-06-29
#Purpose: Explore rain fall event hydrology using waterlevel and survey data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#set working directory
setwd("C:/Workspace/Synoptic_students/Katie_Wardinski")

#load relevant packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)
library(raster)
library(patchwork)

#read in high frequency water level data
May20 <- read_csv("output_20200508_JM.csv")
Oct20 <- read_csv("output_20201015_JM.csv")
May21 <- read_csv("output_20210525_JM.csv")
Nov21 <- read_csv("output_20211112_JM.csv")
Apr22 <- read_csv("output_20220410_JM.csv")

#read in NOAA daily rainfall data
Precip <- read_csv("NOAA_Daily_Precip_2019-2022.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Plot Precip -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#search most common rainfall amount
Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(PRCP_mm > 0) %>% 
  ggplot() +
  geom_histogram(aes(PRCP_mm))

#summary of 11/24/2019-4/8/2022 (record of WL data)
Precip_Summary <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(PRCP_mm > 0) %>% 
  summarize(MedianPrecip = median(PRCP_mm),
            MeanPrecip = mean(PRCP_mm),
            MaxPrecip = max(PRCP_mm), #max daily precip = 76.2 mm
            N_observations = length(PRCP_mm)) #median daily precip is 3.8 mm

Snow_Summary <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(SNOW_mm > 0) %>% 
  summarize(MedianSnow = median(SNOW_mm),
            MeanSnow = mean(SNOW_mm),
            N_observations = length(SNOW_mm)) #median daily snowfall 25mm

#plot precip data
Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  ggplot(aes(mdy(DATE), PRCP_mm)) +
  geom_bar(stat="identity") +   
  xlab("Date") + ylab("Daily Precipitation (mm)") 

#plot snow data
Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  ggplot(aes(mdy(DATE), SNOW_mm)) +
  geom_bar(stat="identity") +   
  xlab("Date") + ylab("Daily Snow (mm)") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Focus on May - Oct 2020 --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#filter precip data
PrecipMayOct20 <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  mutate(Date_Good = mdy(DATE)) %>% 
  filter(Date_Good> "2020-05-07" & Date_Good < "2020-10-20")

#plot precip data
PrecipPlot <- PrecipMayOct20 %>% 
  ggplot(aes(mdy(DATE), PRCP_mm)) +
  geom_bar(stat="identity") +   
  xlab("Date") + ylab("Daily Precipitation (mm)") 

#focus on ND for now
#plot WL data 
WL_Plot <- Oct20 %>% 
  filter(Site_Name == "ND-SW") %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel))

PrecipPlot/WL_Plot
