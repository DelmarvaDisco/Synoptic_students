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
waterlevel <- read_csv("output_JM_2019_2022.csv")

#read in NOAA daily rainfall data
Precip <- read_csv("NOAA_Daily_Precip_2019-2022.csv")

#read in Jackson Lane / Jones Rd precip data from Michael W (2021 water year)
DMVPrecip <- read_csv("2021_WaterYear_Precip_Cleaned.csv")

#jackson lane data for water years 2018-2021
JacksonPrecip <- read_csv("Jackson_Lane_precip_2018_2021.csv")
  
#set theme classic
theme_set(theme_classic())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 NOAA Precip Data -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#NOAA data for Denton, MD

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
  xlab("Date in 2020") + ylab("Daily Precipitation (mm)")+
  ggtitle("NOAA Precip Data - Denton, MD")

#focus on ND for now
#plot WL data 
ND_WL <- waterlevel %>% 
  filter(Site_Name %in% c("ND-SW","ND-UW1","ND-UW2")) %>% 
  filter(Timestamp > ymd_hms("2020-05-08 00:00:00") & Timestamp < ymd_hms("2020-10-20 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("ND")

#zoom in on one storm event in late may at ND
Oct20 %>% 
  filter(Site_Name == "ND-SW") %>%
  filter(Timestamp > ymd_hms("2020-05-21 00:00:00") & Timestamp < ymd_hms("2020-05-25 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel))

#look at some other sites
QB_WL<- waterlevel %>% 
  filter(Site_Name %in% c("QB-SW","QB-UW1","QB-UW2")) %>% 
  filter(Timestamp > ymd_hms("2020-05-08 00:00:00") & Timestamp < ymd_hms("2020-10-20 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("QB")

DB_WL <- waterlevel %>% 
  filter(Site_Name %in% c("DB-SW","DB-UW1","DB-UW2")) %>% 
  filter(Timestamp > ymd_hms("2020-05-08 00:00:00") & Timestamp < ymd_hms("2020-10-20 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("DB")

TB_WL <- waterlevel %>% 
  filter(Site_Name %in% c("TB-SW","TB-UW1","TB-UW2","TB-UW3")) %>% 
  filter(Timestamp > ymd_hms("2020-05-08 00:00:00") & Timestamp < ymd_hms("2020-10-20 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("TB")

PrecipPlot/ND_WL/QB_WL/TB_WL/DB_WL


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 2021 Water year Jackson Lane and Jones Rd Precip Data -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 4.1 First look at data (on half hour time scale) -----------------------
hist(DMVPrecip$JacksonLane_Precip_mm,
     main = "Jackson Lane",
     xlab = "Precip (mm)", ylab = "Frequency",
     col = "darkorchid4")
hist(DMVPrecip$JonesRoad_Precip_mm,
     main = "Jones Rd",
     xlab = "Precip (mm)", ylab = "Frequency",
     col = "darkorchid4")



## 4.2 Determine event length (on half hr time scale) ----------------------------

#Event duration code adapted from:
# https://stackoverflow.com/questions/51371155/r-select-rainfall-events-and-calculate-rainfall-event-total-from-time-series-da

Event <- DMVPrecip %>% 
          #mark times when rainfall was occurring
          mutate(Jackson_rainflag = ifelse(JacksonLane_Precip_mm > 0,1,0),
                 Jones_rainflag = ifelse(JonesRoad_Precip_mm > 0,1,0)) %>% 
          #create column with number of consecutive times there is rain or not
          #rle indicates how many consecutive times values happen
          #rep to repeat it until you hit a new event
          mutate(Jackson_rainlength = rep(rle(Jackson_rainflag)$lengths, 
                                          rle(Jackson_rainflag)$lengths),
                 Jones_rainlength = rep(rle(Jones_rainflag)$lengths, 
                                          rle(Jones_rainflag)$lengths)) %>% 
          
          #create event flag, if rainflag = 1, its a rain event
          #if rain flag is 0, but not for 12 consecutive times (6 hrs), it's still the same event, put a 1
          mutate(Jackson_eventflag = ifelse(Jackson_rainflag == 1, 1, 
                                            ifelse(Jackson_rainflag == 0 & Jackson_rainlength < 12, 1, 0)),
                 Jones_eventflag = ifelse(Jones_rainflag == 1, 1, 
                                            ifelse(Jones_rainflag == 0 & Jones_rainlength < 12, 1, 0))) %>% 
          #create event ID's
          mutate(Jackson_eventID = rep(seq(1,length(rle(Jackson_eventflag)$lengths)), rle(Jackson_eventflag)$lengths),
                 Jones_eventID = rep(seq(1,length(rle(Jones_eventflag)$lengths)), rle(Jones_eventflag)$lengths)) %>% 
          #remove ID's when event flag is zero
          mutate(Jackson_eventID = Jackson_eventID*Jackson_eventflag,
                 Jones_eventID = Jones_eventID*Jones_eventflag) 

#add 30 mins to event duration because if event's only occur in 1 30 min block, time would be zero otherwise        
Jackson_Event <- Event %>% filter(Jackson_eventflag ==1 ) %>% 
                        group_by(Jackson_eventID) %>% 
                          summarize(Precip_mm = sum(JacksonLane_Precip_mm),
                                 Event_Start = first(timestamp),
                                 Event_End = last(timestamp),
                                 Duration_min = as.numeric(difftime(Event_End,Event_Start, units = 'mins'))+30,
                                 Intensity_mm_min = Precip_mm / Duration_min,
                                 Intensity_in_hr = Intensity_mm_min*60/25.4)

Jackson_Event_Summary <- Jackson_Event %>% 
                            summarize(Mean_Precip_mm = mean(Precip_mm),
                                      sd_Precip_mm = sd(Precip_mm),
                                      Mean_Duration_min = mean(Duration_min),
                                      sd_Duration_min = sd(Duration_min),
                                      Mean_Intensity_mm_min = mean(Intensity_mm_min),
                                      sd_Mean_Intensity_mm_min = sd(Intensity_mm_min))


Jones_Event <- Event %>% filter(Jones_eventflag ==1 ) %>% 
                group_by(Jones_eventID) %>% 
                summarize(Precip_mm = sum(JonesRoad_Precip_mm),
                          Event_Start = first(timestamp),
                          Event_End = last(timestamp),
                          Duration_min = as.numeric(difftime(Event_End,Event_Start, units = 'mins'))+30,
                          Intensity_mm_min = Precip_mm / Duration_min,
                          Intensity_in_hr = Intensity_mm_min*60/25.4)

Jones_Event_Summary <- Jones_Event %>% 
  summarize(Mean_Precip_mm = mean(Precip_mm),
            sd_Precip_mm = sd(Precip_mm),
            Mean_Duration_min = mean(Duration_min),
            sd_Duration_min = sd(Duration_min),
            Mean_Intensity_mm_min = mean(Intensity_mm_min),
            sd_Mean_Intensity_mm_min = sd(Intensity_mm_min))


## 4.3 Hourly event data -----------------------------------------
#calculate hourly precip at each site
Hourly <- DMVPrecip %>% mutate(hour = cut(timestamp,breaks="hour")) %>% 
                        group_by(hour) %>% 
                        summarise(Jackson_Hrly_mm = sum(JacksonLane_Precip_mm),
                                  Jones_Hrly_mm = sum(JonesRoad_Precip_mm))
#plot hourly 
Jack_HR <- Hourly %>% 
  ggplot(aes(ymd_hms(hour),Jackson_Hrly_mm))+
  geom_bar(stat="identity")
Jones_HR <-  Hourly %>% 
  ggplot(aes(ymd_hms(hour),Jones_Hrly_mm))+ 
  geom_bar(stat="identity")
  
Jack_HR / Jones_HR


## 4.4 Daily event data ---------------------------------------
#calcualte daily precip at each stie
Daily <- DMVPrecip %>% mutate(day = cut(timestamp,breaks="day")) %>% 
                       group_by(day) %>% 
                       summarise(Jackson_Daily_mm = sum(JacksonLane_Precip_mm),
                                 Jones_Daily_mm = sum(JonesRoad_Precip_mm))

#Plot daily precip
Jack_Day <- Daily %>% 
  ggplot(aes(ymd(day),Jackson_Daily_mm))+
  geom_bar(stat="identity")+
  ggtitle("Jackson Ln Daily Precip")+
  ylab("Precp (mm)")+
  xlab("Date")+
  xlim(ymd("2021-02-01"),ymd("2022-04-11"))
Jones_Day <- Daily %>% 
  ggplot(aes(ymd(day),Jones_Daily_mm))+
  geom_bar(stat="identity")+
  ggtitle("Jones Rd Daily Precip")+
  xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  ylab("Precp (mm)")+
  xlab("Date")+
  ylim(0,120)

Jack_Day / Jones_Day

#replot daily rainfall against water level using actual delmarva data
#plot WL data 
ND_WL <- waterlevel %>% 
  filter(Site_Name %in% c("ND-SW","ND-UW1","ND-UW2")) %>% 
  filter(Timestamp > ymd_hms("2020-10-01 00:00:00") & Timestamp < ymd_hms("2021-09-30 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("ND")

#look at some other sites
QB_WL<- waterlevel %>% 
  filter(Site_Name %in% c("QB-SW","QB-UW1","QB-UW2")) %>% 
  filter(Timestamp > ymd_hms("2020-10-01 00:00:00") & Timestamp < ymd_hms("2021-09-30 00:00:00")) %>%
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("QB")

DB_WL <- waterlevel %>% 
  filter(Site_Name %in% c("DB-SW","DB-UW1","DB-UW2")) %>% 
  filter(Timestamp > ymd_hms("2020-10-01 00:00:00") & Timestamp < ymd_hms("2021-09-30 00:00:00")) %>%
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("DB")

TB_WL <- waterlevel %>% 
  filter(Site_Name %in% c("TB-SW","TB-UW1","TB-UW2","TB-UW3")) %>% 
  filter(Timestamp > ymd_hms("2020-10-01 00:00:00") & Timestamp < ymd_hms("2021-09-30 00:00:00")) %>%
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date in 2020")+
  ylab("Water Level (m)")+
  ggtitle("TB")

Jack_Day/ND_WL/QB_WL/TB_WL/DB_WL

## zoom in on August rain event
Daily$day <- as.Date(Daily$day)
#Plot daily precip
Dec_Jack_Day <- Daily %>% 
  filter(day > ymd("2021-08-07") & day < ymd("2021-08-12")) %>% 
  ggplot(aes(ymd(day),Jackson_Daily_mm))+
  geom_bar(stat="identity")+
  ggtitle("Jackson Rd Daily Precip")+
  ylab("Precp (mm)")

#plot WL data 
Dec_ND_WL <- waterlevel %>% 
  filter(Site_Name %in% c("ND-SW","ND-UW1","ND-UW2")) %>% 
  filter(Timestamp > ymd_hms("2021-08-07 00:00:00") & Timestamp < ymd_hms("2021-08-13 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date")+
  ylab("Water Level (m)")+
  ggtitle("ND")

#look at some other sites
Dec_QB_WL<- waterlevel %>% 
  filter(Site_Name %in% c("QB-SW","QB-UW1","QB-UW2")) %>% 
  filter(Timestamp > ymd_hms("2021-08-07 00:00:00") & Timestamp < ymd_hms("2021-08-13 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date")+
  ylab("Water Level (m)")+
  ggtitle("QB")

Dec_DB_WL <- waterlevel %>% 
  filter(Site_Name %in% c("DB-SW","DB-UW1","DB-UW2")) %>% 
  filter(Timestamp > ymd_hms("2021-08-07 00:00:00") & Timestamp < ymd_hms("2021-08-13 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date")+
  ylab("Water Level (m)")+
  ggtitle("DB")

Dec_TB_WL <- waterlevel %>% 
  filter(Site_Name %in% c("TB-SW","TB-UW1","TB-UW2","TB-UW3")) %>% 
  filter(Timestamp > ymd_hms("2021-08-07 00:00:00") & Timestamp < ymd_hms("2021-08-13 00:00:00")) %>% 
  ggplot()+
  geom_line(aes(Timestamp,waterLevel,col=Site_Name))+
  xlab("Date")+
  ylab("Water Level (m)")+
  ggtitle("TB")

Dec_Jack_Day/Dec_ND_WL/Dec_QB_WL/Dec_TB_WL/Dec_DB_WL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Longer record of just Jackson Lane (2018-2021) ----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Event duration code adapted from:
# https://stackoverflow.com/questions/51371155/r-select-rainfall-events-and-calculate-rainfall-event-total-from-time-series-da

##5.1 Determine Events ----------------------------------------

##Need to review this - think there's an error
Event <- JacksonPrecip %>% 
  #mark times when rainfall was occurring
  mutate(Jackson_rainflag = ifelse(precip_mm > 0,1,0)) %>% 
  #create column with number of consecutive times there is rain or not
  #rle indicates how many consecutive times values happen
  #rep to repeat it until you hit a new event
  mutate(Jackson_rainlength = rep(rle(Jackson_rainflag)$lengths, 
                                  rle(Jackson_rainflag)$lengths)) %>% 
  
  #create event flag, if rainflag = 1, its a rain event
  #if rain flag is 0, but not for 12 consecutive times (6 hrs), it's still the same event, put a 1
  mutate(Jackson_eventflag = ifelse(Jackson_rainflag == 1, 1, 
                                    ifelse(Jackson_rainflag == 0 & Jackson_rainlength < 12, 1, 0))) %>% 
  #create event ID's
  mutate(Jackson_eventID = rep(seq(1,length(rle(Jackson_eventflag)$lengths)), rle(Jackson_eventflag)$lengths)) %>% 
  #remove ID's when event flag is zero
  mutate(Jackson_eventID = Jackson_eventID*Jackson_eventflag) 

#add 30 mins to event duration because if event's only occur in 1 30 min block, time would be zero otherwise        
Jackson_Event <- Event %>% filter(Jackson_eventflag==1 ) %>% 
  group_by(Jackson_eventID) %>% 
  summarise(Precip_mm = sum(precip_mm),
            Event_Start = first(timestamp),
            Event_End = last(timestamp),
            Duration_min = as.numeric(difftime(Event_End,Event_Start, units = 'mins'))+30,
            Intensity_mm_min = Precip_mm / Duration_min,
            Intensity_in_hr = Intensity_mm_min*60/25.4)

Jackson_Event_Summary <- Jackson_Event %>% 
  summarize(Mean_Precip_mm = mean(Precip_mm),
            Max_Precip_mm = max(Precip_mm),
            sd_Precip_mm = sd(Precip_mm),
            Mean_Duration_min = mean(Duration_min),
            Max_Duration_min = max(Duration_min),
            sd_Duration_min = sd(Duration_min),
            Mean_Intensity_mm_min = mean(Intensity_mm_min),
            Max_Intensity_mm_min = max(Intensity_mm_min),
            sd_Intensity_mm_min = sd(Intensity_mm_min))

## 5.2 Hourly event data -----------------------------------------
#calculate hourly precip at each site
Hourly <- JacksonPrecip %>% mutate(hour = cut(timestamp,breaks="hour")) %>% 
  group_by(hour) %>% 
  summarise(Jackson_Hrly_mm = sum(precip_mm))
#plot hourly 
Jack_HR <- Hourly %>% 
  ggplot(aes(ymd_hms(hour),Jackson_Hrly_mm))+
  geom_bar(stat="identity")

Jack_HR


## 5.3 Daily event data ---------------------------------------
#calcualte daily precip at each stie
Daily <- JacksonPrecip %>% mutate(day = cut(timestamp,breaks="day")) %>% 
  group_by(day) %>% 
  summarise(Jackson_Daily_mm = sum(precip_mm))

#Plot daily precip
Jack_Day <- Daily %>% 
  ggplot(aes(ymd(day),Jackson_Daily_mm))+
  geom_bar(stat="identity")

Jack_Day
