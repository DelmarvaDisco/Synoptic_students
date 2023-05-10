#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Water Level Data Exploration
#Coder: Katie Wardinski
#Created: 2022-06-02
#Purpose: Get familiar with water level trends in data to assist with PhD planning
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
library(plotly)
library(ggrepel)
library(ggpmisc)

#set theme classic
theme_set(theme_classic())

#Read data
WL <- read_csv("all_data_JM_2019-2022.csv") #daily mean water level
synoptic <-read_csv("SynopticCurrent.csv") #synoptic data through 2022-07
site <- read_csv("wetland_info.csv") #site info including wetland order

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Data manipulation -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.1 Water Level Data -----------------------------------
## summarize all WL data ##
WL_Summary <- WL %>% 
  group_by(Site_Name) %>% 
  summarize(mean_WL = mean(dly_mean_wtrlvl, na.rm=T),
            sd_WL = sd(dly_mean_wtrlvl, na.rm=T),
            CV_WL = cv(dly_mean_wtrlvl, na.rm = T))

## SW only ##
SW <- WL %>% filter(grepl("SW",Site_Name))

# Remove weird TB data
SW_Clean <- filter(SW, !dly_mean_wtrlvl > 1.2)

#calculate mean water level at each site (SW only)
SW_Summary <- SW_Clean %>% 
              group_by(Site_Name) %>% 
              summarize(mean_WL = mean(dly_mean_wtrlvl, na.rm=T),
                        sd_WL = sd(dly_mean_wtrlvl, na.rm=T),
                        CV_WL = cv(dly_mean_wtrlvl, na.rm = T))

#add column with Site names 
SW_Summary$Site <- c("BD","DB","DF","DK","FN","HB",
                     "JA","JB","JC","MB","NB","ND",
                     "OB","QB","TA","TB","TI","TS","XB")

#join summary to site info
SW_join <- left_join(site,SW_Summary,by="Site")

## GW Only ##
GW <- WL %>% filter(grepl("UW",Site_Name))

#calculate mean water level at each site (GW only)
GW_Summary <- GW %>% 
  group_by(Site_Name) %>% 
  summarize(mean_WL = mean(dly_mean_wtrlvl, na.rm=T),
            sd_WL = sd(dly_mean_wtrlvl, na.rm=T),
            CV_WL = cv(dly_mean_wtrlvl, na.rm = T))

#add column with Site names 
GW_Summary$Site <- c("DB","DK","DK","HB","JB","JB",
                     "JC","MB","ND","ND", "ND",
                     "OB","QB", "QB","TB","TB","TB","TS","XB")

#2.2 Synoptic data ----------------------------------------
#get date into YYYY-MM-DD format
synoptic$DateLong <- ymd(synoptic$Date)

#filter out rivers and channels
wetlands <- synoptic %>% filter(Type != "River") %>% filter(Type !="Channel")

#match water level data at time of sampling
join_WL_syn <- left_join(wetlands,WL,
                         by=c("DateLong" = "Date","Sample_Name" = "Site_Name"))
            
#summarize synoptic data
Syn_Summary <- join_WL_syn %>% 
                    group_by(Sample_Name,SW_GW) %>% 
                    summarize(mean_DOC = mean(NPOC_mgC_L,na.rm=T),
                              mean_TDN = mean(TDN_mgN_L,na.rm=T),
                              mean_TDP = mean(TDP_mgP_L,na.rm=T)) 

#join WL and synoptic summary
Mean_Join <- left_join(Syn_Summary,WL_Summary,by=c("Sample_Name" = "Site_Name"))

#Join in site info
Site_Syn <- left_join(synoptic,site,by="Site")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plot WL data -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##3.1 SW Only ----------------------------------
#all SW data
SW_Clean %>% 
  filter(Site_Name != "DF-SW") %>% 
  filter(Site_Name != "FN-SW") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Interactive plot
SW_Clean %>% 
  #filter(Site_Name %in% c("BD-SW","DB-SW","JB-SW","ND-SW","OB-SW","QB-SW","TB-SW")) %>% 
  plot_ly(x = ~Date) %>% 
  add_trace(y = ~dly_mean_wtrlvl, type = 'scatter', mode = 'lines',color = ~Site_Name) 

#zoom in on summer storm events
SW_Clean %>% 
  filter(Date > "2020-06-30" & Date < "2020-08-15") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()

## 3.2 GW Only --------------------------------
#all GW data
GW %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()


## 3.3 Individual Sites -----------------------------------
#QB trial
SW_Clean %>% 
  filter(Site_Name == "QB-SW") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl))+
  geom_line()

#zoom in on wet up in fall
SW_Clean %>% 
  filter(Site_Name == "QB-SW") %>%
  filter(Date > ymd("2021-07-01") & Date < ymd("2021-12-01")) %>% 
  ggplot()+
  geom_line(aes(Date,dly_mean_wtrlvl))

#ND trial
SW_Clean %>% 
  filter(Site_Name == "ND-SW") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl))+
  geom_line()

#zoom in on one storm event in late may at ND
SW_Clean %>% 
  filter(Site_Name == "ND-SW") %>%
  filter(Date > ymd("2020-02-01") & Date < ymd("2020-03-30")) %>% 
  ggplot()+
  geom_line(aes(Date,dly_mean_wtrlvl))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Explore WL vs Wetland Morphology ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#wetland order
SW_join %>% 
  ggplot()+
  geom_text(aes(wet_order,mean_WL,label=Site,col=property))

SW_join %>% 
  ggplot()+
  geom_text(aes(wet_order,CV_WL,label=Site,col=property))

#watershed area
SW_join %>% 
  ggplot()+
  geom_text(aes(watershed_area_m2,mean_WL,label=Site,col=property))

#perimeter to area ratio
SW_join %>% 
  ggplot()+
  geom_text(aes(p_a_ratio,mean_WL,label=Site,col=property))

#storage volume
SW_join %>% 
  ggplot()+
  geom_text(aes(wetland_storage_volume_m3,mean_WL,label=Site,col=property))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 DOC/Nutrients versus WL ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##5.1 mean water level on date of sampling versus NPOC ----------
join_WL_syn %>% 
  ggplot(aes(dly_mean_wtrlvl,NPOC_mgC_L,col=SW_GW))+
  geom_point()

#SW only - DOC
join_WL_syn %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(dly_mean_wtrlvl,NPOC_mgC_L,col=factor(Date_M)))+
  geom_point(size=2)+
  ylab("DOC (mg C/L)")+
  xlab("Mean daily water level (m) at time of sampling")+
  ggtitle("All Sites - Wetland SW Only")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#SW only - TDN
join_WL_syn %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(dly_mean_wtrlvl,TDN_mgN_L,col=factor(Date_M)))+
  geom_point(size=2)+
  ylab("TDN (mg N/L)")+
  xlab("Mean daily water level (m) at time of sampling")+
  ggtitle("All Sites - Wetland SW Only")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#SW only - TDP
join_WL_syn %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(dly_mean_wtrlvl,TDP_mgP_L,col=factor(Date_M)))+
  geom_point(size=2)+
  ylab("TDP (mg P/L)")+
  xlab("Mean daily water level (m) at time of sampling")+
  ggtitle("All Sites - Wetland SW Only")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

##5.2 Mean Chemistry vs Mean WL -----------------------------
#DOC
Mean_Join %>% 
  ggplot(aes(mean_WL,mean_DOC,col=SW_GW))+
  geom_point()
#TDN
Mean_Join %>% 
  ggplot(aes(mean_WL,mean_TDN,col=SW_GW))+
  geom_point()
#TDP
Mean_Join %>% 
  ggplot(aes(mean_WL,mean_TDP,col=SW_GW))+
  geom_point()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 MS Thesis WL Data -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Look at rate of water rise through different horizons to inform soil core set up

#Read data
#daily mean water level by station in QB, ND, TB, and DB
MS_WL <- read_csv("MS_2020WY_waterLevel_by_station.csv") %>% 
  drop_na() %>% 
  filter(Timestamp > "2019-12-01" & Timestamp < "2020-02-20" ) %>% #filter to 2020 winter wet up
  dplyr::select(-...1) %>%  #drop first column - don't need observation number
  filter(y_n <= 0) #drop points once water level gets above ground surface

#survey data for transect points and soil horzion depths
survey <- read_csv("MS_transect_survey.csv")

## 6.1 Visualize 2019-20 winter wet up event -----------------------------------------
#Zoom in on wet up from December to March, drop upland
ND_winter <- MS_WL %>% 
  filter(wetland == "ND" ) %>%
  pivot_wider(names_from = station, values_from = y_n) %>% 
  #filter(Timestamp > "2019-12-09" & Timestamp < "2019-12-17" ) %>%
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='#045a8d',size=1.5) +
  geom_line(aes(y=`KW-2E`), col='#2b8cbe',size=1.5) +
  geom_line(aes(y=`KW-3T`), col='#74a9cf',size=1.5) +
  geom_hline(yintercept=0,linetype="dashed")+
  ggtitle("ND") +
  theme_bw() +
  ylim(-2.25,1)+
  theme(
    plot.title = element_text(size= 15),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 12),
    plot.margin = margin(t = 0,  # Top margin
                         r = 0.5,  # Right margin
                         b = 0.5,  # Bottom margin
                         l = 0,  # Left margin
                         unit = "cm")) + 
  #Add labels
  xlab("Date") + 
  ylab("Water Level (m)") 

#QB
QB_winter <- MS_WL %>% 
  filter(wetland == "QB" ) %>%
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='#045a8d',size=1.5) +
  geom_line(aes(y=`KW-2E`), col='#2b8cbe',size=1.5) +
  geom_line(aes(y=`KW-3T`), col='#74a9cf',size=1.5) +
  geom_hline(yintercept=0,linetype="dashed")+
  ggtitle("QB") +
  theme_bw() +
  ylim(-1.5,0.5)+
  theme(
    plot.title = element_text(size= 15),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 12),
    plot.margin = margin(t = 0,  # Top margin
                         r = 0.5,  # Right margin
                         b = 0.5,  # Bottom margin
                         l = 0,  # Left margin
                         unit = "cm")) + 
  xlab("Date") + 
  ylab("Water Level (m)") 

#DB
DB_winter <- MS_WL %>% 
  filter(wetland == "DB" ) %>%
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='#045a8d',size=1.5) +
  geom_line(aes(y=`KW-2E`), col='#2b8cbe',size=1.5) +
  geom_line(aes(y=`KW-3T`), col='#74a9cf',size=1.5) +
  geom_hline(yintercept=0,linetype="dashed")+
  ggtitle("DB") +
  theme_bw() +
  ylim(-1.5,0.75)+
  theme(
    plot.title = element_text(size= 15),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 12),
    plot.margin = margin(t = 0,  # Top margin
                         r = 0.5,  # Right margin
                         b = 0.5,  # Bottom margin
                         l = 0,  # Left margin
                         unit = "cm")) + 
  xlab("Date") + 
  ylab("Water Level (m)") 

#TB
TB_winter <- MS_WL %>% 
  filter(wetland == "TB" ) %>%
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='#045a8d',size=1.5) +
  geom_line(aes(y=`KW-2E`), col='#2b8cbe',size=1.5) +
  geom_line(aes(y=`KW-3T`), col='#74a9cf',size=1.5) +
  geom_hline(yintercept=0,linetype="dashed")+
  ggtitle("TB") +
  theme_bw() +
  ylim(-1.5,0.5)+
  theme(
    plot.title = element_text(size= 15),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 12),
    plot.margin = margin(t = 0,  # Top margin
                         r = 0.5,  # Right margin
                         b = 0.5,  # Bottom margin
                         l = 0,  # Left margin
                         unit = "cm")) + 
  xlab("Date") + 
  ylab("Water Level (m)") 

(ND_winter + QB_winter) / (TB_winter + DB_winter)

## 6.2 Calculate duration water spends in each horizon, rate of water rise (Dec 1, 2019 - Feb 20, 2020) ------------------------

#Join together water level data and soil horizon elevations (dates already filtered to winter wet up)
join <- left_join(MS_WL,survey,by=c("wetland","station")) 

#Sort based on site & station
join <- join %>% arrange(wetland, station, Timestamp) %>% drop_na(y_n)

#Create column with binary indicator of saturation in each horizon
soil_sat <- join %>% mutate(inunO = if_else(y_n>O_lower,1,0),
                            inunA = if_else(y_n>A_lower,1,0),
                            inunB = if_else(y_n>B_lower,1,0),
                            water_in_O = if_else(y_n < 0 & y_n > O_lower,1,0),
                            water_in_A = if_else(y_n < O_lower & y_n > A_lower,1,0),
                            water_in_B = if_else(y_n < A_lower & y_n > B_lower,1,0))

#Summarise Data
soil_wetup_metrics<-soil_sat %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(n_observations  = length(Timestamp),
            dur_O_inun_day  = sum(inunO),
            dur_water_in_O_day = sum(water_in_O),
            O_percent_sat   =(sum(dur_water_in_O_day)/n_observations),
            dur_A_inun_day  = sum(inunA),
            dur_water_in_A_day = sum(water_in_A),
            A_percent_sat   =(sum(dur_water_in_A_day)/n_observations),
            dur_B_inun_day  = sum(inunB),
            dur_water_in_B_day = sum(water_in_B),
            B_percent_sat   =(sum(dur_water_in_B_day)/n_observations))


#rate of water rise
rate <- join %>% 
  filter(station != "KW-4U") %>% 
  group_by(wetland, station) %>% 
  filter(Timestamp == first(Timestamp) | Timestamp == last(Timestamp)) 

rate$first_last <- rep(c("first", "last"), times = nrow(rate)/2)
rate_wide <- rate %>% pivot_wider(names_from = first_last, values_from = c(y_n,Timestamp))
rate_rise <- rate_wide %>% 
              mutate(n_days = difftime(Timestamp_last,Timestamp_first,units = "days"),
                     delta_WL = y_n_last - y_n_first,
                     rate = delta_WL/as.numeric(n_days))
mean_rate <- mean(rate_rise$rate)

##6.3 Calculate duration water spends in each horizon, rate of water rise (Dec 9-16, 2019) ------------------------

#filter time series down even more
WL_short <- MS_WL %>%  
  filter(Timestamp > "2019-12-08" & Timestamp < "2019-12-16" ) #Dec 9 - 16, 2019

#Join together water level data and soil horizon elevations (dates already filtered to winter wet up)
short_join <- left_join(WL_short,survey,by=c("wetland","station")) 

#Sort based on site & station
short_join <- short_join %>% arrange(wetland, station, Timestamp) %>% drop_na(y_n)

#Create column with binary indicator of saturation in each horizon
short_soil_sat <- short_join %>% mutate(inunO = if_else(y_n>O_lower,1,0),
                            inunA = if_else(y_n>A_lower,1,0),
                            inunB = if_else(y_n>B_lower,1,0),
                            water_in_O = if_else(y_n < 0 & y_n > O_lower,1,0),
                            water_in_A = if_else(y_n < O_lower & y_n > A_lower,1,0),
                            water_in_B = if_else(y_n < A_lower & y_n > B_lower,1,0))

#Summarise Data
short_soil_wetup_metrics<-short_soil_sat  %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(n_observations  = length(Timestamp),
            dur_O_inun_day  = sum(inunO),
            dur_water_in_O_day = sum(water_in_O),
            O_percent_sat   =(sum(dur_water_in_O_day)/n_observations),
            dur_A_inun_day  = sum(inunA),
            dur_water_in_A_day = sum(water_in_A),
            A_percent_sat   =(sum(dur_water_in_A_day)/n_observations),
            dur_B_inun_day  = sum(inunB),
            dur_water_in_B_day = sum(water_in_B),
            B_percent_sat   =(sum(dur_water_in_B_day)/n_observations))


#rate of water rise
short_rate <- short_join %>% 
  filter(station != "KW-4U") %>% 
  group_by(wetland, station) %>% 
  filter(Timestamp == first(Timestamp) | Timestamp == last(Timestamp)) 

short_rate$first_last <- rep(c("first", "last"), times = nrow(rate)/2)
short_rate_wide <- short_rate %>% pivot_wider(names_from = first_last, values_from = c(y_n,Timestamp))
short_rate_rise <- short_rate_wide %>% 
                    mutate(n_days = difftime(Timestamp_last,Timestamp_first,units = "days"),
                           delta_WL = y_n_last - y_n_first,
                           rate = delta_WL/as.numeric(n_days))
short_mean_rate <- mean(short_rate_rise$rate)

