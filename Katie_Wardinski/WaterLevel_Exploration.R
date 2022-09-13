#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Synoptic Data Exploration
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
#QB trial
SW_Clean %>% 
  filter(Site_Name == "QB-SW") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl))+
  geom_line()

#all SW data
SW_Clean %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()

#zoom in on summer storm events
SW_Clean %>% 
  filter(Date > "2020-06-30" & Date < "2020-08-15") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()

#all GW data
GW %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()

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

#5.1 mean water level on date of sampling versus NPOC ----------
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

#5.2 Mean Chemistry vs Mean WL -----------------------------
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
#6.0 Preliminary Stage-Area Relationships ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 6.1 Look at all sites (preliminary) -------------
sa <- read_csv("jr_stage_area_relationships.csv")

#Explore stage-area
sa %>% 
  ggplot(aes(z,area_m,col=Site_ID))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")

#Explore stage-volume
sa %>% 
  ggplot(aes(z,volume_m3,col=Site_ID))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#Explore area-volume
sa %>% 
  ggplot(aes(area_m,volume_m3,col=Site_ID))+
  geom_point()+
  xlab("Area (m2)")+
  ylab("Volume (m3)")

##6.2 Calculate relative area and volume changes for a given change in water level --------------
#Use OB-SW as example

#fit equation to stage-area and stage-volume relationships
OB_sa <- sa %>% filter(Site_ID == "OB-SW") %>% filter(z < 0.58)

#stage - area is roughly linear
OB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'lm',se=FALSE)+
  stat_regline_equation(label.x = 0.2)+
  stat_cor()#+
  #geom_abline(slope = coef(area_model)[["z"]], 
              #intercept = coef(area_model)[["(Intercept)"]])

area_model <- lm(area_m ~ z, data = OB_sa)
summary(area_model) #area_m = 597.016*Z - 16.081

#stage - volume is roughly a power curve 
OB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

vol_model <- lm(volume_m3 ~ poly(z,2,raw=T),data=OB_sa)
x_axis <- seq(0,0.6,length=58)
plot(OB_sa$z,OB_sa$volume_m3,
     xlab = "Depth (m)",
     ylab = "Volume (m^3)")
lines(x_axis,predict(vol_model,data.frame(x=x_axis)),col='blue')
summary(vol_model) #vol_m3 = 1024.341(z^2) - 75.704(z) + 3

#calculating change in area and volume on a daily timestep
OB_WL <- WL %>% filter(Site_Name == "OB-SW") %>% 
  mutate(area_m2 = if_else(dly_mean_wtrlvl > 0.025,((597.016*dly_mean_wtrlvl)-16.081),0),
         volume_m3 = if_else(dly_mean_wtrlvl > 0.025,((1024.341*(dly_mean_wtrlvl^2)) - (75.704*(dly_mean_wtrlvl)) + 3),0),
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))
#****had to mess around with the water level threshold so I wouldn't get negative area values

#plot water level over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+ #mirrors WL since it's a linear relationship
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("OB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


#plot area over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),area_m2))+ #mirrors WL since it's a linear relationship
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("OB-SW daily wetland area")
#plot volume over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) 
#both
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),volume_m3,col="Volume")) +
  geom_line(aes(ymd(Date),area_m2,col="Area")) +
  ylab("Area (m2) and Volume (m3)")+
  xlab("Date")+
  scale_color_manual(name="Legend",
                     values=c("Volume" = "#F8766D", 
                              "Area" = "#00B8E7"))

#change in both area and volume over time
#plot area over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("OB-SW daily change in wetland area")
#plot volume over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),delta_volume)) 
#both
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),delta_vol,col="Volume")) +
  geom_line(aes(ymd(Date),delta_area,col="Area")) +
  ylab("Delta Area (m2) and Delta Volume (m3)")+
  xlab("Date")+
  scale_color_manual(name="Legend",
                     values=c("Volume" = "#F8766D", 
                              "Area" = "#00B8E7"))
