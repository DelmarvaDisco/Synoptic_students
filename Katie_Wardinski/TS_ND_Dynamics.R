#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: TS and ND Wet Up Dynamics 
#Coder: Katie Wardinski
#Created: 2023-04-04
#Purpose: In-depth exploration of TS and ND stage-area, hydrology, and rain response
#Notes: This analysis focuses on daily data so temporal scales of precip and water level data match

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

#Read water level data
WL <- read_csv("dly_mean_output_NC_2019_2022.csv") #daily mean water level through fall 2022 updated by Nick 
high_freq_WL <- read_csv("output_JM_2019_2022.csv") #15 minute water level data

## SW only ##
SW_Daily <- WL %>% filter(grepl("SW",Site_ID))
SW_Daily$Date <- mdy(SW_Daily$Date)

#filter to TS and ND
Cleaned_WL <- SW_Daily %>% filter(Date > "2021-03-31") %>% 
  filter(Site_ID %in% c("ND-SW","TS-SW"))

#Read in stage area relationships
sa_97 <- read_csv("stage_area_relationships_97.csv") #97% threshold for identifying depressions

#read in NOAA daily rainfall data (need NOAA because Jackson Lane record isn't long enough)
Precip <- read_csv("NOAA_Denton_DailyPrecip_2021-22.csv")
Precip$DATE <- mdy(Precip$DATE)

#Shorten NOAA precip record to match water level data
Short_Precip <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(DATE > "2021-03-31")
  #filter(DATE >= "2021-01-01" & DATE < "2022-10-08")


#read in Jackson Lane precip for comparison to NOAA data and put on a daily time step
DMV_Precip <- read_csv("Jackson_Lane_precip_2018_2021.csv")
Daily_DMV <- DMV_Precip %>% mutate(day = cut(timestamp,breaks="day")) %>% 
            group_by(day) %>% 
            summarise(Daily_Precip_mm = sum(precip_mm)) %>% 
            filter(ymd(day) >= "2021-01-01")
Daily_DMV$Date <- ymd(Daily_DMV$day)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.1 Daily Mean Water Level ----------------------
#Data through October 2022

#Plot all SW data
WL_Plot <- SW_Daily %>% 
  filter(Site_ID %in% c("ND-SW","TS-SW")) %>% 
  filter(Date > "2021-03-31") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_ID))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  xlab("Date")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Interactive SW plot
WL_Plotly <- SW_Daily %>% 
  filter(Site_ID %in% c("ND-SW","TS-SW")) %>% 
  filter(Date > "2021-03-31") %>%
  plot_ly(x = ~Date) %>% 
  add_trace(y = ~dly_mean_wtrlvl, type = 'scatter', 
            mode = 'lines',color = ~Site_ID,
            line=list(width=4)) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Daily Precip --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.1 NOAA Precip ------------------------------

#summary of 1/31/21-12/31/22 (record of WL data)
Precip_Summary <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(PRCP_mm > 0) %>% 
  summarize(MedianPrecip = median(PRCP_mm),
            MeanPrecip = mean(PRCP_mm),
            MaxPrecip = max(PRCP_mm), #max daily precip = 76.2 mm
            N_observations = length(PRCP_mm)) #median daily precip is 3.8 mm

#plot precip and filter to dates where we have water level data
Precip_Plot <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(DATE > "2021-03-31" & DATE < "2021-10-08") %>% 
  ggplot(aes(DATE, PRCP_mm)) +
  geom_bar(stat="identity") +   
  xlab("Date") + ylab("Daily Precipitation (mm)") 

#Interactive SW plot
Precip_Plotly <- Precip %>% 
  dplyr::select(DATE,PRCP_mm,SNOW_mm) %>% 
  filter(DATE > "2021-03-31" & DATE < "2021-10-08") %>% 
  plot_ly(x = ~DATE, y = ~PRCP_mm) %>% 
  add_bars() 

## 3.2 Compare NOAA to Jackson Lane ------------------------

ggplot() +
  geom_bar(data = Short_Precip,mapping=aes(DATE, PRCP_mm),stat="identity",color="Red") + 
  geom_bar(data = Daily_DMV,mapping=aes(Date,Daily_Precip_mm),stat="identity",color="Blue") +
  xlab("Date") + ylab("Daily Precipitation (mm)")

Precip_Join <- left_join(Daily_DMV,Short_Precip,by=c("Date" = "DATE"))

Precip_Join %>% 
  #filter(Daily_Precip_mm > 0 & PRCP_mm > 0) %>% 
  ggplot()+
    geom_point(aes(Daily_Precip_mm,PRCP_mm,col=Date),size=3)+
    ylab("NOAA Denton, MD Precip (mm)")+
    xlab("Jackson Lane UMD Precip (mm)")+
    geom_abline(intercept = 0, slope = 1)+
    theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))
    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Stage-area relationships --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Copied from stage-area script

## 4.1 ND-SW ------------------------------
#plot stage-area relationship
ND_sa <- sa_97 %>% filter(Site_ID == "ND-SW") 

ND_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

ND_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(ND_sa$area_m) #1118 sq m @ z = 0.87

#filter to where there's breaks in the stage-area trend
ND_sa_lower <- sa_97 %>% filter(Site_ID == "ND-SW") %>% filter(z < 0.87)
ND_sa_upper <- sa_97 %>% filter(Site_ID == "ND-SW") %>% filter(z >= 0.87)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 13th order polynomial
ND_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,12,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

ND_area_model <- lm(area_m ~ poly(z,12,raw=T), data = ND_sa_lower) 
summary(ND_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.87,by=0.005)
predict_ND_area_model <- predict(ND_area_model,newdata = list(z = new_vals))
plot(x=ND_sa_lower$z,y=ND_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_ND_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.87 and linear z >= 0.87
ND_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.87
ND_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

ND_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=ND_sa_lower)
summary(ND_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.87,by=0.005)
predict_ND_vol_model <- predict(ND_vol_model_lower,newdata = list(z = new_vals))
plot(x=ND_sa_lower$z,y=ND_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_ND_vol_model,col="Blue")

ND_vol_model_upper <- lm(volume_m3 ~ z,data=ND_sa_upper)
summary(ND_vol_model_upper) 

#calculating change in area and volume on a daily timestep
ND_WL <- SW_Daily %>% 
  filter(Site_ID == "ND-SW") %>% 
  #filter(Date > "2021-03-29") %>% 
  #for area, if water level is <0.87, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.87 & dly_mean_wtrlvl > 0, 
                           ((ND_area_model$coefficients[13]*(dly_mean_wtrlvl^12)) +
                              (ND_area_model$coefficients[12]*(dly_mean_wtrlvl^11)) +
                              (ND_area_model$coefficients[11]*(dly_mean_wtrlvl^10)) +
                              (ND_area_model$coefficients[10]*(dly_mean_wtrlvl^9)) +
                              (ND_area_model$coefficients[9]*(dly_mean_wtrlvl^8)) +
                              (ND_area_model$coefficients[8]*(dly_mean_wtrlvl^7)) +
                              (ND_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (ND_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                              (ND_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                              (ND_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (ND_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                              (ND_area_model$coefficients[2]*dly_mean_wtrlvl) +
                              ND_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 0.87,max(ND_sa$area_m),0)),
         #for volume, if water level is <0.87, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.87 & dly_mean_wtrlvl > 0,
                             ((ND_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                                (ND_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                                (ND_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                                (ND_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                                (ND_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                                ND_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.87,
                                     (ND_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                       ND_vol_model_upper$coefficients[1],0)),
         #calculate distance from wetland center using equation fitted in excel from survey data
         dist_m = if_else(dly_mean_wtrlvl > 0,
                          ((14.697*dly_mean_wtrlvl^3) - (43.52*dly_mean_wtrlvl^2) + (47.209*dly_mean_wtrlvl)),0),
         #calculate daily change in area and volume
         delta_waterlevel = dly_mean_wtrlvl - lag(dly_mean_wtrlvl),
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3),
         delta_dist = dist_m - lag(dist_m))

#delta area
max(ND_WL$delta_area,na.rm=T)
cv(ND_WL$delta_area,na.rm=T)
#area
cv(ND_WL$area_m2)
mean(ND_WL$area_m2)
max(ND_WL$area_m2)
#distance
max(ND_WL$dist_m)
mean(ND_WL$dist_m)
min(ND_WL$dist_m)
cv(ND_WL$dist_m)

#plot water level over time
ND_p1 <- ND_WL %>% 
  filter(Date > "2021-03-31") %>% 
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("ND-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
ND_p2 <- ND_WL %>%
  filter(Date > "2021-03-31") %>% 
  ggplot()+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW daily wetland area")

#plot change in area over time
ND_p3 <- ND_WL %>%
  filter(Date > "2021-03-31") %>% 
  ggplot()+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW daily change in wetland area")


#plot distance over time
ND_p4 <- ND_WL %>%
  filter(Date > "2021-03-31") %>% 
  ggplot()+
  geom_line(aes(ymd(Date),dist_m)) +
  ylab("Edge of water distance (m)")+
  xlab("Date")+  
  ggtitle("ND-SW Edge of water distance from wetland center")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))
  
ND_WL %>%
    #filter(Date > "2021-03-31") %>% 
    ggplot()+
    geom_line(aes(ymd(Date),dist_m),size = 1) +
    ylab("Edge of water distance (m)")+
    geom_hline(yintercept = 3.6,linetype="dashed")+ #transect spot 1
    geom_hline(yintercept = 7.2,linetype="dashed")+  #transect spot 2
    geom_hline(yintercept = 10.8,linetype="dashed")+  #transect spot 3
    geom_hline(yintercept = 14.4,linetype="dashed")+  #transect spot 4
    geom_hline(yintercept = 18.6,linetype="dashed")+  #max distance
    xlab("Date")+  
    ggtitle("ND-SW Edge of water distance from wetland center")+
    theme(axis.text.y   = element_text(size=16),
          axis.text.x   = element_text(size=16),
          axis.title.y  = element_text(size=18),
          axis.title.x  = element_text(size=18),
          title = element_text(size = 18))

#plot volume over time
ggplot(ND_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("ND-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


ND_p1 / ND_p2 / ND_p3 / ND_p4


## 4.2 TS-SW ------------------------------
#plot stage-area relationship
TS_sa <- sa_97 %>% filter(Site_ID == "TS-SW") 

TS_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

TS_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(TS_sa$area_m) #453 sq m @ z = 0.60

#filter to where there's breaks in the stage-area trend
TS_sa_lower <- sa_97 %>% filter(Site_ID == "TS-SW") %>% filter(z < 0.6)
TS_sa_upper <- sa_97 %>% filter(Site_ID == "TS-SW") %>% filter(z >= 0.6)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
TS_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,5,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 12))

TS_area_model <- lm(area_m ~ poly(z,5,raw=T), data = TS_sa_lower) 
summary(TS_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.6,by=0.005)
predict_TS_area_model <- predict(TS_area_model,newdata = list(z = new_vals))
plot(x=TS_sa_lower$z,y=TS_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_TS_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.6 and linear z >= 0.6
TS_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.6
TS_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

TS_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=TS_sa_lower)
summary(TS_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.6,by=0.005)
predict_TS_vol_model <- predict(TS_vol_model_lower,newdata = list(z = new_vals))
plot(x=TS_sa_lower$z,y=TS_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_TS_vol_model,col="Blue")

TS_vol_model_upper <- lm(volume_m3 ~ z,data=TS_sa_upper)
summary(TS_vol_model_upper) 

#calculating change in area and volume on a daily timestep
TS_WL <- SW_Daily %>% 
  filter(Site_ID == "TS-SW") %>% 
  #for area, if water level is <0.6, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.6 & dly_mean_wtrlvl > 0, 
                           ((TS_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                              (TS_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                              (TS_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (TS_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                              (TS_area_model$coefficients[2]*dly_mean_wtrlvl) +
                              TS_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 0.6,max(TS_sa$area_m),0)),
         #for volume, if water level is <0.6, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.6 & dly_mean_wtrlvl > 0,
                             ((TS_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                                (TS_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                                (TS_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                                (TS_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                                (TS_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                                TS_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.6 ,
                                     (TS_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                       TS_vol_model_upper$coefficients[1],0)),
         #calculate distance from wetland center using equation fitted in excel from survey data
         dist_m = if_else(dly_mean_wtrlvl > 0,
                          ((-101.42*dly_mean_wtrlvl^2) + (106.23*dly_mean_wtrlvl)),0),
         #calculate daily change in area and volume
         delta_waterlevel = dly_mean_wtrlvl - lag(dly_mean_wtrlvl),
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3),
         delta_dist = dist_m - lag(dist_m))

#delta area
max(TS_WL$delta_area,na.rm=T)
mean(TS_WL$delta_area,na.rm=T)
cv(TS_WL$delta_area,na.rm=T)
#area
cv(TS_WL$area_m2)
mean(TS_WL$area_m2)
max(TS_WL$area_m2)
#distance
max(TS_WL$dist_m)
mean(TS_WL$dist_m)
min(TS_WL$dist_m)
cv(TS_WL$dist_m)
#water level
mean(TS_WL$dly_mean_wtrlvl)
#delta water level
mean(TS_WL$delta_waterlevel,na.rm=T)

#plot water level over time
TS_p1 <- TS_WL %>% 
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("TS-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
TS_p2 <- ggplot(TS_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TS-SW daily wetland area")

#plot change in area over time
TS_p3 <- ggplot(TS_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TS-SW daily change in wetland area")

#plot distance over time
TS_p4 <- ggplot(TS_WL)+
  geom_line(aes(ymd(Date),dist_m)) +
  ylab("Edge distance (m)")+
  xlab("Date")+  
  ggtitle("TS-SW Edge of water distance from wetland center")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

TS_WL %>%
  ggplot()+
  geom_line(aes(ymd(Date),dist_m),size = 2) +
  ylab("Edge of water distance (m)")+
  geom_hline(yintercept = 4.8,linetype="dashed")+ #transect spot 1
  geom_hline(yintercept = 9.6,linetype="dashed")+  #transect spot 2
  geom_hline(yintercept = 14.4,linetype="dashed")+  #transect spot 3
  geom_hline(yintercept = 19.2,linetype="dashed")+  #transect spot 4
  geom_hline(yintercept = 27.8,linetype="dashed")+  #max distance
  xlab("Date")+  
  ggtitle("TS-SW Edge of water distance from wetland center")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


TS_p1 / TS_p2 / TS_p3 /TS_p4 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Explore relationship between rain event and WL response -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 5.1 Prep data ----------------------------------
#this analysis uses daily data so all temporal scales of precip and water level data match

#clean and join precip and water level data for subsequent analyses
Cleaned_WL_wide <- pivot_wider(Cleaned_WL,names_from = Site_ID,values_from = dly_mean_wtrlvl)

Data_Joined <- left_join(Cleaned_WL_wide,Short_Precip,by=c("Date"="DATE"))

#join stage area data
Data_Join_ND <- left_join(Data_Joined,ND_WL,by="Date")
Data_Join_ND <- Data_Join_ND %>% 
                select(Date,Site_ID,PRCP_mm,SNOW_mm,dly_mean_wtrlvl,area_m2,volume_m3,
                       dist_m,delta_waterlevel,delta_area,delta_vol,delta_dist) %>% 
                mutate(month = month(ymd(Date)))

Data_Join_TS <- left_join(Data_Joined,TS_WL,by="Date")
Data_Join_TS <- Data_Join_TS %>% 
  select(Date,Site_ID,PRCP_mm,SNOW_mm,dly_mean_wtrlvl,area_m2,volume_m3,
         dist_m,delta_waterlevel,delta_area,delta_vol,delta_dist) %>% 
  mutate(month = month(ymd(Date)))


## 5.2 Plot precip vs water level change ----------------------------------
#water level and precip plotted together
WL_Plotly %>% 
  add_bars(data=Short_Precip, x = ~DATE, y = ~PRCP_mm,name="Precip (mm)",
           yaxis = "y2",color="Red") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))
  
#For times when Precip > 0 and Change in WL > 0
#Daily time scale
Data_Join_ND %>% 
  filter(PRCP_mm > 0 & delta_waterlevel > 0) %>% 
  #filter(month %in% c("8","9","10")) %>% 
  ggplot(aes(PRCP_mm,delta_waterlevel,col=month))+
  geom_point(size=3) +
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 1.9,label.y = 0.22)+
  stat_cor(label.x = 1.9,label.y = 0.2)+
  scale_color_continuous(type = "viridis")+
  ylab("Change in daily water level (m)")+
  xlab("Daily Precip (mm)")+
  ggtitle("ND-SW")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

Data_Join_TS %>% 
  filter(PRCP_mm > 0 & delta_waterlevel > 0) %>% 
  #filter(month %in% c("8","9","10")) %>% 
  ggplot(aes(PRCP_mm,delta_waterlevel,col=month))+
  geom_point(size=3) +
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 1.9,label.y = 0.22)+
  stat_cor(label.x = 1.9,label.y = 0.2)+
  scale_color_continuous(type = "viridis")+
  ylab("Change in daily water level (m)")+
  xlab("Daily Precip (mm)")+
  ggtitle("TS-SW")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

## 5.3 Summarize Aug - Oct dynamics ----------------------

### 5.3.1 ND-SW ----------------------------------
ND_Summary <- Data_Join_ND %>% 
                filter(month %in% c("7","8","9","10")) %>%
                group_by(month) %>% 
                summarise(Min_WL = min(dly_mean_wtrlvl,na.rm=T),
                          Mean_WL = mean(dly_mean_wtrlvl,na.rm=T),
                          Max_WL = max(dly_mean_wtrlvl,na.rm=T),
                          CV_WL = cv(dly_mean_wtrlvl,na.rm=T),
                          Min_Dist = min(dist_m,na.rm=T),
                          Mean_Dist = mean(dist_m,na.rm=T),
                          Max_Dist = max(dist_m,na.rm=T),
                          CV_Dist = cv(dist_m,na.rm=T),
                          Min_Area = min(area_m2,na.rm=T),
                          Mean_Area = mean(area_m2,na.rm=T),
                          Max_Area = max(area_m2,na.rm=T),
                          Mean_Delta_WL = mean(delta_waterlevel,na.rm=T),
                          Max_Delta_WL = max(delta_waterlevel,na.rm=T),
                          Mean_Delta_Area = mean(delta_area,na.rm=T),
                          Max_Delta_Area = max(delta_area,na.rm=T),
                          Mean_Delta_Dist = mean(delta_dist,na.rm=T),
                          Max_Delta_Dist = max(delta_dist,na.rm=T),
                          Precip_Total = sum(PRCP_mm,na.rm=T))


### 5.3.2 TS-SW ----------------------------------
#Note TS-SW has some NA values in 2022
TS_Summary <- Data_Join_TS %>% 
  filter(month %in% c("7","8","9","10")) %>%
  group_by(month) %>% 
  summarise(Min_WL = min(dly_mean_wtrlvl,na.rm=T),
            Mean_WL = mean(dly_mean_wtrlvl,na.rm=T),
            Max_WL = max(dly_mean_wtrlvl,na.rm=T),
            CV_WL = cv(dly_mean_wtrlvl,na.rm=T),
            Min_Dist = min(dist_m,na.rm=T),
            Mean_Dist = mean(dist_m,na.rm=T),
            Max_Dist = max(dist_m,na.rm=T),
            CV_Dist = cv(dist_m,na.rm=T), 
            Min_Area = min(area_m2,na.rm=T),
            Mean_Area = mean(area_m2,na.rm=T),
            Max_Area = max(area_m2,na.rm=T),
            Mean_Delta_WL = mean(delta_waterlevel,na.rm=T),
            Max_Delta_WL = max(delta_waterlevel,na.rm=T),
            Mean_Delta_Area = mean(delta_area,na.rm=T),
            Max_Delta_Area = max(delta_area,na.rm=T),
            Mean_Delta_Dist = mean(delta_dist,na.rm=T),
            Max_Delta_Dist = max(delta_dist,na.rm=T),
            Precip_Total = sum(PRCP_mm,na.rm=T))
