#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: TS and ND Wet Up Dynamics 
#Coder: Katie Wardinski
#Created: 2023-04-04
#Purpose: In-depth exploration of TS and ND stage-area, hydrology, and rain response
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

#Read in stage area relationships
sa_97 <- read_csv("stage_area_relationships_97.csv") #97% threshold for identifying depressions

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.1 Daily Mean Water Level ----------------------
#Data through October 2022
## SW only ##
SW_Daily <- WL %>% filter(grepl("SW",Site_ID))
SW_Daily$Date <- mdy(SW_Daily$Date)

#Plot all SW data
SW_Daily %>% 
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
SW_Daily %>% 
  filter(Site_ID %in% c("ND-SW","TS-SW")) %>% 
  filter(Date > "2021-03-31") %>%
  plot_ly(x = ~Date) %>% 
  add_trace(y = ~dly_mean_wtrlvl, type = 'scatter', mode = 'lines',color = ~Site_ID) 


## 2.2 High frequency water level ---------------------
##Only have a shorter interval of data here (ends April 2022)
## SW only ##
SW_hf <- high_freq_WL %>% filter(grepl("SW",Site_Name))

#Plot all SW data
SW_hf %>% 
  filter(Site_Name %in% c("ND-SW","TS-SW")) %>% 
  filter(Timestamp > "2021-03-31 00:00:00") %>% 
  ggplot(aes(Timestamp,waterLevel,col=Site_Name))+
  geom_line()+
  ylab("Water Level (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Interactive SW plot
SW_hf %>% 
  filter(Site_Name %in% c("ND-SW","TS-SW")) %>% 
  filter(Timestamp > "2021-03-31 00:00:00") %>% 
  plot_ly(x = ~Timestamp) %>% 
  add_trace(y = ~waterLevel, type = 'scatter', mode = 'lines',color = ~Site_Name) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Stage-area relationships --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ND-SW ------------------------------
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
ND_WL <- WL %>% 
  filter(Site_ID == "ND-SW") %>% 
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
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(ND_WL$delta_area,na.rm=T)
cv(ND_WL$delta_area,na.rm=T)
cv(ND_WL$area_m2)
mean(ND_WL$area_m2)
max(ND_WL$area_m2)

#plot water level over time
ND_p1 <- WL %>% 
  filter(Site_ID == "ND-SW") %>%
  ggplot()+
  geom_line(aes(mdy(Date),dly_mean_wtrlvl))+
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
ND_p2 <- ggplot(ND_WL)+
  geom_line(aes(mdy(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW daily wetland area")

#plot change in area over time
ND_p3 <- ggplot(ND_WL)+
  geom_line(aes(mdy(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW daily change in wetland area")

#plot volume over time
ND_p4 <- ggplot(ND_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("ND-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


ND_p1 / ND_p2 / ND_p3


##redo using high frequency water level data
ND_WL_hf <- high_freq_WL %>% 
  filter(Site_Name == "ND-SW") %>% 
  #for area, if water level is <0.87, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(waterLevel < 0.87 & waterLevel > 0, 
                           ((ND_area_model$coefficients[13]*(waterLevel^12)) +
                              (ND_area_model$coefficients[12]*(waterLevel^11)) +
                              (ND_area_model$coefficients[11]*(waterLevel^10)) +
                              (ND_area_model$coefficients[10]*(waterLevel^9)) +
                              (ND_area_model$coefficients[9]*(waterLevel^8)) +
                              (ND_area_model$coefficients[8]*(waterLevel^7)) +
                              (ND_area_model$coefficients[7]*(waterLevel^6)) +
                              (ND_area_model$coefficients[6]*(waterLevel^5)) + 
                              (ND_area_model$coefficients[5]*(waterLevel^4)) + 
                              (ND_area_model$coefficients[4]*(waterLevel^3)) + 
                              (ND_area_model$coefficients[3]*(waterLevel^2)) +  
                              (ND_area_model$coefficients[2]*waterLevel) +
                              ND_area_model$coefficients[1]),
                           if_else(waterLevel >= 0.87,max(ND_sa$area_m),0)),
         #for volume, if water level is <0.87, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(waterLevel < 0.87 & waterLevel > 0,
                             ((ND_vol_model_lower$coefficients[6]*(waterLevel^5)) +
                                (ND_vol_model_lower$coefficients[5]*(waterLevel^4)) +
                                (ND_vol_model_lower$coefficients[4]*(waterLevel^3)) + 
                                (ND_vol_model_lower$coefficients[3]*(waterLevel^2)) +
                                (ND_vol_model_lower$coefficients[2]*(waterLevel)) + 
                                ND_vol_model_lower$coefficients[1]),
                             if_else(waterLevel>= 0.87,
                                     (ND_vol_model_upper$coefficients[2]*waterLevel) + 
                                       ND_vol_model_upper$coefficients[1],0)),
         #calculate distance from wetland center using equation fitted in excel from survey data
         dist_m = if_else(waterLevel > 0,
                          ((14.697*waterLevel^3) - (43.52*waterLevel^2) + (47.209*waterLevel)),0),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(ND_WL_hf$delta_area,na.rm=T)
cv(ND_WL_hf$delta_area,na.rm=T)
cv(ND_WL_hf$area_m2)
mean(ND_WL_hf$area_m2)
max(ND_WL_hf$area_m2)
mean(ND_WL_hf$dist_m)
min(ND_WL_hf$dist_m)
max(ND_WL_hf$dist_m)
cv(ND_WL_hf$dist_m)

#plot water level over time
ND_p1_hf <- high_freq_WL %>% 
  filter(Site_Name == "ND-SW") %>%
  ggplot()+
  geom_line(aes(ymd_hms(Timestamp),waterLevel))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("ND-SW wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
ND_p2_hf <- ggplot(ND_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW wetland area")

#distance from center over time
ggplot(ND_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),dist_m))+
  ylab("Dist from Wetland Center (m)")+
  #xlim(ymd_hms("2021-08-07 00:00:00"),ymd_hms("2021-08-17 00:00:00"))+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW Water Edge Dist from Wetland Center")

#plot change in area over time
ND_p3_hf <- ggplot(ND_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("ND-SW change in wetland area")

#plot volume over time
ND_p4_hf <- ggplot(ND_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("ND-SW volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


ND_p1_hf / ND_p2_hf / ND_p3_hf

## TS-SW ------------------------------
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
TS_WL <- WL %>% 
  filter(Site_Name == "TS-SW") %>% 
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
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(TS_WL$delta_area,na.rm=T)
cv(TS_WL$delta_area,na.rm=T)
cv(TS_WL$area_m2)
mean(TS_WL$area_m2)
max(TS_WL$area_m2)

#plot water level over time
TS_p1 <- WL %>% 
  filter(Site_Name == "TS-SW") %>%
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

#plot volume over time
TS_p4 <- ggplot(TS_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("TS-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


TS_p1 / TS_p2 / TS_p3

##redo using high frequency water level data
TS_WL_hf <- high_freq_WL %>% 
  filter(Site_Name == "TS-SW") %>%
  #for area, if water level is <0.6, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(waterLevel < 0.6 & waterLevel > 0, 
                           ((TS_area_model$coefficients[6]*(waterLevel^5)) + 
                              (TS_area_model$coefficients[5]*(waterLevel^4)) + 
                              (TS_area_model$coefficients[4]*(waterLevel^3)) + 
                              (TS_area_model$coefficients[3]*(waterLevel^2)) +  
                              (TS_area_model$coefficients[2]*waterLevel) +
                              TS_area_model$coefficients[1]),
                           if_else(waterLevel >= 0.6,max(TS_sa$area_m),0)),
         #for volume, if water level is <0.6, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(waterLevel < 0.6 & waterLevel > 0,
                             ((TS_vol_model_lower$coefficients[6]*(waterLevel^5)) +
                                (TS_vol_model_lower$coefficients[5]*(waterLevel^4)) +
                                (TS_vol_model_lower$coefficients[4]*(waterLevel^3)) + 
                                (TS_vol_model_lower$coefficients[3]*(waterLevel^2)) +
                                (TS_vol_model_lower$coefficients[2]*(waterLevel)) + 
                                TS_vol_model_lower$coefficients[1]),
                             if_else(waterLevel >= 0.6 ,
                                     (TS_vol_model_upper$coefficients[2]*waterLevel) + 
                                       TS_vol_model_upper$coefficients[1],0)),
         
         #calculate distance from wetland center using equation fitted in excel from survey data
         dist_m = if_else(waterLevel > 0,
                          ((-101.42*waterLevel^2) + (106.23*waterLevel)),0),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(TS_WL_hf$delta_area,na.rm=T)
cv(TS_WL_hf$delta_area,na.rm=T)
cv(TS_WL_hf$area_m2)
max(TS_WL_hf$dist_m)
mean(TS_WL_hf$dist_m)
min(TS_WL_hf$dist_m)
cv(TS_WL_hf$dist_m)

#plot water level over time
TS_p1_hf <- high_freq_WL %>% 
  filter(Site_Name == "TS-SW") %>%
  ggplot()+
  geom_line(aes(ymd_hms(Timestamp),waterLevel))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("TS-SW wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
TS_p2_hf <- ggplot(TS_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TS-SW wetland area")

#distance from center over time
ggplot(TS_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),dist_m))+
  ylab("Dist from Wetland Center (m)")+
  #xlim(ymd_hms("2021-08-07 00:00:00"),ymd_hms("2021-08-17 00:00:00"))+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TS-SW Water Edge Dist from Wetland Center")

#plot change in area over time
TS_p3_hf <- ggplot(TS_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TS-SW change in wetland area")

#plot volume over time
TS_p4_hf <- ggplot(TS_WL_hf)+
  geom_line(aes(ymd_hms(Timestamp),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("TS-SW volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


TS_p1_hf / TS_p2_hf / TS_p3_hf
