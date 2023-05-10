#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Synoptic Data Exploration
#Coder: Katie Wardinski
#Created: 2023-04-04
#Purpose: Explore changes in wetted area over time 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Water level data -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##2.1 Water Level Data -----------------------------------
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

#Plot all SW data
SW_Clean %>% 
  filter(Site_Name != "DF-SW") %>% 
  filter(Site_Name != "FN-SW") %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Interactive SW plot
SW_Clean %>% 
  #filter(Site_Name %in% c("BD-SW","DB-SW","JB-SW","ND-SW","OB-SW","QB-SW","TB-SW")) %>% 
  plot_ly(x = ~Date) %>% 
  add_trace(y = ~dly_mean_wtrlvl, type = 'scatter', mode = 'lines',color = ~Site_Name) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Preliminary Stage-Area Relationships ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.1 Look at all sites (preliminary) -------------
sa_80 <- read_csv("stage_area_relationships_80.csv") #80% threshold for identifying depressions
sa_97 <- read_csv("stage_area_relationships_97.csv") #97% threshold for identifying depressions

#Explore stage-area
sa_80 %>% 
  filter(Site_ID != "DF-SW") %>% 
  filter(Site_ID != "FN-SW") %>% 
  ggplot(aes(z,area_m,col=Site_ID))+
  geom_point()+
  ggtitle("80% threshold")+
  ylim(0,4000)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")

#I'm going to use 97 because it avoids merging issues with certain sites
sa_97 %>% 
  filter(Site_ID != "DF-SW") %>% 
  filter(Site_ID != "FN-SW") %>% 
  ggplot(aes(z,area_m,col=Site_ID))+
  geom_line()+
  ggtitle("97% threshold for basin delineation")+
  xlab("Water Depth (m)")+
  ylab("Area (sq. m.)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#Jackson Lane stage area
sa_97 %>% 
  filter(Site_ID %in% c("ND-SW","TS-SW","DK-SW","BD-SW")) %>%    
  ggplot(aes(z,area_m,col=Site_ID))+
  geom_line(size=1.25)+
  ggtitle("97% threshold for basin delineation")+
  xlab("Water Depth (m)")+
  ylab("Area (sq. m.)")+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=16),
        plot.title = element_text(size = 20))

#Explore stage-volume
sa_97 %>% 
  filter(Site_ID != "DF-SW") %>% 
  filter(Site_ID != "FN-SW") %>% 
  ggplot(aes(z,volume_m3,col=Site_ID))+
  ggtitle("97% threshold for basin delineation")+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#Explore area-volume
sa_97 %>%   
  filter(Site_ID != "DF-SW") %>% 
  filter(Site_ID != "FN-SW") %>% 
  ggplot(aes(area_m,volume_m3,col=Site_ID))+
  geom_point()+
  xlab("Area (m2)")+
  ylab("Volume (m3)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Site Specific Stage-Area Relationships ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate relative area and volume changes for a given change in water level
#Note: I want to look into this raw=T vs raw=F and how it influences the lm model coefficients

## BD-SW ---------------------------------
BD_sa <- sa_97 %>% filter(Site_ID == "BD-SW")

BD_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

BD_sa %>% ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(BD_sa$area_m) #94 sq m @ z = 0.52

#filter to where there's breaks in the data trends
BD_sa_lower <- sa_97 %>% filter(Site_ID == "BD-SW") %>% filter(z < 0.52)
BD_sa_upper <- sa_97 %>% filter(Site_ID == "BD-SW") %>% filter(z >= 0.52)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
BD_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,5,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 16),
        axis.title=element_text(size=16))

BD_area_model <- lm(area_m ~ poly(z,5,raw=T), data = BD_sa_lower) 
summary(BD_area_model) 

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.52,by=0.005)
predict_BD_area_model <- predict(BD_area_model,newdata = list(z = new_vals))
plot(x=BD_sa_lower$z,y=BD_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_BD_area_model,col="Blue")

#stage - volume is 4th order polynomial when z < 0.52 and linear z >= .52
BD_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,4,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.52
BD_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

BD_vol_model_lower <- lm(volume_m3 ~ poly(z,4,raw=T),data=BD_sa_lower)
summary(BD_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.52,by=0.005)
predict_BD_vol_model <- predict(BD_vol_model_lower,newdata = list(z = new_vals))
plot(x=BD_sa_lower$z,y=BD_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_BD_vol_model,col="Blue")

BD_vol_model_upper <- lm(volume_m3 ~ z,data=BD_sa_upper)
summary(BD_vol_model_upper)

#calculating change in area and volume on a daily timestep
BD_WL <- WL %>% 
  filter(Site_Name == "BD-SW") %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, 
  #if WL < 0 print 0, if WL > 0.5 print max area
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.52 & dly_mean_wtrlvl > 0, 
                           ((BD_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (BD_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) +
                            (BD_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (BD_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (BD_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             BD_area_model$coefficients[1]),
                   if_else(dly_mean_wtrlvl >= 0.52,max(BD_sa$area_m),0)),
                   #if_else(dly_mean_wtrlvl <= 0,0,NA))),
         
  #for volume, if water level is <0.5, use stage-area polynomial function, 
  #if WL > 0.5 use the linear relationship, if wl < 0 print 0
         volume_m3 = if_else(dly_mean_wtrlvl < 0.52 & dly_mean_wtrlvl > 0,
                             ((BD_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                              (BD_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (BD_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (BD_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               BD_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.52,
                                     (BD_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                     (BD_vol_model_upper$coefficients[1]),0)),                            # if_else(dly_mean_wtrlvl <= 0,0,NA))),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(BD_WL$delta_area,na.rm=T)
cv(BD_WL$delta_area,na.rm=T)
cv(BD_WL$area_m2)

#plot water level over time
BD_p1 <- WL %>% 
  filter(Site_Name == "BD-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("BD-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
BD_p2 <- ggplot(BD_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("BD-SW daily wetland area")

#plot change in area over time
BD_p3 <- ggplot(BD_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("BD-SW daily change in wetland area")

#plot volume over time
BD_p4 <- ggplot(BD_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("BD-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


BD_p1 / BD_p2 / BD_p3

## DB-SW ---------------------------------
#plot stage-area relationship
DB_sa <- sa_97 %>% filter(Site_ID == "DB-SW") 

DB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

DB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(DB_sa$area_m) #473 sq m @ z = 1.08

#filter to where there's breaks in the stage-area trend
DB_sa_lower <- sa_97 %>% filter(Site_ID == "DB-SW") %>% filter(z < 1.08)
DB_sa_upper <- sa_97 %>% filter(Site_ID == "DB-SW") %>% filter(z >= 1.08)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
DB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,5,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

DB_area_model <- lm(area_m ~ poly(z,5,raw=T), data = DB_sa_lower) 
summary(DB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=1.08,by=0.005)
predict_DB_area_model <- predict(DB_area_model,newdata = list(z = new_vals))
plot(x=DB_sa_lower$z,y=DB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_DB_area_model,col="Blue")

#stage - volume is 4th order polynomial when z < 1.08 and linear z >= 1.08
DB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,4,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.5
DB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

DB_vol_model_lower <- lm(volume_m3 ~ poly(z,4,raw=T),data=DB_sa_lower)
summary(DB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=1.08,by=0.005)
predict_DB_vol_model <- predict(DB_vol_model_lower,newdata = list(z = new_vals))
plot(x=DB_sa_lower$z,y=DB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_DB_vol_model,col="Blue")

DB_vol_model_upper <- lm(volume_m3 ~ z,data=DB_sa_upper)
summary(DB_vol_model_upper) #vol_m3 = 187z + 36.33

#calculating change in area and volume on a daily timestep
DB_WL <- WL %>% 
  filter(Site_Name == "DB-SW") %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 1.08 & dly_mean_wtrlvl > 0, 
                           ((DB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (DB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (DB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (DB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (DB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             DB_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 1.08,max(DB_sa$area_m),0)),
        
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 1.08 & dly_mean_wtrlvl > 0,
                             ((DB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (DB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (DB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (DB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               DB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 1.08,
                                     (DB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      DB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
DB_p1 <- WL %>% 
  filter(Site_Name == "DB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("DB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
DB_p2 <- ggplot(DB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("DB-SW daily wetland area")

#plot change in area over time
DB_p3 <- ggplot(DB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("DB-SW daily change in wetland area")

#plot volume over time
DB_p4 <- ggplot(DB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("DB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


DB_p1 / DB_p2 / DB_p3

## DK-SW ------------------------------

#plot stage-area relationship
DK_sa <- sa_97 %>% filter(Site_ID == "DK-SW") 

DK_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

DK_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(DK_sa$area_m) #478 sq m @ z = 0.54

#filter to where there's breaks in the stage-area trend
DK_sa_lower <- sa_97 %>% filter(Site_ID == "DK-SW") %>% filter(z < 0.54)
DK_sa_upper <- sa_97 %>% filter(Site_ID == "DK-SW") %>% filter(z >= 0.54)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 7th order polynomial
DK_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,7,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

DK_area_model <- lm(area_m ~ poly(z,7,raw=T), data = DK_sa_lower) 
summary(DK_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.54,by=0.005)
predict_DK_area_model <- predict(DK_area_model,newdata = list(z = new_vals))
plot(x=DK_sa_lower$z,y=DK_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_DK_area_model,col="Blue")

#stage - volume is 6th order polynomial when z < 0.54 and linear z >= 0.54
DK_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,6,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.54
DK_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

DK_vol_model_lower <- lm(volume_m3 ~ poly(z,6,raw=T),data=DK_sa_lower)
summary(DK_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.54,by=0.005)
predict_DK_vol_model <- predict(DK_vol_model_lower,newdata = list(z = new_vals))
plot(x=DK_sa_lower$z,y=DK_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_DK_vol_model,col="Blue")

DK_vol_model_upper <- lm(volume_m3 ~ z,data=DK_sa_upper)
summary(DK_vol_model_upper) 

#calculating change in area and volume on a daily timestep
DK_WL <- WL %>% 
  filter(Site_Name == "DK-SW") %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.54 & dly_mean_wtrlvl > 0, 
                           ((DK_area_model$coefficients[8]*(dly_mean_wtrlvl^7)) +
                            (DK_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) +
                            (DK_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (DK_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (DK_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (DK_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (DK_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             DK_area_model$coefficients[1]),
                            if_else(dly_mean_wtrlvl >= 0.54,max(DK_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.54 & dly_mean_wtrlvl > 0,
                             ((DK_vol_model_lower$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (DK_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (DK_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (DK_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (DK_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (DK_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               DK_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.54,
                                     (DK_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      DK_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
DK_p1 <- WL %>% 
  filter(Site_Name == "DK-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("DK-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
DK_p2 <- ggplot(DK_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("DK-SW daily wetland area")

#plot change in area over time
DK_p3 <- ggplot(DK_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("DK-SW daily change in wetland area")

#plot volume over time
DK_p4 <- ggplot(DK_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("DK-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

DK_p1 / DK_p2 / DK_p3

## HB-SW ------------------------------
#plot stage-area relationship
HB_sa <- sa_97 %>% filter(Site_ID == "HB-SW") 

HB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

HB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(HB_sa$area_m) #311 sq m @ z = 0.48

#filter to where there's breaks in the stage-area trend
HB_sa_lower <- sa_97 %>% filter(Site_ID == "HB-SW") %>% filter(z < 0.48)
HB_sa_upper <- sa_97 %>% filter(Site_ID == "HB-SW") %>% filter(z >= 0.48)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
HB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,5,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

HB_area_model <- lm(area_m ~ poly(z,5,raw=T), data = HB_sa_lower) 
summary(HB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.48,by=0.005)
predict_HB_area_model <- predict(HB_area_model,newdata = list(z = new_vals))
plot(x=HB_sa_lower$z,y=HB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_HB_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.48 and linear z >= 0.48
HB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.5
HB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

HB_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=HB_sa_lower)
summary(HB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.48,by=0.005)
predict_HB_vol_model <- predict(HB_vol_model_lower,newdata = list(z = new_vals))
plot(x=HB_sa_lower$z,y=HB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_HB_vol_model,col="Blue")

HB_vol_model_upper <- lm(volume_m3 ~ z,data=HB_sa_upper)
summary(HB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
HB_WL <- WL %>% 
  filter(Site_Name == "HB-SW") %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.48 & dly_mean_wtrlvl > 0, 
                           ((HB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (HB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (HB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (HB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (HB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             HB_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 0.48,max(HB_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.48 & dly_mean_wtrlvl > 0,
                             ((HB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (HB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (HB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (HB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (HB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               HB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.48,
                                     (HB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      HB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
HB_p1 <- WL %>% 
  filter(Site_Name == "HB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("HB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
HB_p2 <- ggplot(HB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("HB-SW daily wetland area")

#plot change in area over time
HB_p3 <- ggplot(HB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("HB-SW daily change in wetland area")

#plot volume over time
HB_p4 <- ggplot(HB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("HB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


HB_p1 / HB_p2 / HB_p3

## JA-SW ------------------------------
#plot stage-area relationship
JA_sa <- sa_97 %>% filter(Site_ID == "JA-SW") 

JA_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

JA_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(JA_sa$area_m) #148 sq m @ z = 0.23

#filter to where there's breaks in the stage-area trend
JA_sa_lower <- sa_97 %>% filter(Site_ID == "JA-SW") %>% filter(z < 0.23)
JA_sa_upper <- sa_97 %>% filter(Site_ID == "JA-SW") %>% filter(z >= 0.23)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
JA_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,5,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

JA_area_model <- lm(area_m ~ poly(z,5,raw=T), data = JA_sa_lower) 
summary(JA_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.23,by=0.005)
predict_JA_area_model <- predict(JA_area_model,newdata = list(z = new_vals))
plot(x=JA_sa_lower$z,y=JA_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_JA_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.23 and linear z >= 0.23
JA_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.23
JA_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

JA_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=JA_sa_lower)
summary(JA_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.23,by=0.005)
predict_JA_vol_model <- predict(JA_vol_model_lower,newdata = list(z = new_vals))
plot(x=JA_sa_lower$z,y=JA_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_JA_vol_model,col="Blue")

JA_vol_model_upper <- lm(volume_m3 ~ z,data=JA_sa_upper)
summary(JA_vol_model_upper) 

#calculating change in area and volume on a daily timestep
JA_WL <- WL %>% 
  filter(Site_Name == "JA-SW") %>% 
  
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.23 & dly_mean_wtrlvl > 0, 
                           ((JA_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (JA_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (JA_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (JA_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (JA_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             JA_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 0.23,max(JA_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.23 & dly_mean_wtrlvl > 0,
                             ((JA_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (JA_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (JA_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (JA_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (JA_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               JA_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.23,
                                    (JA_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      JA_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
JA_p1 <- WL %>% 
  filter(Site_Name == "JA-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("JA-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
JA_p2 <- ggplot(JA_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("JA-SW daily wetland area")

#plot change in area over time
JA_p3 <- ggplot(JA_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("JA-SW daily change in wetland area")

#plot volume over time
JA_p4 <- ggplot(JA_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("JA-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


JA_p1 / JA_p2 / JA_p3

## JB-SW ------------------------------
#plot stage-area relationship
JB_sa <- sa_97 %>% filter(Site_ID == "JB-SW") 

JB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

JB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(JB_sa$area_m) #1837 sq m @ z = 0.53

#filter to where there's breaks in the stage-area trend
JB_sa_lower <- sa_97 %>% filter(Site_ID == "JB-SW") %>% filter(z < 0.53)
JB_sa_upper <- sa_97 %>% filter(Site_ID == "JB-SW") %>% filter(z >= 0.53)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 10th order polynomial
JB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,12,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

JB_area_model <- lm(area_m ~ poly(z,12,raw=T), data = JB_sa_lower) 
summary(JB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.53,by=0.005)
predict_JB_area_model <- predict(JB_area_model,newdata = list(z = new_vals))
plot(x=JB_sa_lower$z,y=JB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_JB_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.53 and linear z >= 0.48
JB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,7,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.53
JB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

JB_vol_model_lower <- lm(volume_m3 ~ poly(z,7,raw=T),data=JB_sa_lower)
summary(JB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.53,by=0.005)
predict_JB_vol_model <- predict(JB_vol_model_lower,newdata = list(z = new_vals))
plot(x=JB_sa_lower$z,y=JB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_JB_vol_model,col="Blue")

JB_vol_model_upper <- lm(volume_m3 ~ z,data=JB_sa_upper)
summary(JB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
JB_WL <- WL %>% 
  filter(Site_Name == "JB-SW") %>% 
  #for area, if water level is <0.53, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.53 & dly_mean_wtrlvl > 0, 
                           ((JB_area_model$coefficients[13]*(dly_mean_wtrlvl^12)) +
                            (JB_area_model$coefficients[12]*(dly_mean_wtrlvl^11)) +
                            (JB_area_model$coefficients[11]*(dly_mean_wtrlvl^10)) +
                            (JB_area_model$coefficients[10]*(dly_mean_wtrlvl^9)) +
                            (JB_area_model$coefficients[9]*(dly_mean_wtrlvl^8)) +
                            (JB_area_model$coefficients[8]*(dly_mean_wtrlvl^7)) +
                            (JB_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) +
                            (JB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (JB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (JB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (JB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (JB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             JB_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 0.53,max(JB_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.53 & dly_mean_wtrlvl > 0,
                             ((JB_vol_model_lower$coefficients[8]*(dly_mean_wtrlvl^7)) +
                              (JB_vol_model_lower$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (JB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (JB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (JB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (JB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (JB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               JB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.53,
                                     (JB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      JB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
JB_p1 <- WL %>% 
  filter(Site_Name == "JB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("JB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
JB_p2 <- ggplot(JB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("JB-SW daily wetland area")

#plot change in area over time
JB_p3 <- ggplot(JB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("JB-SW daily change in wetland area")

#plot volume over time
JB_p4 <- ggplot(JB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("JB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


JB_p1 / JB_p2 / JB_p3

## JC-SW ------------------------------
#plot stage-area relationship
JC_sa <- sa_97 %>% filter(Site_ID == "JC-SW") 

JC_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

JC_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(JC_sa$area_m) #521 sq m @ z = 0.36

#filter to where there's breaks in the stage-area trend
JC_sa_lower <- sa_97 %>% filter(Site_ID == "JC-SW") %>% filter(z < 0.36)
JC_sa_upper <- sa_97 %>% filter(Site_ID == "JC-SW") %>% filter(z >= 0.36)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 6th order polynomial
JC_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,6,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

JC_area_model <- lm(area_m ~ poly(z,6,raw=T), data = JC_sa_lower) 
summary(JC_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.36,by=0.005)
predict_JC_area_model <- predict(JC_area_model,newdata = list(z = new_vals))
plot(x=JC_sa_lower$z,y=JC_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_JC_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.53 and linear z >= 0.48
JC_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,6,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.36
JC_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

JC_vol_model_lower <- lm(volume_m3 ~ poly(z,6,raw=T),data=JC_sa_lower)
summary(JC_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.36,by=0.005)
predict_JC_vol_model <- predict(JC_vol_model_lower,newdata = list(z = new_vals))
plot(x=JC_sa_lower$z,y=JC_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_JC_vol_model,col="Blue")

JC_vol_model_upper <- lm(volume_m3 ~ z,data=JC_sa_upper)
summary(JC_vol_model_upper) 

#calculating change in area and volume on a daily timestep
JC_WL <- WL %>% 
  filter(Site_Name == "JC-SW") %>% 
  #for area, if water level is <0.53, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.36 & dly_mean_wtrlvl > 0, 
                           ((JC_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) +
                            (JC_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (JC_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (JC_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (JC_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (JC_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             JC_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 0.36,max(JC_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.36 & dly_mean_wtrlvl > 0,
                             ((JC_vol_model_lower$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (JC_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (JC_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (JC_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (JC_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (JC_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               JC_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.36,
                                     (JC_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      JC_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
JC_p1 <- WL %>% 
  filter(Site_Name == "JC-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("JC-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
JC_p2 <- ggplot(JC_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("JC-SW daily wetland area")

#plot change in area over time
JC_p3 <- ggplot(JC_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("JC-SW daily change in wetland area")

#plot volume over time
JC_p4 <- ggplot(JC_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("JC-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


JC_p1 / JC_p2 / JC_p3

## MB-SW ------------------------------
#plot stage-area relationship
MB_sa <- sa_97 %>% filter(Site_ID == "MB-SW") 

MB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

MB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(MB_sa$area_m) #1069 sq m @ z = 0.75

#filter to where there's breaks in the stage-area trend
MB_sa_lower <- sa_97 %>% filter(Site_ID == "MB-SW") %>% filter(z < 0.75)
MB_sa_upper <- sa_97 %>% filter(Site_ID == "MB-SW") %>% filter(z >= 0.75)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 6th order polynomial
MB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,6,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

MB_area_model <- lm(area_m ~ poly(z,6,raw=T), data = MB_sa_lower) 
summary(MB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.75,by=0.005)
predict_MB_area_model <- predict(MB_area_model,newdata = list(z = new_vals))
plot(x=MB_sa_lower$z,y=MB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_MB_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.75 and linear z >= 0.75
MB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.5
MB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

MB_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=MB_sa_lower)
summary(MB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.75,by=0.005)
predict_MB_vol_model <- predict(MB_vol_model_lower,newdata = list(z = new_vals))
plot(x=MB_sa_lower$z,y=MB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_MB_vol_model,col="Blue")

MB_vol_model_upper <- lm(volume_m3 ~ z,data=MB_sa_upper)
summary(MB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
MB_WL <- WL %>% 
  filter(Site_Name == "MB-SW") %>% 
  
  #for area, if water level is <0.75, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.75 & dly_mean_wtrlvl > 0, 
                           ((MB_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) + 
                            (MB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (MB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (MB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (MB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (MB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             MB_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 0.75,max(MB_sa$area_m),0)),
         #for volume, if water level is <0.75, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.75 & dly_mean_wtrlvl > 0,
                             ((MB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (MB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (MB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (MB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (MB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               MB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.75,
                                     (MB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      MB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(MB_WL$delta_area,na.rm=T)
cv(MB_WL$delta_area,na.rm=T)
cv(MB_WL$area_m2)

#plot water level over time
MB_p1 <- WL %>% 
  filter(Site_Name == "MB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("MB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
MB_p2 <- ggplot(MB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("MB-SW daily wetland area")

#plot change in area over time
MB_p3 <- ggplot(MB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("MB-SW daily change in wetland area")

#plot volume over time
MB_p4 <- ggplot(MB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("MB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


MB_p1 / MB_p2 / MB_p3

## NB-SW ------------------------------
#plot stage-area relationship
NB_sa <- sa_97 %>% filter(Site_ID == "NB-SW") 

NB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

NB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(NB_sa$area_m) #144 sq m @ z = 0.29

#filter to where there's breaks in the stage-area trend
NB_sa_lower <- sa_97 %>% filter(Site_ID == "NB-SW") %>% filter(z < 0.29)
NB_sa_upper <- sa_97 %>% filter(Site_ID == "NB-SW") %>% filter(z >= 0.29)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 6th order polynomial
NB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,6,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

NB_area_model <- lm(area_m ~ poly(z,6,raw=T), data = NB_sa_lower) 
summary(NB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.29,by=0.005)
predict_NB_area_model <- predict(NB_area_model,newdata = list(z = new_vals))
plot(x=NB_sa_lower$z,y=NB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_NB_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.48 and linear z >= 0.48
NB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.5
NB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

NB_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=NB_sa_lower)
summary(NB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.29,by=0.005)
predict_NB_vol_model <- predict(NB_vol_model_lower,newdata = list(z = new_vals))
plot(x=NB_sa_lower$z,y=NB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_NB_vol_model,col="Blue")

NB_vol_model_upper <- lm(volume_m3 ~ z,data=NB_sa_upper)
summary(NB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
NB_WL <- WL %>% 
  filter(Site_Name == "NB-SW") %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.29 & dly_mean_wtrlvl > 0, 
                           ((NB_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) +
                            (NB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (NB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (NB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (NB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (NB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             NB_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 0.29,max(NB_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.29 & dly_mean_wtrlvl > 0,
                             ((NB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (NB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (NB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (NB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (NB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               NB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.29,
                                     (NB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                    NB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
NB_p1 <- WL %>% 
  filter(Site_Name == "NB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("NB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
NB_p2 <- ggplot(NB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("NB-SW daily wetland area")

#plot change in area over time
NB_p3 <- ggplot(NB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("NB-SW daily change in wetland area")

#plot volume over time
NB_p4 <- ggplot(NB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("NB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


NB_p1 / NB_p2 / NB_p3

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
  filter(Site_Name == "ND-SW") %>% 
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

#plot water level over time
ND_p1 <- WL %>% 
  filter(Site_Name == "ND-SW") %>%
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
ND_p2 <- ggplot(ND_WL)+
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
ND_p3 <- ggplot(ND_WL)+
  geom_line(aes(ymd(Date),delta_area))+
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

## OB-SW  ----------------------------
#plot stage-area relationship
OB_sa <- sa_97 %>% filter(Site_ID == "OB-SW") 

OB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

OB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(OB_sa$area_m) #187 sq m @ z = 0.5

#filter to where there's breaks in the stage-area trend
OB_sa_lower <- sa_97 %>% filter(Site_ID == "OB-SW") %>% filter(z < 0.5)
OB_sa_upper <- sa_97 %>% filter(Site_ID == "OB-SW") %>% filter(z >= 0.5)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 4th order polynomial
OB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,4,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

OB_area_model <- lm(area_m ~ poly(z,4,raw=T), data = OB_sa_lower) 
summary(OB_area_model) #area_m = 3442.178z^4 - 6008.535z^3 + 2426.926z^2 + 216.826z + 2.547
#Note: I want to look into this raw=T vs raw=F and how it influences the coeficients

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.5,by=0.005)
predict_OB_area_model <- predict(OB_area_model,newdata = list(z = new_vals))
plot(x=OB_sa_lower$z,y=OB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_OB_area_model,col="Blue")

#stage - volume is cubic polynomial when z < 0.5 and linear z > -.5
OB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,3,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.5
OB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

OB_vol_model_lower <- lm(volume_m3 ~ poly(z,3,raw=T),data=OB_sa_lower)
summary(OB_vol_model_lower) #vol_m3 = -2110.5033z^3 + 1817.4408(z^2) - 132.9173(z) + 2.6822

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.5,by=0.005)
predict_OB_vol_model <- predict(OB_vol_model_lower,newdata = list(z = new_vals))
plot(x=OB_sa_lower$z,y=OB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_OB_vol_model,col="Blue")

OB_vol_model_upper <- lm(volume_m3 ~ z,data=OB_sa_upper)
summary(OB_vol_model_upper) #vol_m3 = 187z + 36.33

#calculating change in area and volume on a daily timestep
OB_WL <- WL %>% 
  filter(Site_Name == "OB-SW") %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.5 & dly_mean_wtrlvl > 0, 
                           ((OB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                              (OB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (OB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                              (OB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                              OB_area_model$coefficients[1]),
                           if_else(dly_mean_wtrlvl >= 0.5,max(OB_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.5 & dly_mean_wtrlvl > 0,
                             ((OB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                                (OB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                                (OB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                                OB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.5,
                                     (OB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      OB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

max(OB_WL$delta_area,na.rm=TRUE)

#plot water level over time
OB_p1 <- WL %>% 
  filter(Site_Name == "OB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("OB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
OB_p2 <- ggplot(OB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("OB-SW daily wetland area")

#plot change in area over time
OB_p3 <- ggplot(OB_WL)+
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
OB_p4 <- ggplot(OB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("OB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


OB_p1 / OB_p2 / OB_p3 / OB_p4


#both
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),volume_m3,col="Volume")) +
  geom_line(aes(ymd(Date),area_m2,col="Area")) +
  ylab("Area (m2) and Volume (m3)")+
  xlab("Date")+
  scale_color_manual(name="Legend",
                     values=c("Volume" = "#F8766D", 
                              "Area" = "#00B8E7"))

#plot change in volume over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),delta_vol)) 

#both changes over time
ggplot(OB_WL )+
  geom_line(aes(ymd(Date),delta_vol,col="Volume")) +
  geom_line(aes(ymd(Date),delta_area,col="Area")) +
  ylab("Delta Area (m2) and Delta Volume (m3)")+
  xlab("Date")+
  scale_color_manual(name="Legend",
                     values=c("Volume" = "#F8766D", 
                              "Area" = "#00B8E7"))

## QB-SW ------------------------------
#plot stage-area relationship
QB_sa <- sa_97 %>% filter(Site_ID == "QB-SW") 

QB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

QB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(QB_sa$area_m) #858 sq m @ z = 0.74

#filter to where there's breaks in the stage-area trend
QB_sa_lower <- sa_97 %>% filter(Site_ID == "QB-SW") %>% filter(z < 0.74)
QB_sa_upper <- sa_97 %>% filter(Site_ID == "QB-SW") %>% filter(z >= 0.74)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
QB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,10,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

QB_area_model <- lm(area_m ~ poly(z,10,raw=T), data = QB_sa_lower) 
summary(QB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.74,by=0.005)
predict_QB_area_model <- predict(QB_area_model,newdata = list(z = new_vals))
plot(x=QB_sa_lower$z,y=QB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_QB_area_model,col="Blue")

#stage - volume is 7th order polynomial when z < 0.74 and linear z >= 0.74
QB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,7,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.74
QB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

QB_vol_model_lower <- lm(volume_m3 ~ poly(z,7,raw=T),data=QB_sa_lower)
summary(QB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.74,by=0.005)
predict_QB_vol_model <- predict(QB_vol_model_lower,newdata = list(z = new_vals))
plot(x=QB_sa_lower$z,y=QB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_QB_vol_model,col="Blue")

QB_vol_model_upper <- lm(volume_m3 ~ z,data=QB_sa_upper)
summary(QB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
QB_WL <- WL %>% 
  filter(Site_Name == "QB-SW") %>% 
  #for area, if water level is <0.74, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.74 & dly_mean_wtrlvl > 0, 
                           ((QB_area_model$coefficients[11]*(dly_mean_wtrlvl^10)) + 
                            (QB_area_model$coefficients[10]*(dly_mean_wtrlvl^9)) + 
                            (QB_area_model$coefficients[9]*(dly_mean_wtrlvl^8)) + 
                            (QB_area_model$coefficients[8]*(dly_mean_wtrlvl^7)) + 
                            (QB_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) + 
                            (QB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (QB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (QB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (QB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (QB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             QB_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 0.74,max(QB_sa$area_m),0)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.74 & dly_mean_wtrlvl > 0,
                             ((QB_vol_model_lower$coefficients[8]*(dly_mean_wtrlvl^7)) +
                              (QB_vol_model_lower$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (QB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (QB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (QB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (QB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (QB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               QB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.74 ,
                                     (QB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      QB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
QB_p1 <- WL %>% 
  filter(Site_Name == "QB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("QB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
QB_p2 <- ggplot(QB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("HB-SW daily wetland area")

#plot change in area over time
QB_p3 <- ggplot(QB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("QB-SW daily change in wetland area")

#plot volume over time
QB_p4 <- ggplot(QB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("QB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


QB_p1 / QB_p2 / QB_p3

## TA-SW ------------------------------
#plot stage-area relationship
TA_sa <- sa_97 %>% filter(Site_ID == "TA-SW") 

TA_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

TA_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(TA_sa$area_m) #957 sq m @ z = 1.03

#filter to where there's breaks in the stage-area trend
TA_sa_lower <- sa_97 %>% filter(Site_ID == "TA-SW") %>% filter(z < 1.03)
TA_sa_upper <- sa_97 %>% filter(Site_ID == "TA-SW") %>% filter(z >= 1.03)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 8th order polynomial
TA_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,8,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

TA_area_model <- lm(area_m ~ poly(z,8,raw=T), data = TA_sa_lower) 
summary(TA_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=1.03,by=0.005)
predict_TA_area_model <- predict(TA_area_model,newdata = list(z = new_vals))
plot(x=TA_sa_lower$z,y=TA_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_TA_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 1.03 and linear z >= 1.03
TA_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 1.03
TA_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

TA_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=TA_sa_lower)
summary(TA_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=1.03,by=0.005)
predict_TA_vol_model <- predict(TA_vol_model_lower,newdata = list(z = new_vals))
plot(x=TA_sa_lower$z,y=TA_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_TA_vol_model,col="Blue")

TA_vol_model_upper <- lm(volume_m3 ~ z,data=TA_sa_upper)
summary(TA_vol_model_upper) 

#calculating change in area and volume on a daily timestep
TA_WL <- WL %>% 
  filter(Site_Name == "TA-SW") %>% 
  #for area, if water level is <1.03, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 1.03 & dly_mean_wtrlvl > 0, 
                           ((TA_area_model$coefficients[9]*(dly_mean_wtrlvl^8)) + 
                            (TA_area_model$coefficients[8]*(dly_mean_wtrlvl^7)) + 
                            (TA_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) + 
                            (TA_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (TA_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (TA_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (TA_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (TA_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             TA_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 1.03,max(TA_sa$area_m),0)),
         #for volume, if water level is <1.03, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 1.03 & dly_mean_wtrlvl > 0,
                             ((TA_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (TA_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (TA_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (TA_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (TA_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               TA_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 1.03,
                                     (TA_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      TA_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
TA_p1 <- WL %>% 
  filter(Site_Name == "TA-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("TA-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
TA_p2 <- ggplot(TA_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TA-SW daily wetland area")

#plot change in area over time
TA_p3 <- ggplot(TA_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TA-SW daily change in wetland area")

#plot volume over time
TA_p4 <- ggplot(TA_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("TA-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


TA_p1 / TA_p2 / TA_p3

## TB-SW ------------------------------
#plot stage-area relationship
TB_sa <- sa_97 %>% filter(Site_ID == "TB-SW") 

TB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

TB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(TB_sa$area_m) #1165 sq m @ z = 0.8

#filter to where there's breaks in the stage-area trend
TB_sa_lower <- sa_97 %>% filter(Site_ID == "TB-SW") %>% filter(z < 0.8)
TB_sa_upper <- sa_97 %>% filter(Site_ID == "TB-SW") %>% filter(z >= 0.8)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 6th order polynomial
TB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,6,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

TB_area_model <- lm(area_m ~ poly(z,6,raw=T), data = TB_sa_lower) 
summary(TB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.8,by=0.005)
predict_TB_area_model <- predict(TB_area_model,newdata = list(z = new_vals))
plot(x=TB_sa_lower$z,y=TB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_TB_area_model,col="Blue")

#stage - volume is 6th order polynomial when z < 0.8and linear z >= 0.8
TB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,6,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.8
TB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

TB_vol_model_lower <- lm(volume_m3 ~ poly(z,6,raw=T),data=TB_sa_lower)
summary(TB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.8,by=0.005)
predict_TB_vol_model <- predict(TB_vol_model_lower,newdata = list(z = new_vals))
plot(x=TB_sa_lower$z,y=TB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_TB_vol_model,col="Blue")

TB_vol_model_upper <- lm(volume_m3 ~ z,data=TB_sa_upper)
summary(TB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
TB_WL <- WL %>% 
  filter(Site_Name == "TB-SW") %>% 
  #for area, if water level is <0.8, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.8 & dly_mean_wtrlvl > 0, 
                           ((TB_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) + 
                            (TB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (TB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (TB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (TB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (TB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             TB_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 0.8,max(TB_sa$area_m),0)),
         #for volume, if water level is <0.8, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.8 & dly_mean_wtrlvl > 0,
                             ((TB_vol_model_lower$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (TB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (TB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (TB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (TB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (TB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               TB_vol_model_lower$coefficients[1]),
                            if_else(dly_mean_wtrlvl >= 0.8,
                                    (TB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      TB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
TB_p1 <- WL %>% 
  filter(Site_Name == "TB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("TB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
TB_p2 <- ggplot(TB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TB-SW daily wetland area")

#plot change in area over time
TB_p3 <- ggplot(TB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TB-SW daily change in wetland area")

#plot volume over time
TB_p4 <- ggplot(TB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("TB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


TB_p1 / TB_p2 / TB_p3

## TI-SW ------------------------------
#plot stage-area relationship
TI_sa <- sa_97 %>% filter(Site_ID == "TI-SW") 

TI_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

TI_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(TI_sa$area_m) #2330 sq m @ z = 0.45

#filter to where there's breaks in the stage-area trend
TI_sa_lower <- sa_97 %>% filter(Site_ID == "TI-SW") %>% filter(z < 0.45)
TI_sa_upper <- sa_97 %>% filter(Site_ID == "TI-SW") %>% filter(z >= 0.45)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 11th order polynomial
TI_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,11,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 12))

TI_area_model <- lm(area_m ~ poly(z,11,raw=T), data = TI_sa_lower) 
summary(TI_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.45,by=0.005)
predict_TI_area_model <- predict(TI_area_model,newdata = list(z = new_vals))
plot(x=TI_sa_lower$z,y=TI_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_TI_area_model,col="Blue")

#stage - volume is 7th order polynomial when z < 0.45 and linear z >= 0.45
TI_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,7,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.45
TI_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

TI_vol_model_lower <- lm(volume_m3 ~ poly(z,7,raw=T),data=TI_sa_lower)
summary(TI_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.45,by=0.005)
predict_TI_vol_model <- predict(TI_vol_model_lower,newdata = list(z = new_vals))
plot(x=TI_sa_lower$z,y=TI_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_TI_vol_model,col="Blue")

TI_vol_model_upper <- lm(volume_m3 ~ z,data=TI_sa_upper)
summary(TI_vol_model_upper) 

#calculating change in area and volume on a daily timestep
TI_WL <- WL %>% 
  filter(Site_Name == "TI-SW") %>% 
  #for area, if water level is <0.45, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.45 & dly_mean_wtrlvl > 0, 
                           ((TI_area_model$coefficients[12]*(dly_mean_wtrlvl^11)) + 
                            (TI_area_model$coefficients[11]*(dly_mean_wtrlvl^10)) + 
                            (TI_area_model$coefficients[10]*(dly_mean_wtrlvl^9)) + 
                            (TI_area_model$coefficients[9]*(dly_mean_wtrlvl^8)) + 
                            (TI_area_model$coefficients[8]*(dly_mean_wtrlvl^7)) + 
                            (TI_area_model$coefficients[7]*(dly_mean_wtrlvl^6)) + 
                            (TI_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (TI_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (TI_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (TI_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (TI_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             TI_area_model$coefficients[1]),
                        if_else(dly_mean_wtrlvl >= 0.45,max(TI_sa$area_m),0)),
         #for volume, if water level is <0.45, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.45 & dly_mean_wtrlvl > 0,
                             ((TI_vol_model_lower$coefficients[8]*(dly_mean_wtrlvl^7)) +
                              (TI_vol_model_lower$coefficients[7]*(dly_mean_wtrlvl^6)) +
                              (TI_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (TI_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (TI_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (TI_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (TI_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               TI_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.45,
                                     (TI_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      TI_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
TI_p1 <- WL %>% 
  filter(Site_Name == "TI-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("TI-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
TI_p2 <- ggplot(TI_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TI-SW daily wetland area")

#plot change in area over time
TI_p3 <- ggplot(TI_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("TI-SW daily change in wetland area")

#plot volume over time
TI_p4 <- ggplot(TI_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("TB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


TI_p1 / TI_p2 / TI_p3

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

## XB-SW ------------------------------
#plot stage-area relationship
XB_sa <- sa_97 %>% filter(Site_ID == "XB-SW") 

XB_sa %>% 
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  theme(axis.text = element_text(size = 14))

XB_sa %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")

#find max area
max(XB_sa$area_m) #191 sq m @ z = 0.29

#filter to where there's breaks in the stage-area trend
XB_sa_lower <- sa_97 %>% filter(Site_ID == "XB-SW") %>% filter(z < 0.29)
XB_sa_upper <- sa_97 %>% filter(Site_ID == "XB-SW") %>% filter(z >= 0.29)

#fit equation to stage-area and stage-volume relationships
#stage - area looks like 5th order polynomial
XB_sa_lower %>%
  ggplot(aes(z,area_m))+
  geom_point(size=2)+
  xlab("Water Depth (m)")+
  ylab("Area (m2)")+
  geom_smooth(method = 'glm',
              formula = y ~ poly(x,5,raw=T),
              se = FALSE)+
  theme(axis.text = element_text(size = 14))

XB_area_model <- lm(area_m ~ poly(z,5,raw=T), data = XB_sa_lower) 
summary(XB_area_model)

#check area model against real data to ensure fit good
new_vals <- seq(from=0,to=0.29,by=0.005)
predict_XB_area_model <- predict(XB_area_model,newdata = list(z = new_vals))
plot(x=XB_sa_lower$z,y=XB_sa_lower$area_m,cex=1,pch=19)
points(x=new_vals,y=predict_XB_area_model,col="Blue")

#stage - volume is 5th order polynomial when z < 0.29 and linear z >= 0.29
XB_sa_lower %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'glm',formula = y ~ poly(x,5,raw=T),se=FALSE)

#stage-volume is linear when z >= 0.29
XB_sa_upper %>% 
  ggplot(aes(z,volume_m3))+
  geom_point()+
  xlab("Water Depth (m)")+
  ylab("Volume (m3)")+
  geom_smooth(method = 'lm',formula = y ~ x,se=FALSE)

XB_vol_model_lower <- lm(volume_m3 ~ poly(z,5,raw=T),data=XB_sa_lower)
summary(XB_vol_model_lower) 

#check vol model against real data to ensure fit good
new_vals <- seq(from=0,to=0.29,by=0.005)
predict_XB_vol_model <- predict(XB_vol_model_lower,newdata = list(z = new_vals))
plot(x=XB_sa_lower$z,y=XB_sa_lower$volume_m3,cex=1,pch=19)
points(x=new_vals,y=predict_XB_vol_model,col="Blue")

XB_vol_model_upper <- lm(volume_m3 ~ z,data=XB_sa_upper)
summary(XB_vol_model_upper) 

#calculating change in area and volume on a daily timestep
XB_WL <- WL %>% 
  filter(Site_Name == "XB-SW") %>% 
  
  #for area, if water level is <0.29, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.29 & dly_mean_wtrlvl > 0, 
                           ((XB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (XB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (XB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (XB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (XB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             XB_area_model$coefficients[1]),
                         if_else(dly_mean_wtrlvl >= 0.29,max(XB_sa$area_m),0)),
         #for volume, if water level is <0.29, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.29 & dly_mean_wtrlvl > 0,
                             ((XB_vol_model_lower$coefficients[6]*(dly_mean_wtrlvl^5)) +
                              (XB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (XB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (XB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (XB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               XB_vol_model_lower$coefficients[1]),
                             if_else(dly_mean_wtrlvl >= 0.29,
                                     (XB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                                      XB_vol_model_upper$coefficients[1],0)),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

#plot water level over time
XB_p1 <- WL %>% 
  filter(Site_Name == "XB-SW") %>%
  ggplot()+
  geom_line(aes(ymd(Date),dly_mean_wtrlvl))+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Water level (m)")+
  xlab("Date")+
  ggtitle("XB-SW daily wetland water level")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))

#plot area over time
XB_p2 <- ggplot(XB_WL)+
  geom_line(aes(ymd(Date),area_m2))+ 
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("XB-SW daily wetland area")

#plot change in area over time
XB_p3 <- ggplot(XB_WL)+
  geom_line(aes(ymd(Date),delta_area))+
  ylab("Delta Area (m2)")+
  xlab("Date")+  
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("XB-SW daily change in wetland area")

#plot volume over time
XB_p4 <- ggplot(XB_WL )+
  geom_line(aes(ymd(Date),volume_m3)) +
  ylab("Volume (m3)")+
  xlab("Date")+  
  ggtitle("XB-SW daily volume")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))


XB_p1 / XB_p2 / XB_p3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Plot all sites together -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Focus in on dates when all wetland water level loggers were online (after 2021-02)

## 4.1 All sites together -----------------------
#Long term Jackson Lane precip
JacksonPrecip <- read_csv("Jackson_Lane_precip_2018_2021.csv")
JL_Daily <- JacksonPrecip %>% mutate(day = cut(timestamp,breaks="day")) %>% 
  group_by(day) %>% 
  summarise(Jackson_Daily_mm = sum(precip_mm))
JL_rain <- JL_Daily %>% 
  ggplot(aes(ymd(day),Jackson_Daily_mm))+
  geom_bar(stat="identity")+
  ggtitle("Jackson Ln Daily Precip")+
  xlim(ymd("2019-12-31"),ymd("2022-04-11"))+
  #xlim(ymd("2021-08-05"),ymd("2021-08-12"))+
  ylab("Daily Precip (mm)")+
  xlab("Date")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

#All SW plot
ALL_WL <- SW_Clean %>% 
  filter(Site_Name != "DF-SW") %>% 
  filter(Site_Name != "FN-SW") %>% 
  #filter(Date > ymd("2021-02-01")) %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

JL_rain / ALL_WL


#All daily wetland area
ggplot()+
  geom_line(aes(ymd(Date),area_m2,col="BD"),data=BD_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="DB"),data=DB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="DK"),data=DK_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="HB"),data=HB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="JA"),data=JA_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="JB"),data=JB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="JC"),data=JC_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="MB"),data=MB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="NB"),data=NB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="ND"),data=ND_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="OB"),data=OB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="QB"),data=QB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TA"),data=TA_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TB"),data=TB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TI"),data=TI_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TS"),data=TS_WL)+
  geom_line(aes(ymd(Date),area_m2,col="XB"),data=XB_WL)+ 
  xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18),
        legend.text = element_text(size=16))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#88CCEE",
                              "DB" = "#CC6677",
                              "DK" = "#DDCC77",
                              "HB" = "#117733",
                              "JA" = "#332288",
                              "JB" = "#AA4499",
                              "JC" = "#44AA99",
                              "MB" = "#999933",
                              "NB" = "#882255",
                              "ND" = "#661100",
                              "OB" = "#6699CC",
                              "QB" = "#888888",
                              "TA" = "#00C19A",
                              "TB" = "#FF68A1",
                              "TI" = "#0CB702",
                              "TS" = "#00A9FF",
                              "XB" = "#C77CFF"))

#All daily wetland area normalized to max area
ggplot()+
  geom_line(aes(ymd(Date),area_m2/max(BD_sa$area_m),col="BD"),data=BD_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(DB_sa$area_m),col="DB"),data=DB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(DK_sa$area_m),col="DK"),data=DK_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(HB_sa$area_m),col="HB"),data=HB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(JA_sa$area_m),col="JA"),data=JA_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(JB_sa$area_m),col="JB"),data=JB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(JC_sa$area_m),col="JC"),data=JC_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(MB_sa$area_m),col="MB"),data=MB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(NB_sa$area_m),col="NB"),data=NB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(ND_sa$area_m),col="ND"),data=ND_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(OB_sa$area_m),col="OB"),data=OB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(QB_sa$area_m),col="QB"),data=QB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(TA_sa$area_m),col="TA"),data=TA_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(TB_sa$area_m),col="TB"),data=TB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(TI_sa$area_m),col="TI"),data=TI_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(TS_sa$area_m),col="TS"),data=TS_WL)+
  geom_line(aes(ymd(Date),area_m2/max(XB_sa$area_m),col="XB"),data=XB_WL)+ 
  ylab("Area (m^2) / Max Area (m^2)")+
  xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18),
        legend.text = element_text(size=16))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#88CCEE",
                              "DB" = "#CC6677",
                              "DK" = "#DDCC77",
                              "HB" = "#117733",
                              "JA" = "#332288",
                              "JB" = "#AA4499",
                              "JC" = "#44AA99",
                              "MB" = "#999933",
                              "NB" = "#882255",
                              "ND" = "#661100",
                              "OB" = "#6699CC",
                              "QB" = "#888888",
                              "TA" = "#00C19A",
                              "TB" = "#FF68A1",
                              "TI" = "#0CB702",
                              "TS" = "#00A9FF",
                              "XB" = "#C77CFF"))

#All daily change in wetland area
ggplot()+
  geom_line(aes(ymd(Date),delta_area,col="BD"),data=BD_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="DB"),data=DB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="DK"),data=DK_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="HB"),data=HB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="JA"),data=JA_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="JB"),data=JB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="JC"),data=JC_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="MB"),data=MB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="NB"),data=NB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="ND"),data=ND_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="OB"),data=OB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="QB"),data=QB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TA"),data=TA_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TB"),data=TB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TI"),data=TI_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TS"),data=TS_WL)+
  geom_line(aes(ymd(Date),delta_area,col="XB"),data=XB_WL)+ 
  xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  #ylim(-100,100)+
  ylab("Change in Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18),
        legend.text = element_text(size=16))+
  ggtitle("Daily change in wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#88CCEE",
                              "DB" = "#CC6677",
                              "DK" = "#DDCC77",
                              "HB" = "#117733",
                              "JA" = "#332288",
                              "JB" = "#AA4499",
                              "JC" = "#44AA99",
                              "MB" = "#999933",
                              "NB" = "#882255",
                              "ND" = "#661100",
                              "OB" = "#6699CC",
                              "QB" = "#888888",
                              "TA" = "#00C19A",
                              "TB" = "#FF68A1",
                              "TI" = "#0CB702",
                              "TS" = "#00A9FF",
                              "XB" = "#C77CFF"))

## 4.2 Break up by catchment -----------------------
#Also add in precip for comparison
#read in Jackson Lane / Jones Rd precip data from Michael W (2021 water year)
DMVPrecip <- read_csv("2021_WaterYear_Precip_Cleaned.csv")

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
  #xlim(ymd("2020-01-01"),ymd("2022-04-11"))+
  #xlim(ymd("2021-08-05"),ymd("2021-08-12"))+
  ylab("Precp (mm)")+
  xlab("Date")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))
 
Jones_Day <- Daily %>% 
  ggplot(aes(ymd(day),Jones_Daily_mm))+
  geom_bar(stat="identity")+
  ggtitle("Jones Rd Daily Precip")+
  xlim(ymd("2021-04-01"),ymd("2022-04-11"))+
  ylab("Precp (mm)")+
  xlab("Date")+
  ylim(0,120)+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))

Jack_Day / Jones_Day


#JL
JL <- SW_Clean %>% 
  filter(Site_Name %in% c("BD-SW","DK-SW","ND-SW","TS-SW")) 
  

### 4.2.1 Jackson Lane -------------------------------
#All SW plot
JL_SW <- SW_Clean %>% 
  filter(Site_Name %in% c("BD-SW","DK-SW","ND-SW","TS-SW")) %>% 
  #filter(Date > ymd("2021-08-05") & Date < ymd("2021-08-12")) %>% 
  #filter(Date > ymd("2021-02-01")) %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  ggtitle("Jackson Lane Daily Water Level")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=16))

#All daily wetland area
JL_day_area <- ggplot()+
  geom_line(aes(ymd(Date),area_m2,col="BD"),data=BD_WL)+
  geom_line(aes(ymd(Date),area_m2,col="DK"),data=DK_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="ND"),data=ND_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TS"),data=TS_WL)+ 
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  xlim(ymd("2021-08-05"),ymd("2021-08-12"))+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 16),
        legend.text = element_text(size=16))+
  ggtitle("Jackson Lane daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#F8766D",
                              "DK" = "#7CAE00",
                              "ND" = "#00B8E7",
                              "TS" = "#C77CFF"))
#zoom in on wet up event
ggplot()+
  geom_line(aes(ymd(Date),area_m2,col="BD"),data=BD_WL,size=1.25)+
  geom_line(aes(ymd(Date),area_m2,col="DK"),data=DK_WL,size=1.25)+ 
  geom_line(aes(ymd(Date),area_m2,col="ND"),data=ND_WL,size=1.25)+ 
  geom_line(aes(ymd(Date),area_m2,col="TS"),data=TS_WL,size=1.25)+
  xlim(ymd("2021-08-05"),ymd("2021-08-12"))+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 16),
        legend.text = element_text(size=16))+
  ggtitle("Jackson Lane daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#F8766D",
                              "DK" = "#7CAE00",
                              "ND" = "#00B8E7",
                              "TS" = "#C77CFF"))

#All daily wetland area normalized to max area
JL_norm <- ggplot()+
  geom_line(aes(ymd(Date),area_m2/max(BD_sa$area_m),col="BD"),data=BD_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(DK_sa$area_m),col="DK"),data=DK_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(ND_sa$area_m),col="ND"),data=ND_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(TS_sa$area_m),col="TS"),data=TS_WL)+
  ylab("Area (m^2) / Max Area (m^2)")+
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  #xlim(ymd("2021-08-05"),ymd("2021-08-12"))+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 16),
        legend.text = element_text(size=16))+
  ggtitle("Jackson Lane daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#F8766D",
                              "DK" = "#7CAE00",
                              "ND" = "#00B8E7",
                              "TS" = "#C77CFF"))

#All daily change in wetland area
JL_change <- ggplot()+
  geom_line(aes(ymd(Date),delta_area,col="BD"),data=BD_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="DK"),data=DK_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="ND"),data=ND_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TS"),data=TS_WL)+ 
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  #xlim(ymd("2021-08-05"),ymd("2021-08-12"))+
  #ylim(-100,100)+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 16),
        legend.text = element_text(size=16))+
  ggtitle("Daily change in wetland area")+
  scale_color_manual(name="Legend",
                     values=c("BD" = "#F8766D",
                              "DK" = "#7CAE00",
                              "ND" = "#00B8E7",
                              "TS" = "#C77CFF"))
Jack_Day / JL_SW / JL_day_area / JL_norm / JL_change

### 4.2.2 Baltimore Corner -----------------------

#All SW plot
BC_WL <- SW_Clean %>% 
  filter(Site_Name %in% c("HB-SW","MB-SW","OB-SW","XB-SW")) %>% 
  #filter(Date > ymd("2021-02-01")) %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=16))

#All daily wetland area
BC_area <- ggplot()+ 
  geom_line(aes(ymd(Date),area_m2,col="HB"),data=HB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="MB"),data=MB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="OB"),data=OB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="XB"),data=XB_WL)+ 
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18),
        legend.text = element_text(size=16))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c(
                              "HB" = "#F8766D",
                              "MB" = "#7CAE00",
                              "OB" = "#00B8E7",
                              "XB" = "#C77CFF"))

#All daily wetland area normalized to max area
BC_norm <- ggplot()+
  geom_line(aes(ymd(Date),area_m2/max(HB_sa$area_m),col="HB"),data=HB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(MB_sa$area_m),col="MB"),data=MB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(OB_sa$area_m),col="OB"),data=OB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(XB_sa$area_m),col="XB"),data=XB_WL)+ 
  ylab("Area (m^2) / Max Area (m^2)")+
 #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18),
        legend.text = element_text(size=16))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("HB" = "#F8766D",
                              "MB" = "#7CAE00",
                              "OB" = "#00B8E7",
                              "XB" = "#C77CFF"))

#All daily change in wetland area
BC_change <- ggplot()+
  geom_line(aes(ymd(Date),delta_area,col="HB"),data=HB_WL)+  
  geom_line(aes(ymd(Date),delta_area,col="MB"),data=MB_WL)+  
  geom_line(aes(ymd(Date),delta_area,col="OB"),data=OB_WL)+
  geom_line(aes(ymd(Date),delta_area,col="XB"),data=XB_WL)+ 
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  #ylim(-100,100)+
  ylab("Change in Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18),
        legend.text = element_text(size=16))+
  ggtitle("Daily change in wetland area")+
  scale_color_manual(name="Legend",
                     values=c("HB" = "#F8766D",
                              "MB" = "#7CAE00",
                              "OB" = "#00B8E7",
                              "XB" = "#C77CFF"))

BC_WL / BC_area / BC_norm / BC_change

### 4.2.3 Baltimore Corner Extras ------------------------
#All SW plot
BCE_WL <- SW_Clean %>% 
  filter(Site_Name %in% c("QB-SW","TI-SW","JA-SW","JB-SW","JC-SW","NB-SW" )) %>% 
  #filter(Date > ymd("2021-02-01")) %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=16))

#All daily wetland area
BCE_area <- ggplot()+ 
  geom_line(aes(ymd(Date),area_m2,col="JA"),data=JA_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="JB"),data=JB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="JC"),data=JC_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="NB"),data=NB_WL)+  
  geom_line(aes(ymd(Date),area_m2,col="QB"),data=QB_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TI"),data=TI_WL)+  
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("JA" = "#F8766D",
                              "JB" = "#CD9600",
                              "JC" = "#0CB702",
                              "NB" = "#00BFC4",
                              "QB" = "#00B8E7",
                              "TI" = "#FF61CC"))

#All daily wetland area normalized to max area
BCE_norm <- ggplot()+
  geom_line(aes(ymd(Date),area_m2/max(JA_sa$area_m),col="JA"),data=JA_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(JB_sa$area_m),col="JB"),data=JB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(JC_sa$area_m),col="JC"),data=JC_WL)+  
  geom_line(aes(ymd(Date),area_m2/max(NB_sa$area_m),col="NB"),data=NB_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(QB_sa$area_m),col="QB"),data=QB_WL)+
  geom_line(aes(ymd(Date),area_m2/max(TI_sa$area_m),col="TI"),data=TI_WL)+ 
  ylab("Area (m^2) / Max Area (m^2)")+
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("JA" = "#F8766D",
                              "JB" = "#CD9600",
                              "JC" = "#0CB702",
                              "NB" = "#00BFC4",
                              "QB" = "#00B8E7",
                              "TI" = "#FF61CC"))

#All daily change in wetland area
BCE_change <- ggplot()+ 
  geom_line(aes(ymd(Date),delta_area,col="JA"),data=JA_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="JB"),data=JB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="JC"),data=JC_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="NB"),data=NB_WL)+  
  geom_line(aes(ymd(Date),delta_area,col="QB"),data=QB_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TI"),data=TI_WL)+  
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  #ylim(-100,100)+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("Daily change in wetland area")+
  scale_color_manual(name="Legend",
                     values=c("JA" = "#F8766D",
                              "JB" = "#CD9600",
                              "JC" = "#0CB702",
                              "NB" = "#00BFC4",
                              "QB" = "#00B8E7",
                              "TI" = "#FF61CC"))

BCE_WL / BCE_area / BCE_norm / BCE_change

### 4.2.4 Bee Tree -------------------------------
#All SW plot
BT_WL <- SW_Clean %>% 
  filter(Site_Name %in% c("DB-SW","TA-SW","TB-SW")) %>% 
  #filter(Date > ymd("2021-02-01")) %>% 
  ggplot(aes(Date,dly_mean_wtrlvl,col=Site_Name))+
  geom_line()+
  ylab("Daily Mean Water Level (m)")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=16))

#All daily wetland area
BT_area <- ggplot()+
  geom_line(aes(ymd(Date),area_m2,col="DB"),data=DB_WL)+  
  geom_line(aes(ymd(Date),area_m2,col="TA"),data=TA_WL)+ 
  geom_line(aes(ymd(Date),area_m2,col="TB"),data=TB_WL)+ 
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("DB" = "#F8766D",
                              "TA" = "#0CB702",
                              "TB" = "#00A9FF"))

#All daily wetland area normalized to max area
BT_norm <- ggplot()+
  geom_line(aes(ymd(Date),area_m2/max(DB_sa$area_m),col="DB"),data=DB_WL)+  
  geom_line(aes(ymd(Date),area_m2/max(TA_sa$area_m),col="TA"),data=TA_WL)+ 
  geom_line(aes(ymd(Date),area_m2/max(TB_sa$area_m),col="TB"),data=TB_WL)+  
  ylab("Area (m^2) / Max Area (m^2)")+
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("Daily wetland area")+
  scale_color_manual(name="Legend",
                     values=c("DB" = "#F8766D",
                              "TA" = "#0CB702",
                              "TB" = "#00A9FF"))

#All daily change in wetland area
BT_change <- ggplot()+
  geom_line(aes(ymd(Date),delta_area,col="DB"),data=DB_WL)+
  geom_line(aes(ymd(Date),delta_area,col="TA"),data=TA_WL)+ 
  geom_line(aes(ymd(Date),delta_area,col="TB"),data=TB_WL)+
  #xlim(ymd("2021-02-01"),ymd("2022-04-11"))+
  #ylim(-100,100)+
  ylab("Area (m2)")+
  xlab("Date")+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        title = element_text(size = 18))+
  ggtitle("Daily change in wetland area")+
  scale_color_manual(name="Legend",
                     values=c("DB" = "#F8766D",
                              "TA" = "#0CB702",
                              "TB" = "#00A9FF"))

BT_WL / BT_area / BT_norm / BT_change 
