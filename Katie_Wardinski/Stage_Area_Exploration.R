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

## 6.1 Look at all sites (preliminary) -------------
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
#3.0 Site Specific Stage-Area Relationships ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate relative area and volume changes for a given change in water level
#Note: I want to look into this raw=T vs raw=F and how it influences the lm model coefficients

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
  filter(dly_mean_wtrlvl >= 0) %>% 
  
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.5 & dly_mean_wtrlvl > 0, 
                           ((OB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (OB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (OB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (OB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             OB_area_model$coefficients[1]),
                             max(OB_sa$area_m)),
  #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
  #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.5 & dly_mean_wtrlvl > 0,
                             ((OB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (OB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (OB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               OB_vol_model_lower$coefficients[1]),
                             ((OB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                               OB_vol_model_upper$coefficients[1])),
  #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

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
  theme(axis.text = element_text(size = 14))

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
  filter(dly_mean_wtrlvl >= 0) %>% 
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 0.52 & dly_mean_wtrlvl > 0, 
                           ((BD_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (BD_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) +
                            (BD_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (BD_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (BD_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             BD_area_model$coefficients[1]),
                           max(BD_sa$area_m)),
         
  #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
  #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 0.52 & dly_mean_wtrlvl > 0,
                             ((BD_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                              (BD_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (BD_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (BD_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               BD_vol_model_lower$coefficients[1]),
                             ((BD_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                               BD_vol_model_upper$coefficients[1])),
         #calculate daily change in area and volume
         delta_area = area_m2 - lag(area_m2),
         delta_vol = volume_m3 - lag(volume_m3))

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

DB_vol_model_lower$coefficients[5]
DB_vol_model_lower$coefficients[4]
DB_vol_model_lower$coefficients[3]
DB_vol_model_lower$coefficients[2]
DB_vol_model_lower$coefficients[1]

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
  filter(dly_mean_wtrlvl >= 0) %>% 
  
  #for area, if water level is <0.5, use stage-area polynomial function, otherwise 
  #print the max area value
  mutate(area_m2 = if_else(dly_mean_wtrlvl < 1.08 & dly_mean_wtrlvl > 0, 
                           ((DB_area_model$coefficients[6]*(dly_mean_wtrlvl^5)) + 
                            (DB_area_model$coefficients[5]*(dly_mean_wtrlvl^4)) + 
                            (DB_area_model$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                            (DB_area_model$coefficients[3]*(dly_mean_wtrlvl^2)) +  
                            (DB_area_model$coefficients[2]*dly_mean_wtrlvl) +
                             DB_area_model$coefficients[1]),
                           max(DB_sa$area_m)),
         #for volume, if water level is <0.5, use stage-area polynomial function, otherwise
         #use the linear relationship
         volume_m3 = if_else(dly_mean_wtrlvl < 1.08 & dly_mean_wtrlvl > 0,
                             ((DB_vol_model_lower$coefficients[5]*(dly_mean_wtrlvl^4)) +
                              (DB_vol_model_lower$coefficients[4]*(dly_mean_wtrlvl^3)) + 
                              (DB_vol_model_lower$coefficients[3]*(dly_mean_wtrlvl^2)) +
                              (DB_vol_model_lower$coefficients[2]*(dly_mean_wtrlvl)) + 
                               DB_vol_model_lower$coefficients[1]),
                             ((DB_vol_model_upper$coefficients[2]*dly_mean_wtrlvl) + 
                               DB_vol_model_upper$coefficients[1])),
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

### DK-SW ------------------------------
### HB-SW ------------------------------
### JA-SW ------------------------------
### JB-SW ------------------------------
### JC-SW ------------------------------
### MB-SW ------------------------------
### NB-SW ------------------------------
### ND-SW ------------------------------
### QB-SW ------------------------------
### TA-SW ------------------------------
### TB-SW ------------------------------
### TI-SW ------------------------------
### TS-SW ------------------------------
### XB-SW ------------------------------