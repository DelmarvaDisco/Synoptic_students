#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Synoptic Data Exploration
#Coder: Katie Wardinski
#Created: 2022-06-02
#Purpose: Get familiar with isotope trends in data to assist with PhD planning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)
library(patchwork)

#Read data
synoptic <-read_csv("SynopticCurrent.csv") #synoptic data through 2022-03
site <- read_csv("wetland_info.csv") #site info including wetland order

#Join in site info
df <- left_join(synoptic,site,by="Site")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Isotopes -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.1 Isotopes over time
#2.1.2 d2H over time colored by sample type ------------------
summary_H <- df %>% 
  mutate(Date_Month = ym(Date_M)) %>%
  group_by(Type,Date_Month) %>% 
  summarize(mean = mean(d2H_VSMOW, na.rm = T),
            lwr  = mean - sd(d2H_VSMOW, na.rm = T)/sqrt(n()), 
            upr  = mean + sd(d2H_VSMOW, na.rm = T)/sqrt(n())) %>%
 drop_na() 

Channel_H <- summary_H %>% filter(Type == "Channel")
SW_H <- summary_H %>% filter(Type == "Wetland SW")
GW_H <- summary_H %>% filter(Type == "Wetland GW")
River_H <- summary_H %>% filter(Type == "River")

#Define ribbon tranparency
ribbon_alpha<-0.80

#Define colors
cols<-c(
  'Channel' = '#045a8d', 
  'Wetland SW' = '#2b8cbe', 
  'Wetland GW' = '#74a9cf', 
  'River' = '#bdc9e1')

line_col<-"grey50"

#Start ggplot
d2H <- ggplot()+
  #Channel
  geom_ribbon(aes(ymin = Channel_H$lwr, 
                  ymax = Channel_H$upr, 
                  x = Channel_H$Date_Month, 
                  fill='Channel'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=Channel_H$Date_Month, 
                y=Channel_H$mean), 
            col=line_col) +
  #River
  geom_ribbon(aes(ymin = River_H$lwr, 
                  ymax = River_H$upr, 
                  x = River_H$Date_Month, 
                  fill='River'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=River_H$Date_Month, 
                y=River_H$mean), 
            col=line_col) +
  
  #Wetland SW
  geom_ribbon(aes(ymin = SW_H$lwr, 
                  ymax = SW_H$upr, 
                  x = SW_H$Date_Month, 
                  fill='Wetland SW'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=SW_H$Date_Month, 
                y=SW_H$mean), 
            col=line_col) +
  
  #Wetland GW
  geom_ribbon(aes(ymin = GW_H$lwr, 
                  ymax = GW_H$upr, 
                  x = GW_H$Date_Month, 
                  fill='Wetland GW'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=GW_H$Date_Month, 
                y=GW_H$mean), 
            col=line_col) +
  #set theme
  theme_classic()+
  ylab("d2H")+
  xlab(element_blank())+
  theme(axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 16))
  
#2.1.2 d18O over time colored by sample type ------------------
summary_O <- df %>% 
  mutate(Date_Month = ym(Date_M)) %>%
  group_by(Type,Date_Month) %>% 
  summarize(mean = mean(d18O_VSMOW, na.rm = T),
            lwr  = mean - sd(d18O_VSMOW, na.rm = T)/sqrt(n()), 
            upr  = mean + sd(d18O_VSMOW, na.rm = T)/sqrt(n())) %>%
  drop_na() 

Channel_O <- summary_O %>% filter(Type == "Channel")
SW_O <- summary_O %>% filter(Type == "Wetland SW")
GW_O <- summary_O %>% filter(Type == "Wetland GW")
River_O <- summary_O %>% filter(Type == "River")

#Define ribbon tranparency
ribbon_alpha<-0.80

#Define colors
cols<-c(
  'Channel' = '#045a8d', 
  'Wetland SW' = '#2b8cbe', 
  'Wetland GW' = '#74a9cf', 
  'River' = '#bdc9e1')

line_col<-"grey50"

#Start ggplot
d18O <- ggplot()+
  #Channel
  geom_ribbon(aes(ymin = Channel_O$lwr, 
                  ymax = Channel_O$upr, 
                  x = Channel_O$Date_Month, 
                  fill='Channel'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=Channel_O$Date_Month, 
                y=Channel_O$mean), 
            col=line_col) +
  #River
  geom_ribbon(aes(ymin = River_O$lwr, 
                  ymax = River_O$upr, 
                  x = River_O$Date_Month, 
                  fill='River'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=River_O$Date_Month, 
                y=River_O$mean), 
            col=line_col) +
  
  #Wetland SW
  geom_ribbon(aes(ymin = SW_O$lwr, 
                  ymax = SW_O$upr, 
                  x = SW_O$Date_Month, 
                  fill='Wetland SW'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=SW_O$Date_Month, 
                y=SW_O$mean), 
            col=line_col) +
  
  #Wetland GW
  geom_ribbon(aes(ymin = GW_O$lwr, 
                  ymax = GW_O$upr, 
                  x = GW_O$Date_Month, 
                  fill='Wetland GW'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=GW_O$Date_Month, 
                y=GW_O$mean), 
            col=line_col) +
  #set theme
  theme_classic()+
  ylab("d18O")+
  xlab(element_blank())+
  theme(axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 16))

d2H / d18O

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 d2H vs d18O -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data=df)+
  geom_point(aes(x=d18O_VSMOW,y=d2H_VSMOW,col=factor(Date_M),shape=Type),size=3) +
  ylab("D2H") +
  xlab("D18O") +
  ggtitle("D2H vs D18O")+
  theme_bw()
