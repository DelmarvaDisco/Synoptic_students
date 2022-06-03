#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Synoptic Data Exploration
#Coder: Katie Wardinski
#Created: 2022-06-02
#Purpose: Get familiar with nutrient trends in data to assist with PhD planning
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

#Read data
synoptic <-read_csv("SynopticCurrent.csv") #synoptic data through 2022-03
site <- read_csv("wetland_info.csv") #site info including wetland order

#Join in site info
df <- left_join(synoptic,site,by="Site")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Temporal -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.1 Boxplots by sample type --------------
#NO3
df %>%
  ggplot(aes(factor(Date_M),NO3_mgN_L,fill=Type))+
  geom_boxplot()+
  theme_classic()

#no river
df %>%
  filter(Type != "River") %>% 
  ggplot(aes(factor(Date_M),NO3_mgN_L,fill=Type))+
  geom_boxplot()+
  theme_classic()

#NH3
df %>%
  ggplot(aes(factor(Date_M),NH3_mgN_L,fill=Type))+
  geom_boxplot()+
  theme_classic()

#TDN
df %>%
  ggplot(aes(factor(Date_M),TDN_mgN_L,fill=Type))+
  geom_boxplot()+
  theme_classic()

#oPO4
df %>%
  ggplot(aes(factor(Date_M),oPO4_mgP_L,fill=Type))+
  geom_boxplot()+
  theme_classic()

#TDP
df %>%
  ggplot(aes(factor(Date_M),TDP_mgP_L,fill=Type))+
  geom_boxplot()+
  theme_classic()

#2.1 TDN over time by sample type ----------
summary <- df %>% 
  mutate(Date_Month = ym(Date_M)) %>%
  group_by(Type,Date_Month) %>% 
  summarize(mean = mean(TDN_mgN_L, na.rm = T),
            lwr  = mean - sd(TDN_mgN_L, na.rm = T)/sqrt(n()), 
            upr  = mean + sd(TDN_mgN_L, na.rm = T)/sqrt(n())) %>%
  drop_na() 

Channel <- summary %>% filter(Type == "Channel")
SW <- summary %>% filter(Type == "Wetland SW")
GW <- summary %>% filter(Type == "Wetland GW")
River <- summary %>% filter(Type == "River")

#Define ribbon tranparency
ribbon_alpha<-0.90

#Define colors
cols<-c(
  'Channel' = '#045a8d', 
  'Wetland SW' = '#2b8cbe', 
  'Wetland GW' = '#74a9cf', 
  'River' = '#bdc9e1')

line_col<-"grey50"

#Start ggplot
ggplot()+
  #Channel
  geom_ribbon(aes(ymin = Channel$lwr, 
                  ymax = Channel$upr, 
                  x = Channel$Date_Month, 
                  fill='Channel'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=Channel$Date_Month, 
                y=Channel$mean), 
            col=line_col) +
  #River
  geom_ribbon(aes(ymin = River$lwr, 
                  ymax = River$upr, 
                  x = River$Date_Month, 
                  fill='River'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=River$Date_Month, 
                y=River$mean), 
            col=line_col) +
  
  #Wetland SW
  geom_ribbon(aes(ymin = SW$lwr, 
                  ymax = SW$upr, 
                  x = SW$Date_Month, 
                  fill='Wetland SW'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=SW$Date_Month, 
                y=SW$mean), 
            col=line_col) +
  
  #Wetland GW
  geom_ribbon(aes(ymin = GW$lwr, 
                  ymax = GW$upr, 
                  x = GW$Date_Month, 
                  fill='Wetland GW'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=GW$Date_Month, 
                y=GW$mean), 
            col=line_col) +
  #set theme
  theme_classic()+
  ylab("TDN (mg N /L)")+
  xlab(element_blank())+
  theme(axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 16))
