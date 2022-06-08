#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Synoptic Data Exploration
#Coder: Katie Wardinski
#Created: 2022-06-02
#Purpose: Get familiar with DOC trends in data to assist with PhD planning
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

#2.1 DOC over time by sample type ----------
summary <- df %>% 
  mutate(Date_Month = ym(Date_M)) %>%
  group_by(Type,Date_Month) %>% 
  summarize(mean = mean(NPOC_mgC_L, na.rm = T),
            lwr  = mean - sd(NPOC_mgC_L, na.rm = T)/sqrt(n()), 
            upr  = mean + sd(NPOC_mgC_L, na.rm = T)/sqrt(n())) %>%
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
  ylab("DOC (mg C /L)")+
  xlab(element_blank())+
  theme(axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 16))

#boxplot by date ****like this one
df %>%
  ggplot(aes(factor(Date_M),NPOC_mgC_L,fill=Type))+
  geom_boxplot()+
  ylab("DOC (mg C/L)")+
  xlab(element_blank())+
  theme_classic()+
  theme(axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 10))

#filter to just one property
df %>% 
  filter(property == "Jackson Lane") %>% 
  ggplot(aes(ym(Date_M),NPOC_mgC_L,col=Type))+
  geom_point()+
  theme_classic()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Wetland Order -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#number of samples for each wetland order
site %>%
  drop_na(Site) %>% 
  ggplot(aes(wet_order))+
  geom_histogram()+
  theme_classic()+
  ylab("Count")+
  scale_x_discrete(name ="Wetland Order", 
                   limits=c("1","2","3","4","5","6","7"))

#3.1 DOC versus wetland order ----------------
#Boxplot
df %>%
  drop_na(wet_order) %>% 
  ggplot(aes(factor(wet_order),NPOC_mgC_L))+
  geom_boxplot()+
  theme_classic()
#Boxplot by type
df %>%
  drop_na(wet_order) %>% 
  ggplot(aes(factor(wet_order),NPOC_mgC_L,fill=Type))+
  geom_boxplot()+
  theme_classic()+
  ylab("DOC (mg C/L)")+
  xlab("Wetland Order")


df %>%
  drop_na(wet_order) %>% 
  ggplot(aes(wet_order,NPOC_mgC_L,col=SW_GW))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_classic()

df %>%
  drop_na(wet_order) %>% 
  ggplot(aes(factor(wet_order),NPOC_mgC_L,col=property))+
  geom_point()+
  theme_classic()

#SW only
df %>%
  drop_na(wet_order) %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(factor(wet_order),NPOC_mgC_L,col=property))+
  geom_point()+
  theme_classic()

#filter to just one month
df %>% 
  filter(Date_M == 202203) %>% 
  drop_na(wet_order) %>% 
  ggplot(aes(wet_order,NPOC_mgC_L,col=property))+
  geom_point()+
  theme_classic()

#filter to just one property
df %>% 
  filter(property == "Jackson Lane") %>% 
  ggplot(aes(wet_order,NPOC_mgC_L,col=Type))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_classic()+
  ggtitle("Jackson Lane Catchment - All Dates")

#one property and one month
df %>% 
  filter(property == "Jackson Lane") %>% 
  filter(Date_M == 202203) %>% 
  ggplot(aes(wet_order,NPOC_mgC_L,col=Type))+
  geom_point()+
  theme_classic()+
  ggtitle("Jackson Lane Catchment - March 2022")

df %>% 
  filter(property == "Jackson Lane") %>% 
  filter(Date_M == 202203) %>% 
  ggplot(aes(wet_order,NPOC_mgC_L))+
  geom_point()+
  geom_smooth(method='lm')+ 
  theme_classic()+
  ggtitle("Jackson Lane Catchment - March 2022")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Wetland size -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df %>%
  drop_na(wetland_storage_volume_m3) %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(wetland_storage_volume_m3,NPOC_mgC_L,col=Site))+
  geom_point()+
  theme_classic()

#storage volume
df %>%
  drop_na(wetland_storage_volume_m3) %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(wetland_storage_volume_m3,NPOC_mgC_L))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#wetland area
df %>%
  drop_na(area_m2) %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(area_m2,NPOC_mgC_L))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#watershed area
df %>%
  drop_na(watershed_area_m2) %>% 
  filter(SW_GW == "SW") %>% 
  ggplot(aes(watershed_area_m2,NPOC_mgC_L))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 DOC versus other variables ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOC vs TDN
synoptic %>%
  filter(Type != "River" & Type != "Channel") %>% 
  ggplot(aes(TDN_mgN_L,NPOC_mgC_L,col=SW_GW))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#DOC vs CO2
DOC_CO2 <- synoptic %>%
  filter(Type != "River" & Type != "Channel") %>% 
  ggplot(aes(NPOC_mgC_L,CO2,col=SW_GW))+
  geom_point()+
  ylab("CO2")+
  xlab(element_blank())+
  geom_smooth(method="lm")+
  stat_regline_equation()+
  stat_cor(label.x = 45)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

#DOC vs CH4
DOC_CH4 <- synoptic %>%
  filter(Type != "River" & Type != "Channel") %>% 
  ggplot(aes(NPOC_mgC_L,CH4,col=SW_GW))+
  geom_point()+
  ylab("CH4")+
  xlab("DOC (mg C/L")+
  geom_smooth(method="lm")+
  stat_regline_equation()+
  stat_cor(label.x = 45)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

DOC_CO2 / DOC_CH4
