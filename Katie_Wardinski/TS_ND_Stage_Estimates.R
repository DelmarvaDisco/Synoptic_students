#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Stage Estimates
#Coder: Katie Wardinski
#Created: 2024-04-05
#Purpose: Use OW (Flux restored) stage data to estimate ND and TS stages 
#during rain event sampling in Dec 2023
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Read water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 2.1 Read water level data -----------------------------------
#daily mean water level 2019 through fall 2022 updated by Nick 
WL_2019_2022 <- read_csv("dly_mean_output_NC_2019_2022.csv") 

#daily mean water level Jackson lane only 2022 to 2023
WL_2022_2023 <- read_csv("JL_SW_dailyWL_2022_2023.csv")

#2019 - 2022 15 minute water level data
hf_WL_2019_2022 <- read_csv("output_JM_2019_2022.csv") 

#2022-23 15 minute water level data for ND and TS
ND_hf_WL_2022_2023 <- read_csv("ND-SW_WL2022_2023.csv")
TS_hf_WL_2022_2023 <- read_csv("TS-SW_WL2022_2023.csv")

## 2.2 Read in Flux restored OW and ND estimates from Michael W -----------------
OW_hf_WL_2021_2024 <- read_csv("OW_ND_stage_estimate_4-28-21_to_2-29-24.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Format water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#fix column names, date formate, and filter to ND/TS

#full data set 2019-2022
hf_WL_2019_2022b <- hf_WL_2019_2022 %>% 
  rename(Site_ID = Site_Name,
         Stage = waterLevel) %>% 
  filter(Site_ID %in% c("ND-SW","ND-UW1","ND-UW2","TS-SW","BD-CH","TS-UW1")) %>% 
  dplyr::select(Timestamp,Site_ID,Stage)

#ND-SW 2022-2023
ND_hf_WL_2022_2023b <- ND_hf_WL_2022_2023 %>%  
  dplyr::select(Timestamp,Site_ID,Stage)

ND_hf_WL_2022_2023b$Timestamp <- mdy_hm(ND_hf_WL_2022_2023b$Timestamp)

#TS-SW 2022-2023
TS_hf_WL_2022_2023b <- TS_hf_WL_2022_2023 %>% 
  dplyr::select(Timestamp,Site_ID,Stage)

TS_hf_WL_2022_2023b$Timestamp <- mdy_hm(TS_hf_WL_2022_2023b$Timestamp)

# Bind data together
hf_WLa <- rbind(hf_WL_2019_2022b,ND_hf_WL_2022_2023b)
hf_WL <- rbind(hf_WLa,TS_hf_WL_2022_2023b)

##******there's a gap between april and october 2022*****###

#Format Michael W data to match other high freq data
OW_hf_WL_2021_2024b <- OW_hf_WL_2021_2024 %>% 
  rename(Stage = OW_SW_stage_m) %>% dplyr::select(Timestamp,Stage) %>% mutate(Site_ID = "FR-OW")
OW_hf_WL_2021_2024b$Timestamp <- mdy_hm(OW_hf_WL_2021_2024b$Timestamp)

#Bind Michael OW stage data
hf_WL_OW <- rbind(hf_WL,OW_hf_WL_2021_2024b)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plot water level data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.1 All data ----------------------------------------------
hf_WL_OW %>% 
  ggplot(aes(Timestamp,Stage,col=Site_ID))+
  geom_line()+
  ylab("Water Level (m)")+
  xlab("")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

## 3.2 SW vs FR-OW relationships --------------------------------
hf_WL_OW_wide <- pivot_wider(hf_WL_OW,
                             names_from = Site_ID,
                             values_from = Stage)

colnames(hf_WL_OW_wide) <- c("Timestamp","BD_CH","ND_UW1","ND_UW2","ND_SW","TS_SW","TS_UW1","FR_OW")

hf_WL_OW_wide$Year <- format(hf_WL_OW_wide$Timestamp, format="%Y")
hf_WL_OW_wide$Month <- format(hf_WL_OW_wide$Timestamp, format="%Y-%m")

#ND-SW vs FR-SW
ND_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,ND_SW,FR_OW) %>% drop_na()

ggplot(data=ND_FR,aes(x=FR_OW,y=ND_SW,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("ND-SW (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS-SW vs FR-SW
TS_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,TS_SW,FR_OW) %>% drop_na()

ggplot(data=TS_FR,aes(x=FR_OW,y=TS_SW,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("TS-SW (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

## 3.3 GW vs FR-OW relationships ----------------------------------
#ND-UW vs FR-SW
ND_GW_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,ND_UW1,ND_UW2,FR_OW) %>% drop_na()

ggplot(data=ND_GW_FR,aes(x=FR_OW,y=ND_UW1,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW1 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

ggplot(data=ND_GW_FR,aes(x=FR_OW,y=ND_UW2,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW2 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#ND-UW vs FR-SW
TS_GW_FR <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,TS_UW1,BD_CH,FR_OW) %>% drop_na()

ggplot(data=TS_GW_FR,aes(x=FR_OW,y=BD_CH,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("TS-UW1 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

ggplot(data=TS_GW_FR,aes(x=FR_OW,y=TS_UW1,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.2)+
  stat_cor(label.x = 0.3)+
  ylab("TS-UW2 (m)")+
  xlab("FR-OW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

# 3.4 SW vs GW relationships --------------------------

#ND SW vs ND-UW1 and ND-UW2
ND_all <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,ND_SW,ND_UW1,ND_UW2) %>% drop_na()


ggplot(data=ND_all,aes(x=ND_SW,y=ND_UW1,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.1)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW1 (m)")+
  xlab("ND-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

ggplot(data=ND_all,aes(x=ND_SW,y=ND_UW2,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.1)+
  stat_cor(label.x = 0.3)+
  ylab("ND-UW2 (m)")+
  xlab("ND-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

#TS SW vs TS-UW1 and TS-UW2
TS_all <- hf_WL_OW_wide %>% 
  dplyr::select(Timestamp,Year,Month,TS_SW,BD_CH,TS_UW1) %>% drop_na()


ggplot(data=TS_all,aes(x=TS_SW,y=BD_CH,col=Year))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_regline_equation(label.x = 0.1)+
  stat_cor(label.x = 0.3)+
  ylab("TS-UW1 (m)")+
  xlab("TS-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))

ggplot(data=TS_all,aes(x=TS_SW,y=TS_UW1,col=Month))+
  geom_point()+
  #geom_smooth(method='lm')+
  #stat_regline_equation(label.x = 0.1)+
  #stat_cor(label.x = 0.3)+
  ylab("TS-UW2 (m)")+
  xlab("TS-SW (m)")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))
