# Delmarva water level data
# Merging and plotting water level time-series
# Carla López Lloreda
# Created 3/18/2022

library(tidyverse)
library(lubridate)
library(scales)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Water level")

# Read in all the water level files
WL_20200508 <- read_csv("output/output_20200508_JM.csv")
WL_20201015 <- read_csv("output/output_20201015_JM.csv")
WL_20210525 <- read_csv("output/output_20210525_JM.csv")

# Bind WL files
wl <- rbind(WL_20200508, WL_20201015, WL_20210525)

# Fix date

wl$Timestamp_corrected <- parse_date_time(wl$Timestamp, "Ymd HMS", truncated = 3)

# Add a sample type column so you can then subset only SW/GW

wl$Sample_Type <- substr(wl$Site_Name, start= 4, stop= 5)

wl_SW <- subset(wl, Sample_Type %in% "SW")

# Plotting only one site

ggplot(wl[wl$Site_Name %in% "ND-SW", ], aes(x= Timestamp, y= waterLevel)) +
  geom_point()

# Plot WL time-series for all the sites

ggplot(wl_SW, aes(x= Timestamp_corrected, y= waterLevel, color= Site_Name)) +
  geom_line() +
  theme(legend.position = "right") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"), date_breaks = "3 months")

# Plotting with on the same time frame as synoptic sampling
lims <- as.POSIXct(strptime(c("2020-11-30 00:00", "2021-09-11 00:00"), 
                            format = "%Y-%m-%d %H:%M"))

wl_graph <- ggplot(wl_SW, aes(x= Timestamp_corrected, y= waterLevel, color = Site_Name)) +
  geom_line() +
  theme(legend.position = "none", axis.text.y=element_text(size=12)) +
  scale_x_datetime(labels = date_format("%Y-%m-%d"), date_breaks = "3 months",
                   limits = lims,
                   expand = c(0, 0)) + 
  labs(x = "", y = "Water level (m)")

wl_graph

# To-do: 1) make same colors as CO2 and CH4 graphs & 2) fix date breaks
