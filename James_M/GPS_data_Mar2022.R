#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Plot Mar 22 GPS data
# Coder: James Maze
# Date: April 4th 2022
# Purpose: Check on well points and inundated area map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# !!! No crs on the files from Keystone. Not sure if I properly applied the right crs???


# 1. Packages and workspace -----------------------------------------------

remove(list = ls())
 
library(leaflet)
library(sf)
library(raster)
library(htmltools)
library(htmlwidgets)
library(tidyverse)

#Define data directory
data_dir <- "data/geospatial/"

#Define master projection
p <- "??????????????????"

# 2. Read data -----------------------------------------------------

#Read the points shapefiles
points_ge <- st_read(dsn = paste0(data_dir, "Point_ge.shp"), crs = 2248) %>%
  #Ughh different columns in points shapefiles
  rename("Flag_Label" = `Comment`)
generic <- st_read(dsn = paste0(data_dir, "Generic_.shp"), crs = 2248) %>% 
  #Ughh different columns in points shapefiles
  select(-Descriptio)

#Merge the points shapefiles
points_gen <- rbind(points_ge, generic)
#Clean up environment
rm(points_ge, generic)

#Read the area shapefiles
Area_gen <- st_read(dsn = paste0(data_dir, "Area_gen.shp"), crs = 2248)

# 3. Convert points from projection to lat/long ---------------------------------------------------

points <- st_transform(x = points_gen, crs = "+proj=longlat +datum=NAD83 +no_defs") %>% 
  as_tibble() %>% 
  mutate(long = str_extract(as.character(geometry),
                           #LOL I'm so bad at strings
                           pattern ="\\-\\d\\d\\.\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d"),
         lat = str_extract(as.character(geometry),
                           pattern = "\\,\\s\\d\\d\\.\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d")) %>% 
                           #Dumbest syntax ever
  mutate(lat = str_extract(lat, pattern = "\\d\\d\\.\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d"))




