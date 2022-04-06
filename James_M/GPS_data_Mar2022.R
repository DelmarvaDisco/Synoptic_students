#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Plot Mar 22 GPS data
# Coder: James Maze
# Date: April 4th 2022
# Purpose: Check on well points and inundated area map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# Just seeing if this works. Should I use leaflet or something else?

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
p <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# 2. Read data -----------------------------------------------------

#Read in the area shapefiles
area_gen <- st_read(paste0(data_dir, "Area_gen.shp"))

points_ge <- st_read(paste0(data_dir, "Point_ge.shp"))

Generic <- st_read(paste0(data_dir, "Generic_.shp"))

flooded_map <- leaflet(Generic) %>% 
  addProviderTiles("Esri.WorldImagery", group = "ESRI")

(flooded_map)



