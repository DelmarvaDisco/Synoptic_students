#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Plot Mar 22 GPS data
# Coder: James Maze
# Date: April 4th 2022
# Purpose: Check on well points and inundated area map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# Not sure whether I properly applied the CRS


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
area_gen <- st_read(paste0(data_dir, "Area_gen.shp"), crs = 2248) %>% 
  st_transform(.,p) %>% 
  st_zm(.)

points_ge <- st_read(paste0(data_dir, "Point_ge.shp"), crs = 2248) %>% 
  st_transform(.,p) %>% 
  st_zm(.)

generic <- st_read(paste0(data_dir, "Generic_.shp"), crs = 2248) %>% 
  st_transform(.,p) %>% 
  st_zm(.)

both <- st_join(area_gen, generic)

BC_flooded <- area_gen %>% 
  filter(Comment %in% c("OB-SW", "MB-SW", "HB-SW"))

JL_flooded <- area_gen %>% 
  filter(Comment %in% c("BD-SW", "TS-SW", "DK-SW", "ND-SW"))

BC_flooded %>% st_geometry() %>% plot()
JL_flooded %>% st_geometry() %>% plot()

# 3. Make a leaflet map ---------------------------------------------------
area_gen <- area_gen %>% 
  st_transform(., crs = 4326)

generic <- generic %>% 
  st_transform(., crs = 4326)

area_gen <- readOGR(paste0(data_dir, "Area_gen.shp"))

flooded_map <- leaflet(area_gen) %>% 
  addProviderTiles("Esri.WorldImagery", group = "ESRI") %>% 
  addPolygons(area_gen)

(flooded_map)



