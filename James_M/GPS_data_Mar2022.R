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

points_ge <- st_read(dsn = paste0(data_dir, "Point_ge.shp"), crs = 2248) 

generic <- st_read(dsn = paste0(data_dir, "Generic_.dbf"), crs = 2248)

Area_gen <- st_read(dsn = paste0(data_dir, "Area_gen.dbf"), crs = 2248)


# 3. Make a leaflet map ---------------------------------------------------




