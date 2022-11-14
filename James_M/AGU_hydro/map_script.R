#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: DEM Mapper
# Coder: James Maze
# Date: November 2022
# Purpose: Generate a cool map with DEM, depression shapes and wells
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Input data comes from Nate Jone's inundation script
# 


# 1.0 Libraries and packages ----------------------------------------------

rm(list = ls())

library(tidyverse)
library(leaflet)
library(sf)
library(raster)
library(mapview)

data_dir <- "data\\AGU_hydro\\output\\"

p <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# 2.0 Read in the data ----------------------------------------------------

#Read in DEM
processed_DEM <- raster(paste0(data_dir, "dem_baltimore_corner.tif"))

#Read in different iterations of the stochastic depression model. 
giws_25 <- st_read(paste0(data_dir, "giws_25.shp"))
giws_50 <- st_read(paste0(data_dir, "giws_50.shp"))
giws_70 <- st_read(paste0(data_dir, "giws_70.shp"))
giws_80 <- st_read(paste0(data_dir, "giws_80.shp"))
giws_95 <- st_read(paste0(data_dir, "giws_95.shp"))

#Well locations
well_locations <- st_read(paste0(data_dir, "site_points.shp")) %>% 
  filter(Ctchmnt == "Baltimore Corner")

# 3.0 Get everything on the same crs -----------------------------------------------

#Reduce the pixle size of the DEM
processed_DEM_ag <- raster::aggregate(processed_DEM,
                                      fact = 5,
                                      fun = mean)

#Make sure everything is on the correct coordinate reference system.
st_crs(giws_25) <- p
st_crs(giws_50) <- p
st_crs(giws_80) <- p
st_crs(giws_95) <- p
st_crs(giws_70) <- p
crs(processed_DEM) <- p
crs(processed_DEM_ag) <- p
st_crs(well_locations) <- p

# 4.0 Mapview --------------------------------------------------

  mapview(giws_80, alpha = 0.5) + 
  mapview(well_locations) %>% addLabels(label = well_locations$Site_ID,
                                              noHide = TRUE)


# Leaflet -----------------------------------------------------------------


blue_orange_pal <- colorNumeric(c("blue", "orange"), values(processed_DEM_ag))

map <- leaflet(well_locations) %>% 
  addTiles() %>% 
  addRasterImage(processed_DEM_ag, 
                 colors = blue_orange_pal) %>% 
  addLegend(pal = pal,
            values = values(processed_DEM_ag)) %>% 
  addPolygons()

well_locations_c <- st_transform(well_locations, p)

map <- leaflet(well_locations) %>% 
  addMarkers()
  

(map)
  



