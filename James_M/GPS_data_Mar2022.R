#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Plot Mar 22 GPS data
# Coder: James Maze
# Date: April 4th 2022
# Purpose: Check on well points and inundated area map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#

# 1. Libraries and Packages -----------------------------------------------

remove(list = ls())

library(purrr) 
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)

data_dir <- "data/geospatial"