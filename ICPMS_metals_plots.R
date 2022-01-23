#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: ICP-MS metals plots
# Coder: James Maze
# Date: 23 Jan 2021
# Purpose: Metals visualization for Spring 22 data meeting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#   - let's see


# 1. Libraries and Packages -----------------------------------------------

remove(list = ls())

library(purrr) 
library(lubridate)
library(stringr)
library(tidyverse)

data_dir <- "data/ICP_MS/"


# 2. Read the data --------------------------------------------------------


