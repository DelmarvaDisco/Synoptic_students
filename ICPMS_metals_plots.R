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
library(readxl)
library(plyr)
library(rlang)

data_dir <- "data/ICP_MS/"


# 2. Read the data --------------------------------------------------------

file_paths <- list.files(paste0(data_dir), full.names = TRUE)

download_fun <- function(file_paths){
 
   temp <- read_xlsx(paste0(file_paths),
                    skip = 10,
                    col_types = "text") %>% 
    as.tibble() %>% 
    select(-c(Bottle, Rep, Sample_ID)) %>% 
    mutate(sample_month = str_sub(Sample_Date, start = 5, end = 6))
   
   temp
}

data <- file_paths %>% 
  map(download_fun) %>% 
  reduce(bind_rows)


# 3. Clean up data----------------------------------------------------------------------

#Convert the analyte columns to numeric
data[ ,c(5:32,34)] <- lapply(data[ ,c(5:32,34)], as.numeric)

#Turn Sample_Date to a Datetime
data <- data %>% 
  #Silly sample dates were in scientific notation. 
  mutate(Date = str_replace(Sample_Date, 
                            pattern = "([.])",
                            replacement = "")) %>% 
  mutate(Dates = str_trunc(Date, width = 6, side = "right", ellipsis = "")) %>%
  mutate(Sample_Date_Factor = as.factor(Dates)) %>% 
  #Turn into a datetime
  mutate(Sample_Date = ym(Dates)) %>% 
  select(-c(Date, Dates)) 


#Turn the negative values to 0
data[data < 0] <- 0

# 4. Plot ggplot ----------------------------------------------------------------------

dt <- data %>% 
  filter(Flag < 1) %>% 
  #!!!Which type of site!!!
  filter(str_detect(Site_ID, "SW"))


boxy_ploty <- function(df, x_var , y_var){
  
              ggplot2::ggplot(data = df, 
                              aes( x = {{x_var}}, 
                              y = {{y_var}},
                              color = {{x_var}})) +
                       geom_boxplot() +
                       theme_bw() 
  
}


boxy_ploty(df = dt, x_var = Sample_Date_Factor, y_var = `54Fe`)



