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
library(dplyr)

data_dir <- "data/ICP_MS/"


# 2. Read the data --------------------------------------------------------

file_paths <- list.files(paste0(data_dir), full.names = TRUE) 

file_paths <- file_paths[str_detect(file_paths,"Spectroscopy")]

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

#Turn the negative values to 0
data[data < 0] <- 0

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

#Designate Catchments

#!!! Be sure to include the Site Directory in data folder!!!
site_data_path <- paste0(data_dir, "Site_Directory_Core.xlsx")

sheet_names <- excel_sheets(path = site_data_path)  

sheet_names <- sheet_names[1:3] %>% 
  as.list()
 
Site_data <- lapply(sheet_names, function(x) read_excel(path = site_data_path, sheet = x)) %>% 
  reduce(rbind) %>% 
  select(c(Site_ID, Catchment))

#Join site data
data <- left_join(data, Site_data, by = "Site_ID")

# 4. Plot ggplot ----------------------------------------------------------------------


# 4b. Make plotting functions ---------------------------------------------

#Box plot function
boxy_ploty <- function(df, x_var , y_var){
  
  boxy <- ggplot(data = df, 
                         aes( x = {{x_var}}, 
                              y = {{y_var}},
                              color = {{x_var}})) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = quantile(df %>% pull({{y_var}}), c(0.05, 0.95)))
  
  (boxy)         
  
}

#Bar plot function
barz_plotz <- function(df, x_var, y_var){
  
  dt <- df %>% 
    dplyr::group_by({{x_var}}) %>% 
    dplyr::summarise(y_mean = mean({{y_var}}))
  
  barz <- ggplot(data = dt,
                 aes(x = reorder({{x_var}}, y_mean),
                     y = y_mean,
                     color = {{x_var}})) +
    geom_col(aes(fill = {{x_var}})) +
    xlab("Site ID") +
    ylab()
    
  
  (dt)
  (barz)
  
}


# 4a. SW Boxplots ---------------------------------------------------------

data_sw <- data %>% 
  filter(Flag < 1) %>% 
  #!!!Which type of site!!!
  filter(str_detect(Site_ID, "SW")) %>% 
  filter(!str_detect(Site_ID, "AG-SW")) %>% 
  filter(!str_detect(Site_ID, "TR-SW")) %>% 
  filter(!str_detect(Site_ID, "CR-SW")) %>% 
  mutate("Site_ID" = as.factor(Site_ID))



SW_54Fe_box <- boxy_ploty(data_sw, Sample_Date_Factor, `54Fe`)
SW_54Fe_bar <- barz_plotz(data_sw, Site_ID, `54Fe`)



# SW Barplots -------------------------------------------------------------



