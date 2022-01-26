#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: ICP-MS metals plots
# Coder: James Maze
# Date: 23 Jan 2021
# Purpose: Metals visualization for Spring 22 data meeting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# 

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

#Sample_Date to a Datetime
data <- data %>% 
  #Silly sample dates were in scientific notation. Needed to fix.
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
 
Site_data <- lapply(sheet_names, 
                    function(x) read_excel(path = site_data_path, 
                                           sheet = x)) %>% 
  reduce(rbind) %>% 
  select(c(Site_ID, Catchment))

#Join site data
data <- left_join(data, Site_data, by = "Site_ID")

rm(site_data_path, sheet_names, Site_data, data_dir, file_paths)


# 4. Make plotting functions ---------------------------------------------

#Box plot function
boxy_ploty <- function(df, x_var , y_var, title){
  
  boxy <- ggplot(data = df, 
                         aes( x = {{x_var}}, 
                              y = {{y_var}})) +
    geom_boxplot(aes(fill = {{x_var}})) +
    theme_bw() +
    theme(axis.text = element_text(size = 14, 
                                   face = "bold", 
                                   angle = 45),
          plot.title = element_text(size = 36)) +
    scale_y_continuous(limits = quantile(df %>% pull({{y_var}}), c(0.1, 0.90))) +
    ggtitle({{title}})
  
  (boxy)         
  
}

#Bar plot function
barz_plotz <- function(df, x_var, y_var, title){
  
  dt <- df %>% 
    dplyr::group_by({{x_var}}) %>% 
    dplyr::summarise(y_mean = mean({{y_var}}))
  
  dt <- dt %>% 
    mutate("Site_type" = str_sub(Site_ID, 4, 6)) %>% 
    mutate("Site_type" = as.factor(Site_type))
    
  
  barz <- ggplot(data = dt,
                 aes(x = reorder({{x_var}}, Site_type),
                     y = y_mean,
                     color = Site_type)) +
    geom_col(aes(fill = Site_type)) +
    ggtitle({{title}}) +
    theme(axis.text = element_text(size = 14, 
                                   face = "bold", 
                                   angle = 90),
          plot.title = element_text(size = 36)) +
    xlab("Site ID") +
    ylab("ppb")
  
  (dt)
  (barz)
  
}


# 5. Baltimore Corner Catchment-------------------------------------------------------------

data_BC <- data %>% 
  filter(Catchment == "Baltimore Corner")

#Make bar plots
BC_23Na_bar <- barz_plotz(data_BC, Site_ID, `23Na`, "23Na BC all sites")
(BC_23Na_bar)
BC_27Al_bar <- barz_plotz(data_BC, Site_ID, `27Al`, "27Al BC all sites")
(BC_27Al_bar)
BC_29Si_bar <- barz_plotz(data_BC, Site_ID, `29Si`, "29Si BC all sites")
(BC_29Si_bar)
BC_54Fe_bar <- barz_plotz(data_BC, Site_ID, `54Fe`, "54Fe BC all sites")
(BC_54Fe_bar)
BC_55Mn_bar <- barz_plotz(data_BC, Site_ID, `55Mn`, "55MN BC all sites")
(BC_55Mn_bar)

rm(BC_23Na_bar, BC_27Al_bar, BC_29Si_bar, BC_54Fe_bar, BC_55Mn_bar)

#Make box plots
BC_23Na_box <- boxy_ploty(data_BC, Sample_Date_Factor, `23Na`, "BC 23Na overtime")
(BC_23Na_box)
BC_54Fe_box <- boxy_ploty(data_BC, Sample_Date_Factor, `54Fe`, "BC 54Fe overtime")
(BC_54Fe_box)

rm(BC_23Na_box, BC_54Fe_box, data_BC)

# 6. Jackson Lane Catchment -----------------------------------------------

data_JL <- data %>% 
  filter(Catchment == "Jackson Lane")

#Bar plots
JL_23Na_bar <- barz_plotz(data_JL, Site_ID, `23Na`, "23Na JL all sites")
(JL_23Na_bar)
JL_27Al_bar <- barz_plotz(data_JL, Site_ID, `27Al`, "27Al JL all sites")
(JL_27Al_bar)
JL_29Si_bar <- barz_plotz(data_JL, Site_ID, `29Si`, "29Si JL all sites")
(JL_29Si_bar)
JL_54Fe_bar <- barz_plotz(data_JL, Site_ID, `54Fe`, "54Fe JL all sites")
(JL_54Fe_bar)
JL_55Mn_bar <- barz_plotz(data_JL, Site_ID, `55Mn`, "55Mn JL all sites")
(JL_55Mn_bar)

rm(JL_23Na_bar, JL_27Al_bar, JL_29Si_bar, JL_54Fe_bar, JL_55Mn_bar)

#Box plots
JL_23Na_box <- boxy_ploty(data_JL, Sample_Date_Factor, `23Na`, "23Na at JL")
(JL_23Na_box)
JL_27Al_box <- boxy_ploty(data_JL, Sample_Date_Factor, `27Al`, "27Al at JL")
(JL_27Al_box)
JL_29Si_box <- boxy_ploty(data_JL, Sample_Date_Factor, `29Si`, "29Si at JL")
(JL_29Si_box)
JL_54Fe_box <- boxy_ploty(data_JL, Sample_Date_Factor, `54Fe`, "54Fe at JL")
(JL_54Fe_box)
JL_55Mn_box <- boxy_ploty(data_JL, Sample_Date_Factor, `55Mn`, "55Mn at JL")
(JL_55Mn_box)

rm(JL_54Fe_box)

# 7. Synoptic Sites -------------------------------------------------------

data_synop <- data %>% 
  filter(!Catchment == "Jackson Lane") %>% 
  filter(!Catchment == "Baltimore Corner") %>% 
  filter(!Site_ID == "TR-SW") %>% 
  filter(!Site_ID == "CR-SW") %>% 
  filter(!Site_ID == "AG-SW") %>% 
  filter(!Sample_Date_Factor == "202109") %>% 
  filter(!Sample_Date_Factor == "202110")

#Bar plots
synop_23Na_bar <- barz_plotz(data_synop, Site_ID, `23Na`, "23Na spatial at synoptic sites")
(synop_23Na_bar)
synop_27Al_bar <- barz_plotz(data_synop, Site_ID, `27Al`, "27Al spatial at synoptic sites")
(synop_27Al_bar)
synop_29Si_bar <- barz_plotz(data_synop, Site_ID, `29Si`, "29Si spatial at synoptic sites")
(synop_29Si_bar)
synop_54Fe_bar <- barz_plotz(data_synop, Site_ID, `54Fe`, "54Fe spatial at synoptic sites")
(synop_54Fe_bar)
synop_55Mn_bar <- barz_plotz(data_synop, Site_ID, `55Mn`, "55Mn spatial at synoptic sites")
(synop_55Mn_bar)

#Box plots
synop_23Na_box <- boxy_ploty(data_synop, Sample_Date_Factor, `23Na`, "23Na temporal at synoptic sites")
(synop_23Na_box)
synop_27Al_box <- boxy_ploty(data_synop, Sample_Date_Factor, `27Al`, "27Al temporal at synoptic sites")
(synop_27Al_box)
synop_29Si_box <- boxy_ploty(data_synop, Sample_Date_Factor, `29Si`, "29Si temporal at synoptic sites")
(synop_29Si_box)
synop_54Fe_box <- boxy_ploty(data_synop, Sample_Date_Factor, `54Fe`, "54Fe temporal at synoptic sites")
(synop_54Fe_box)
synop_55Mn_box <- boxy_ploty(data_synop, Sample_Date_Factor, `55Mn`, "55Mn temporal at synoptic sites")
(synop_55Mn_box)

