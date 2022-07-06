#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: ICP-MS metals plots
# Coder: James Maze
# Date: 23 Jan 2022
# Purpose: Metals visualization for Spring 22 data meeting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# - Replace all of the Sample_month_factor with Yr_Mon

# 1. Libraries and Packages -----------------------------------------------

remove(list = ls())

library(purrr) 
library(lubridate)
library(stringr)
library(tidyverse)
library(readxl)
library(cowplot)
library(plyr)
library(rlang)
library(dplyr)

data_dir <- "data/ICP_MS/"


# 2. Read the data --------------------------------------------------------

file_paths <- list.files(paste0(data_dir), full.names = TRUE) 

file_paths <- file_paths[str_detect(file_paths,"Spectroscopy")]

#Download function for ICPMS data
download_fun <- function(file_paths){
 
   temp <- read_xlsx(paste0(file_paths),
                    skip = 10,
                    col_types = "text") %>% 
    as_tibble() %>% 
    select(-c(Bottle, Rep, Sample_ID))
   
   temp
}

#Run the download function
data <- file_paths %>% 
  map(download_fun) %>% 
  reduce(bind_rows)

# 3. Clean up data----------------------------------------------------------------------

# 3.1 Rename and reformat columns -----------------------------------------

#Convert the analyte columns to numeric
data[ ,c(5:32,33)] <- lapply(data[ ,c(5:32,33)], as.numeric)

#Turn the negative values to 0
data[data < 0] <- 0

#Sample_Date to a Datetime
data <- data %>% 
  #Sample dates were in scientific notation, because google sheets is dumb. 
  mutate(Date = str_replace(Sample_Date, 
                            pattern = "([.])",
                            replacement = "")) %>% 
  mutate(Dates = str_trunc(Date, width = 8, side = "right", ellipsis = "")) %>%
  mutate(Year = str_sub(Dates, 1, 4),
         Month = str_sub(Dates, 5, 6),
         Day = str_sub(Dates, 7, 8)) %>% 
  mutate(Day = str_replace(Day, "E", "0")) %>% 
  mutate(Sample_Date = ymd(paste0(Year, "-", Month, "-", Day))) %>% 
  select(-c(Date, Dates, Day)) %>% 
  #Convert month and year to factor
  mutate(Month = as.factor(Month),
         Year = as.factor(Year),
         Yr_Mon = as.factor(paste0(Year, "-", Month)))


# 3.2 Designate Catchments  -----------------------------------------------

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
  
  df <- df %>% 
    mutate(Site_type = str_sub(Site_ID, 4, 5)) 
  
  boxy <- ggplot(data = df, 
                         aes( x = {{x_var}}, 
                              y = {{y_var}})) +
    geom_boxplot(aes(fill = Site_type),
                 position = position_dodge(width = 0.75,
                                           preserve = "single"),
                 width = 0.75) +
    theme_bw() +
    theme(axis.text = element_text(size = 6, 
                                   face = "bold", 
                                   angle = 45),
          plot.title = element_text(size = 36),
          axis.title.x = element_blank()) +
    #Trimming some outliers to improve readability
    scale_y_continuous(limits = quantile(df %>% pull({{y_var}}), c(0.1, 0.90))) +
    ylab({{title}})
    #ggtitle({{title}})
  
  (boxy)         
  
}

#Bar plot function
barz_plotz <- function(df, x_var, y_var, title){
  
  dt <- df %>% 
    dplyr::group_by({{x_var}}) %>% 
    dplyr::summarise(y_mean = mean({{y_var}}))
  
  dt <- dt %>% 
    mutate("Site_type_char" = str_sub(Site_ID, 4, 6)) %>% 
    mutate(Site_type = as.factor(Site_type_char)) %>% 
    mutate(Wetland = str_sub(Site_ID, 1, 2)) %>% 
    mutate(Wetland = as.factor(Wetland)) 
    
  
  barz <- ggplot(data = dt,
                 aes(x = Wetland,
                     y = y_mean,
                     fill = Site_type)) +
    geom_col(position = position_dodge(width = 0.75, 
                                       preserve = "single"),
             color = "black",
             width = 0.75) +
    #ggtitle({{title}}) +
    theme_bw() +
    theme(axis.text = element_text(size = 8, 
                                   face = "bold", 
                                   angle = 0),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 18)) +
    ylab({{title}}) 
  
  (dt)
  (barz)
  
}

# 5. Baltimore Corner Catchment-------------------------------------------------------------

data_BC <- data %>% 
  filter(Catchment == "Baltimore Corner") %>% 
#Huge outlier in XB-CH might want to remove in some instances
  filter(!Site_ID == "XB-CH")

# 5.1 BC bar plots --------------------------------------------------------

#Make bar plots
BC_23Na_bar <- barz_plotz(data_BC, Site_ID, `23Na`, "23Na at Baltimore Corner sites")
(BC_23Na_bar)
BC_27Al_bar <- barz_plotz(data_BC, Site_ID, `27Al`, "27Al at Baltimore Corner sites")
(BC_27Al_bar)
BC_29Si_bar <- barz_plotz(data_BC, Site_ID, `29Si`, "29Si at Baltimore Corner sites")
(BC_29Si_bar)
BC_34S_bar <- barz_plotz(data_BC, Site_ID, `34S`, "34S at Baltimore Corner sites")
(BC_34S_bar)
BC_54Fe_bar <- barz_plotz(data_BC, Site_ID, `54Fe`, "54Fe at Baltimore Corner sites")
(BC_54Fe_bar)
BC_55Mn_bar <- barz_plotz(data_BC, Site_ID, `55Mn`, "55MN at Baltimore Corner sites")
(BC_55Mn_bar)

# Pick your analytes to stack up
BC_redox_space <- plot_grid(BC_34S_bar, BC_54Fe_bar, BC_55Mn_bar,
                             ncol = 1, axis = "b")
(BC_redox_space)

BC_funstuff_space <- plot_grid(BC_23Na_bar, BC_29Si_bar, BC_27Al_bar,
                            ncol = 1, axis = "b")
(BC_funstuff_space)
                              
#Clean up some environment
rm(BC_23Na_bar, BC_27Al_bar, BC_29Si_bar, BC_34S_bar, BC_54Fe_bar, 
   BC_55Mn_bar, BC_redox_space, BC_funstuff_space)


# 5.2 BC box plots --------------------------------------------------------

#Make box plots
BC_23Na_box <- boxy_ploty(data_BC, Yr_Mon, `23Na`, "23Na at Baltimore Corner Sites")
(BC_23Na_box)
BC_27Al_box <- boxy_ploty(data_BC, Yr_Mon, `27Al`, "27Al at Baltimore Corner Sites")
(BC_27Al_box)
BC_29Si_box <- boxy_ploty(data_BC, Yr_Mon, `29Si`, "29Si at Baltimore Corner Sites")
(BC_29Si_box)
BC_34S_box <- boxy_ploty(data_BC, Yr_Mon, `34S`, "34S at Baltimore Corner Sites")
(BC_34S_box)
BC_54Fe_box <- boxy_ploty(data_BC, Yr_Mon, `54Fe`, "54Fe at Baltimore Corner Sites")
(BC_54Fe_box)
BC_55Mn_box <- boxy_ploty(data_BC, Yr_Mon, `55Mn`, "55Mn at Baltimore Corner Sites")
(BC_55Mn_box)

BC_redox_time <- plot_grid(BC_34S_box, BC_54Fe_box, BC_55Mn_box,
                           ncol = 1, axis = "b")
(BC_redox_time)

BC_funstuff_time <- plot_grid(BC_23Na_box, BC_27Al_box, BC_29Si_box,
                            ncol = 1, axis = "b")
(BC_funstuff_time)

rm(BC_23Na_box, BC_27Al_box, BC_29Si_box, BC_34S_box, BC_54Fe_box, 
   BC_55Mn_box, data_BC, BC_funstuff_time, BC_redox_time)

# 6. Jackson Lane Catchment -----------------------------------------------

data_JL <- data %>% 
  filter(Catchment == "Jackson Lane")

# 6.1 JL bar plots --------------------------------------------------------

#Bar plots
JL_23Na_bar <- barz_plotz(data_JL, Site_ID, `23Na`, "23Na at JL sites")
(JL_23Na_bar)
JL_27Al_bar <- barz_plotz(data_JL, Site_ID, `27Al`, "27Al at JL sites")
(JL_27Al_bar)
JL_29Si_bar <- barz_plotz(data_JL, Site_ID, `29Si`, "29Si at JL sites")
(JL_29Si_bar)
JL_34S_bar <- barz_plotz(data_JL, Site_ID, `34S`,  "34S at JL sites")
(JL_34S_bar)
JL_54Fe_bar <- barz_plotz(data_JL, Site_ID, `54Fe`, "54Fe at JL sites")
(JL_54Fe_bar)
JL_55Mn_bar <- barz_plotz(data_JL, Site_ID, `55Mn`, "55Mn at JL sites")
(JL_55Mn_bar)

JL_redox_space <- plot_grid(JL_34S_bar, JL_54Fe_bar, JL_55Mn_bar,
                             ncol = 1, axis = "b")
(JL_redox_space)

JL_funstuff_space <- plot_grid(JL_23Na_bar, JL_29Si_bar, JL_27Al_bar,
                               ncol = 1, axis = "b")
(JL_funstuff_space)

rm(JL_23Na_bar, JL_27Al_bar, JL_29Si_bar, JL_34S_bar, JL_54Fe_bar, 
   JL_55Mn_bar, JL_redox_space, JL_funstuff_space)

# 6.2 JL box plots --------------------------------------------------------

#Box plots
JL_23Na_box <- boxy_ploty(data_JL, Yr_Mon, `23Na`, "23Na at JL Sites")
(JL_23Na_box)
JL_27Al_box <- boxy_ploty(data_JL, Yr_Mon, `27Al`, "27Al at JL Sites")
(JL_27Al_box)
JL_29Si_box <- boxy_ploty(data_JL, Yr_Mon, `29Si`, "29Si at JL Sites")
(JL_29Si_box)
JL_34S_box <- boxy_ploty(data_JL, Yr_Mon, `34S`, "34S at JL Sites")
(JL_34S_box)
JL_54Fe_box <- boxy_ploty(data_JL, Yr_Mon, `54Fe`, "54Fe at JL Sites")
(JL_54Fe_box)
JL_55Mn_box <- boxy_ploty(data_JL, Yr_Mon, `55Mn`, "55Mn at JL Sites")
(JL_55Mn_box)

JL_redox_time <- plot_grid(JL_34S_box, JL_54Fe_box, JL_55Mn_box,
                           ncol = 1, axis = "b")
(JL_redox_time)

JL_funstuff_time <- plot_grid(JL_23Na_box, JL_29Si_box, JL_27Al_box,
                              ncol = 1, axis = "b")
(JL_funstuff_time)

rm(JL_23Na_box, JL_27Al_box, JL_29Si_box, JL_34S_box, JL_54Fe_box,
   JL_55Mn_box, JL_redox_time, JL_funstuff_time, data_JL)

# 7. Synoptic Sites -------------------------------------------------------

data_synop <- data %>% 
  filter(!Catchment == "Jackson Lane") %>% 
  filter(!Catchment == "Baltimore Corner") %>% 
  filter(!Site_ID == "TR-SW") %>% 
  filter(!Site_ID == "CR-SW") %>% 
  filter(!Site_ID == "AG-SW") %>% 
  filter(!Sample_Date_Factor == "202109") %>% 
  filter(!Sample_Date_Factor == "202110")

# 7.1 Synop bar plots -----------------------------------------------------

#Bar plots
synop_23Na_bar <- barz_plotz(data_synop, Site_ID, `23Na`, "23Na at synoptic sites")
(synop_23Na_bar)
synop_27Al_bar <- barz_plotz(data_synop, Site_ID, `27Al`, "27Al at synoptic sites")
(synop_27Al_bar)
synop_29Si_bar <- barz_plotz(data_synop, Site_ID, `29Si`, "29Si at synoptic sites")
(synop_29Si_bar)
synop_34S_bar <- barz_plotz(data_synop, Site_ID, `34S`, "34S at synoptic sites")
(synop_34S_bar)
synop_54Fe_bar <- barz_plotz(data_synop, Site_ID, `54Fe`, "54Fe at synoptic sites")
(synop_54Fe_bar)
synop_55Mn_bar <- barz_plotz(data_synop, Site_ID, `55Mn`, "55Mn at synoptic sites")
(synop_55Mn_bar)

#Combine plots
synop_redox_space <- plot_grid(synop_34S_bar, synop_54Fe_bar, synop_55Mn_bar,
                               ncol = 1, axis = "b")
(synop_redox_space)

synop_funstuff_space <- plot_grid(synop_23Na_bar, synop_29Si_bar, synop_27Al_bar,
                                  ncol = 1, axis = "b")
(synop_funstuff_space)

#Clean up directory
rm(synop_23Na_bar, synop_27Al_bar, synop_29Si_bar, synop_34S_bar, synop_54Fe_bar,
   synop_55Mn_bar, synop_redox_space, synop_funstuff_space)


# 7.2 Synop Box Plots -----------------------------------------------------

#Box plots
synop_23Na_box <- boxy_ploty(data_synop, Yr_Mon, `23Na`, "23Na synoptic sites")
(synop_23Na_box)
synop_27Al_box <- boxy_ploty(data_synop, Yr_Mon, `27Al`, "27Al synoptic sites")
(synop_27Al_box)
synop_29Si_box <- boxy_ploty(data_synop, Yr_Mon, `29Si`, "29Si synoptic sites")
(synop_29Si_box)
synop_34S_box <- boxy_ploty(data_synop, Yr_Mon, `34S`, "34S synoptic sites")
(synop_34S_box)
synop_54Fe_box <- boxy_ploty(data_synop, Yr_Mon, `54Fe`, "54Fe synoptic sites")
(synop_54Fe_box)
synop_55Mn_box <- boxy_ploty(data_synop, Yr_Mon, `55Mn`, "55Mn synoptic sites")
(synop_55Mn_box)

#Combine plots
synop_redox_time <- plot_grid(synop_34S_box, synop_54Fe_box, synop_55Mn_box,
                              ncol = 1, axis = "b")
(synop_redox_time)

synop_funstuff_time <- plot_grid(synop_23Na_box, synop_29Si_box, synop_27Al_box,
                              ncol = 1, axis = "b")
(synop_funstuff_time)

#Clean up directory
rm(synop_23Na_box, synop_27Al_box, synop_29Si_box, synop_34S_box, synop_54Fe_box,
   synop_55Mn_box, synop_redox_time, synop_funstuff_time, data_synop)


# 8. Plot variability for each metal ----------------------------------


data_wetland <- data %>% 
  #Remove the rivers and AG ditch for more representative dataset. 
  filter(!Site_ID %in% c("TR-SW", "CR-SW", "AG-SW")) %>% 
  select(-c("Flag", "Flag_Notes", "Catchment")) 

wetland_long <- pivot_longer(data = data_wetland,
                             cols = c("7Li", "23Na", "24Mg", "27Al",
                                      "29Si", "31P", "34S", "35Cl",
                                      "39K", "44Ca", "47Ti", "51V",
                                      "52Cr", "54Fe", "55Mn", "59Co",
                                      "60Ni", "65Cu", "66Zn", "75As",
                                      "78Se", "88Sr", "95Mo", "107Ag",
                                      "111Cd", "120Sn", "138Ba", "208Pb",
                                      "238U"),
                             names_to = "Analyte",
                             values_to = "value_ppb") %>% 
  filter(!is.na(value_ppb)) %>% 
  mutate(Site_type = str_sub(Site_ID, 4, 5)) %>% 
  #Remove the noisy low conc. values
  filter(value_ppb >= 3)


# 8.1 Coefficient of variation plot ---------------------------------------

#Summary stats grouped by analyte and site type
summary_analyte_type <- wetland_long %>% 
  dplyr :: group_by(Analyte, Site_type) %>% 
  dplyr :: summarize(mean_value_ppb = mean(value_ppb),
                     sd_value_ppb = sd(value_ppb)) %>% 
  mutate(coeff_var = (sd_value_ppb / mean_value_ppb) * 100)

#Summary stats grouped by analyte only
summary_analyte <- wetland_long %>% 
  dplyr :: group_by(Analyte) %>% 
  dplyr :: summarize(mean_value_ppb = mean(value_ppb),
                     sd_value_ppb = sd(value_ppb)) %>% 
  mutate(coeff_var = (sd_value_ppb / mean_value_ppb) * 100)


#Compare the coefficient of variation across analytes
coeff_var_plot <- ggplot(data = summary_analyte,
                           mapping = aes(x = reorder(Analyte, 
                                                     -coeff_var),
                                         y = coeff_var)) +
  geom_point(size = 24, 
             shape = "-") +
  geom_point(data = summary_analyte_type,
             aes(x = Analyte,
                 y = coeff_var, 
                 color = Site_type),
             size = 5,
             shape = 4,
             stroke = 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 10, 
                                 face = "bold", 
                                 angle = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab("Analyte") +
  ylab("Coefficient of Variation %") 
  
(coeff_var_plot)

#Calculate the z-score for each observation and plot

# 8.2 z-score of observations plot ----------------------------------------

wetland_long <- left_join(wetland_long, summary_analyte)


