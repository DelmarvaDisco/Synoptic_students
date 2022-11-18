#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Bpxplot Stage Quantiles and Head Gradients
# Coder: James Maze
# Date: November 2022
# Purpose: Plot head gradients against stage quantiles.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# -

# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(broom)
library(cowplot)
library(RColorBrewer)
library(lubridate)
library(tidyverse)


data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

# 2. Read the data --------------------------------------------------------

rel_wtr_lvl <- read_csv(paste0(data_dir, "rel_wtr_lvls.csv"))

hydro_heads <- read_csv(paste0(data_dir, "hydro_heads.csv"))

# Head_relationships <- read_xlsx(paste0(data_dir, "Head_relationships.xlsx"))

# heads_list_JL <- hydro_heads %>% 
#   filter(Catchment == "Jackson Lane") %>% 
#   select(Site_IDs, elevation_gradient, elevation_diff_m) %>% 
#   unique()
# 
# heads_list_BC <- hydro_heads %>% 
#   filter(Catchment == "Jackson Lane") %>% 
#   select(Site_IDs, elevation_gradient, elevation_diff_m) %>% 
#   unique()



# 3.0 Box-plots based on water level quartile ---------------------------------------------------------------------

catchment_scale_head_list <- c("BDSW_DKSW", "NDSW_DKSW",
                               "OBSW_HBSW", "XBSW_HBSW",
                               "NDUW1_DKUW2", "OBUW1_MBUW1",
                               "OBCH_HBCH", "XBCH_HBCH",
                               "BDCH_DKCH", "TSUW1_DKUW2",
                               "OBUW1_HBUW1", "TSCH_DKCH")

#Select catchment scale gradients
temp <- hydro_heads %>% 
  filter(Site_IDs %in% catchment_scale_head_list) %>% 
  #Remove some wacky outliers
  filter(!head_gradient <= -0.0025) %>% 
  filter(!head_gradient >= 0.01)


# 3.1 Look at quantiles for aggregate water level data --------------------

#Create quantiles of the aggregate water level data to classify hydrograph regimes
quantiles <- temp %>%
  dplyr::select(c(dly_mean_wtrlvl_allsites, Catchment, Date)) %>%
  group_by(Catchment) %>%
  summarise(quant_20 = quantile(dly_mean_wtrlvl_allsites, probs = 0.2),
            quant_40 = quantile(dly_mean_wtrlvl_allsites, probs = 0.4),
            quant_60 = quantile(dly_mean_wtrlvl_allsites, probs = 0.6),
            quant_80 = quantile(dly_mean_wtrlvl_allsites, probs = 0.8))

quantiles_long <- quantiles %>%
  pivot_longer(cols = c(quant_20, quant_40, quant_60, quant_80),
               names_to = "quantile_number",
               values_to = "quantile_value") 

#
# #Check on data distribution/skew with density plot
# density_plot <- ggplot(data = temp %>%
#                          dplyr::select(c(Catchment, dly_mean_wtrlvl_allsites, Date)) %>%
#                          unique(),
#                        mapping = aes(x = dly_mean_wtrlvl_allsites,
#                                      color = Catchment)) +
#   geom_density(size = 2) +
#   geom_point(data = quantiles_long,
#              mapping = aes(x = quantile_value,
#                            y = rando_y,
#                            color = Catchment,
#                            shape = quantile_number),
#              size = 10) +
#   ylab("Density")
#
# #Print plot
# (density_plot)

# #Clean up environment
# rm(density_plot, quantiles_long)


# 3.2 Create a boxplot of hydro heads based on stage regime ---------------

#Create a stage regime column
#Join quantiles to the data.
temp <- temp %>% 
  left_join(., quantiles, by = "Catchment") 

temp <- temp %>%   
  #Classify stage regime based on the quantiles. 
  mutate(stage_regime = if_else(dly_mean_wtrlvl_allsites < quant_20,
                                "Dry",
                                if_else((quant_20 <= dly_mean_wtrlvl_allsites & dly_mean_wtrlvl_allsites < quant_40),
                                        "Dry_med",
                                        if_else((quant_40 <= dly_mean_wtrlvl_allsites & dly_mean_wtrlvl_allsites < quant_60),
                                                "Med",
                                                ifelse(quant_60 <= dly_mean_wtrlvl_allsites & dly_mean_wtrlvl_allsites < quant_80,
                                                       "Med_wet",
                                                       if_else(dly_mean_wtrlvl_allsites >= quant_80,
                                                               "Wet",
                                                               "NA")))))) %>% 
  #Designate site relationships (e.g. UW -> UW)
  mutate(Relationship = if_else(str_detect(Site_IDs, "UW"),
                                "Upland to Upland",
                                if_else(str_detect(Site_IDs, "CH"),
                                        "Channel to Channel",
                                        "Wetland to Wetland"))) %>% 
  mutate(head_gradient_cm_m = head_gradient * 100)


stage_gradient_catchment_boxplot <- ggplot(data = temp, 
                                           mapping = aes(x = stage_regime,
                                                         y = head_gradient_cm_m,
                                                         color = Catchment)) +
  geom_boxplot(outlier.shape = NA) +
  ylab("Head gradient dh/dL (cm/m)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  ggtitle("Catchment scale gradients grouped by stage quantiles (20% grouping)") +
  scale_color_brewer(palette = "Set2")

#View the plot
(stage_gradient_catchment_boxplot)

stage_gradient_sitetype_boxplot <- ggplot(data = temp,
                                          mapping = aes(x = stage_regime, 
                                                        y = head_gradient_cm_m,
                                                        color = Relationship)) +
  geom_boxplot(outlier.shape = NA) +
  ylab("Head gradient dh/dL (cm/m)") +
  xlab("Stage Regime") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

#View the plot
(stage_gradient_sitetype_boxplot)

#Combine with cowplots
stage_gradient_boxplots <- plot_grid(stage_gradient_catchment_boxplot,
                                     stage_gradient_sitetype_boxplot,
                                     ncol = 1, axis = "b", align = "v")
#View the combination
(stage_gradient_boxplots)

#Save to the plot directory
ggsave(filename = "stage_gradient_boxplots.png", 
       plot =  stage_gradient_boxplots,
       path = paste0(plot_dir))

