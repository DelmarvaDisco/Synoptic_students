#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Catchment Correlations Plots
# Coder: James Maze
# Date: November 2022
# Purpose: Plot correlations between catchment scale gradients and aggregate water level
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# -


# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(broom)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(lubridate)
library(tidyverse)


data_dir <- "data\\AGU_hydro\\output\\"
plot_dir <- "data\\AGU_hydro\\plots\\"

# 2. Read the data --------------------------------------------------------

rel_wtr_lvl <- read_csv(paste0(data_dir, "rel_wtr_lvls.csv"))

hydro_heads <- read_csv(paste0(data_dir, "hydro_heads.csv"))

# 3.1 Filter sites of interest --------------------------------------------

catchment_scale_head_list <- c("BDSW_DKSW", "NDSW_DKSW",
                               "OBSW_HBSW", "XBSW_HBSW",
                               "NDUW1_DKUW2", "OBUW1_MBUW1",
                               "OBCH_HBCH", "XBCH_HBCH",
                               "BDCH_DKCH", "TSUW1_DKUW2",
                               "OBUW1_HBUW1", "TSCH_DKCH")

#Filter by site
temp <- hydro_heads %>% 
  filter(Site_IDs %in% catchment_scale_head_list) %>% 
  #Remove some wacky outliers
  filter(!head_gradient <= -0.0025) %>%
  filter(!head_gradient >= 0.01) %>%
  #Designate site relationships (e.g. UW -> UW)
  mutate(Relationship = if_else(str_detect(Site_IDs, "UW"),
                                "Upland to Upland",
                                if_else(str_detect(Site_IDs, "CH"),
                                        "Channel to Channel",
                                        "Wetland to Wetland"))) %>%
  #Redo factor levels
  mutate(Relationship = factor(Relationship, levels = c("Channel to Channel",
                                                        "Wetland to Wetland",
                                                        "Upland to Upland"))) %>% 
  #Rename Site_IDs with better syntax
  mutate(Site_IDs = recode(Site_IDs,
                           BDSW_DKSW = "BD Wetland -> DK Wetland",
                           NDSW_DKSW = "ND Wetland -> DK Wetland", 
                           OBSW_HBSW = "OB Wetland -> HB Wetland",
                           XBSW_HBSW = "XB Wetland -> HB Wetland",
                           NDUW1_DKUW2 = "ND Upland1 -> DK Upland2",
                           OBUW1_MBUW1 = "OB Upland1 -> MB Upland1",
                           OBCH_HBCH = "OB Channel -> HB Channel",
                           XBCH_HBCH = "XB Channel -> HB Channel", 
                           BDCH_DKCH = "BD Channel -> DK Channel",
                           TSUW1_DKUW2 = "TS Upland1 -> DK Upland2",
                           OBUW1_HBUW1 = "OB Upland1 -> HB Upland1",
                           TSCH_DKCH = "TS Channel -> DK Channel")) %>% 
  #Changing head gradient units to (cm/m) makes units more digestible.
  mutate(head_gradient_cm_m = (head_gradient * 100)) 


# 3.2 Generate SW summary stats and model info -------------------------------

models <- temp %>% 
  #Group by Site_ID
  group_by(Site_IDs) %>%
  #Nest to get model output and data frame for each head-gradient pair
  nest() %>% 
  #Generate linear models
  mutate(gradient_models = map(.x = data, 
                               ~lm(head_gradient_cm_m ~ dly_mean_wtrlvl_allsites, 
                                   data = .x) %>% 
                                 #Use tidy() to convert model output to
                                 # a data frame. 
                                 tidy())) %>% 
  unnest(gradient_models) %>% 
  #Remove column with model input data
  dplyr::select(-c(data)) %>% 
  #Convert to a tibble
  as_tibble() %>% 
  #Improve the term column with more descriptive verbage
  mutate(term = recode(term,
                       `(Intercept)` = "y-intercept",
                       `dly_mean_wtrlvl_allsites` = "slope"))

#Get the summary statistics
stats <- temp %>% 
  #Group by Site_ID
  group_by(Site_IDs) %>%
  #Nest to get model output and data frame for each head-gradient pair
  nest() %>% 
  #Generate linear models
  mutate(gradient_models = map(.x = data, 
                               ~lm(head_gradient_cm_m ~ dly_mean_wtrlvl_allsites, 
                                   data = .x) %>% 
                                 #Get the model summary statics using glance
                                 glance())) %>% 
  #Unnest the summary statistics into a data frame
  unnest(gradient_models) %>% 
  #Eliminate column 
  dplyr::select(-c(data)) %>% 
  as_tibble() 


# 3.3 Make the SW correlation plot -------------------------------------------

correlation_plot <- ggplot(data = temp, 
                           mapping = aes(x = dly_mean_wtrlvl_allsites,
                                         y = head_gradient_cm_m,
                                         color = Relationship,
                                         fill = Site_IDs)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "black",
              se = F) +
  # geom_text(data = stats,
  #           aes(label = paste0("r^2 = ", round(r.squared, digits = 2))),
  #           x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2,
  #           inherit.aes = FALSE,
  #           color = "black",
  #           size = 4) +
  # geom_text(data = models %>%
  #             filter(term == "slope"),
  #           aes(label = paste0("slope = ", round(estimate, digits = 5))),
  #           x = -Inf, y = Inf, hjust = -0.1, vjust = 2.5,
  #           inherit.aes = FALSE,
  #           color = "black",
  #           size = 4) +
  theme_bw() +
  ylab("dh/dL in (cm/m)") +
  xlab("Daily mean water level aggregated across catchment (m)") +
  theme(legend.position = "bottom",
        ) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(Relationship), 
             scales = "free") +
  ylim(c(-0.15, 0.8)) 

#Print and save the plot
(correlation_plot)

ggsave(filename = "Catchment_scale_gradient_to_stage_correlations.png", 
       plot = correlation_plot, 
       path = paste0(plot_dir))


# 3.4 Summary table of correlations ---------------------------------------


#Generate a table with summary statistics
corr_table <- stats %>% 
  dplyr::select(c(Site_IDs, r.squared)) %>% 
  left_join(., 
            models %>% filter(term == "slope") %>% 
              dplyr::select(c(estimate, Site_IDs)),
            by = "Site_IDs") %>% 
  #Designate site relationships
  mutate(Relationship = if_else(str_detect(Site_IDs, "Upland"),
                                "Upland to Upland",
                                if_else(str_detect(Site_IDs, "Channel"),
                                        "Channel to Channel",
                                        "Wetland to Wetland"))) %>% 
  #Redo factor levels
  mutate(Relationship = factor(Relationship, levels = c("Channel to Channel",
                                                        "Wetland to Wetland",
                                                        "Upland to Upland"))) %>% 
  #Designate Catchments
  mutate(Catchment = if_else(str_detect(Site_IDs, "OB | TP | XB | MB"),
                             "Baltimore Corner",
                             "Jackson Lane")) %>% 
  rename("slope" = estimate)


#Create bar chart colored by catchment
slope_bars_well_relationships <- ggplot(data = corr_table, 
                               mapping = aes(x = reorder(x = Site_IDs,
                                               X = slope),
                                             y = slope,
                                             fill = Relationship)) +
                                geom_col() +
                                geom_label(data = corr_table,
                                          aes(label = paste0("r^2 =", round(r.squared, digits = 2))),
                                          color = "black",
                                          fill = "white",
                                          hjust = 0.75,
                                          size = 4) +
                                theme_bw() +
                                theme(legend.position = "bottom",
                                      axis.title.y = element_blank()) +
                                ylab("Slope of regression line") +
                                ggtitle("Strength of relationship between head gradient (dh/dL) and stage") +
                                coord_flip() +
                                scale_fill_brewer(palette = "Set1")

#Print the plot
(slope_bars_well_relationships)


#Create bar chart colored by Catchment
slope_bars_catchment <- ggplot(data = corr_table, 
                                        mapping = aes(x = reorder(x = Site_IDs,
                                                                  X = slope),
                                                      y = slope,
                                                      fill = Catchment)) +
                                  geom_col() +
                                  theme_bw() +
                                  geom_label(data = corr_table,
                                             aes(label = paste0("r^2 =", round(r.squared, digits = 2))),
                                             color = "black",
                                             fill = "white",
                                             hjust = 0.75,
                                             size = 4) +
                                  xlab("Site Pairs") +
                                  ylab("Strength of regression (slope)") +
                                  coord_flip() +
                                  scale_fill_brewer(palette = "Set2")
#Print the plot
(slope_bars_catchment)

#Combine the plots

slope_bars_plot <- plot_grid(slope_bars_well_relationships, 
                             slope_bars_catchment,
                             ncol = 1)

(slope_bars_plot)

