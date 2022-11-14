#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Delmarva Disco Catchment Hydro
# Coder: James Maze
# Date: November 2022
# Purpose: Synthesized info/update for team members
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# -


# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(readxl)
library(broom)
library(cowplot)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
library(lubridate)

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


# 3. Show relative water level time series ----------------------------------------------------------------------

# 3.1 Baltimore Corner SW & CH rel wtr lvl -----------------------------

#Assign catchment positions for the figure. 
Site_ID_list <- c("HB-CH", "HB-SW", "MB-CH", "MB-SW", "OB-CH", 
                "OB-SW", "TP-CH", "XB-CH", "XB-SW")

catchment_pos <- c("2", "3", "4", "5", "8", "9", "1", "7", "6")

catchment_pos <- data.frame(Site_ID_list, catchment_pos) %>% 
  rename(Site_ID = Site_ID_list)

#Filter out sites of interest
temp <- rel_wtr_lvl %>%
  filter(Catchment == "Baltimore Corner") %>%
  filter(Site_ID %in% Site_ID_list)

#Need a dummy POSIXct column for every date, which 
# prevents geom_line from arbitrarily drawing lines between gaps. 
ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-11-01"), by = "day")
ts <- format.POSIXct(ts, "%Y-%m-%d")
ts <- data.frame(Date = ymd(ts))

temp <- full_join(ts, temp)

#Need to prevent ggplot from drawing lines between missing data.

#First need to pull off elevation data, add it back later.
elevation_temp <- temp %>% 
  select(c(Site_ID, Elevation_m)) %>% 
  unique()

#Pivoting between wide and long populates the data gaps with "NA"
temp <- temp %>% 
  pivot_wider(id_cols = c(Date, dly_mean_wtrlvl_allsites),
              names_from = Site_ID,
              values_from = Wtrlvl_rel_datum) %>% 
  select(-c("NA")) %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_ID",
               values_to = "Wtrlvl_rel_datum") 

#Rejoin the elevation and position attributes
temp <- left_join(temp, elevation_temp, by = "Site_ID")
temp <- left_join(temp, catchment_pos, by = "Site_ID") %>% 
  mutate(catchment_pos_Site_ID = paste0(catchment_pos, " ", Site_ID))

#Make plot colored by elevation. 
relwtrlvl_SWCH_BaltimoreCorner_elevation <- ggplot(data = temp,
                                                   mapping = aes(x = Date,
                                                                 y = Wtrlvl_rel_datum,
                                                                 color = Elevation_m)) +
  ylab("Relative water level (meters)") +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "orange") 

#Print the plot 
(relwtrlvl_SWCH_BaltimoreCorner_elevation)

#Make plot colored by Site_ID
relwtrlvl_SWCH_BaltimoreCorner_Site_ID <- ggplot(data = temp, 
                                                 mapping = aes(x = Date, 
                                                               y = Wtrlvl_rel_datum,
                                                               color = catchment_pos_Site_ID)) +
  ylab("Relative water level (meters)") +
  geom_line(size = 1) +
  theme_bw() + 
  scale_color_brewer(palette = "Spectral", direction = -1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 10, 
                                   face = "bold")) +
  labs(col = "Catchment Position") +
  ggtitle("BC wetland and chanel relative water level by Site ID (top) and Elevation (bottom)")

(relwtrlvl_SWCH_BaltimoreCorner_Site_ID)

#Combine the plots 
relwtrlvl_SWCH_BaltimoreCorner <- plot_grid(relwtrlvl_SWCH_BaltimoreCorner_Site_ID,
                                            relwtrlvl_SWCH_BaltimoreCorner_elevation,
                                            ncol = 1, axis = "b", align = "v")
#View the combination
(relwtrlvl_SWCH_BaltimoreCorner)

#Save to the plot directory
ggsave(filename = "Relative_wtrlvl_SWCH_BaltimoreCorner.png", 
       plot =  relwtrlvl_SWCH_BaltimoreCorner,
       path = paste0(plot_dir))

rm(relwtrlvl_SWCH_BaltimoreCorner, relwtrlvl_SWCH_BaltimoreCorner_Site_ID, 
   relwtrlvl_SWCH_BaltimoreCorner_elevation, ts, temp, cathment_pos, elevation_temp)

# 3.2 Baltimore Corner UW and CH rel wtr lvl ------------------------------



# 3.4 Jackson Lane UW elevation heads ------------------------------------------------------------

#Illustrate elevation head timeseries using Jackson Lane data
temp <- rel_wtr_lvl %>% 
  filter(Catchment == "Jackson Lane") %>% 
  filter(well_type %in% c("UW", "CH"))

#Need a dummy POSIXct column for every date, 
#which prevents geom_line from arbitrarily drawing lines between gaps. 
ts <- seq.POSIXt(as.POSIXct("2021-03-01"), as.POSIXct("2022-11-01"), by = "day")
ts <- format.POSIXct(ts, "%Y-%m-%d")
ts <- data.frame(Date = ymd(ts))

temp <- full_join(ts, temp)

#Pivoting between wide and long populates the data gaps with "NA", 
#which prevents ggplot from drawing lines between missing data.
temp <- temp %>% 
  pivot_wider(id_cols = c(Date, dly_mean_wtrlvl_allsites),
              names_from = Site_ID,
              values_from = Wtrlvl_rel_datum) %>%
  select(-c("NA")) %>% 
  pivot_longer(cols = -c(Date, dly_mean_wtrlvl_allsites),
               names_to = "Site_ID",
               values_to = "Wtrlvl_rel_datum")


relwtrlvl_UW_JacksonLane <- ggplot(data = temp,
                                   mapping = aes(x = Date,
                                                 y = Wtrlvl_rel_datum,
                                                 color = Site_ID)) +
  geom_line(size = 1.2) +
  ggtitle("Water level (m) relative to DK-SW wetland bottom") +
  ylab("meters") +
  theme_bw() +
  scale_color_brewer(palette = "Spectral")

#Print the plot
(relwtrlvl_UW_JacksonLane)

rm(relwtrlvl_UW_JacksonLane, ts, temp)


# 4.0 Catchment scale head gradients vs aggregate water level  ---------------------------------------------------------------------

#Filter by site
temp <- hydro_heads %>% 
  filter(Site_IDs %in% c("BDSW_DKSW", "NDSW_DKSW",
                         "OBSW_HBSW", "XBSW_HBSW")) %>% 
  #Rename Site_IDs with better syntax
  mutate(Site_IDs = recode(Site_IDs,
                           BDSW_DKSW = "Baby Doll SW -> Dark Bay SW",
                           NDSW_DKSW = "N. Dog Bone SW -> Dark Bay SW", 
                           OBSW_HBSW = "Origin Bay SW -> Hummock Bay SW",
                           XBSW_HBSW = "Gnarly Bay SW -> Hummock Bay SW"))

# 4.1 Generate summary stats and model info -------------------------------

models <- temp %>% 
  #Group by Site_ID
  group_by(Site_IDs) %>%
  #Nest to get model output and data frame for each head-gradient pair
  nest() %>% 
  #Generate linear models
  mutate(gradient_models = map(.x = data, 
                               ~lm(head_gradient ~ dly_mean_wtrlvl_allsites, 
                                   data = .x) %>% 
                                 #Use tidy() to convert model output to
                                 # a data frame. 
                                 tidy())) %>% 
  unnest(gradient_models) %>% 
  #Remove column with model input data
  select(-c(data)) %>% 
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
                               ~lm(head_gradient ~ dly_mean_wtrlvl_allsites, 
                                   data = .x) %>% 
                                 #Get the model summary statics using glance
                                 glance())) %>% 
  #Unnest the summary statistics into a data frame
  unnest(gradient_models) %>% 
  #Eliminate column 
  select(-c(data)) %>% 
  as_tibble()


# 4.2 Make the correlation plot -------------------------------------------

correlation_plot <- ggplot(data = temp, 
                           mapping = aes(x = dly_mean_wtrlvl_allsites,
                                         y = head_gradient,
                                         color = Catchment)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "black") +
  geom_text(data = stats,
            aes(label = paste0("r^2 = ", round(r.squared, digits = 2))),
            x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2,
            inherit.aes = FALSE,
            color = "black",
            size = 4) +
  geom_text(data = stats,
            aes(label = paste0("slope = ", round(estimate, digits = 5))),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 2.5,
            inherit.aes = FALSE,
            color = "black",
            size = 4) +
  geom_text(data = models %>% 
              filter(term == "slope"),
            aes(label = paste0("slope = ", round(estimate, digits = 5))),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 2.5,
            inherit.aes = FALSE,
            color = "black",
            size = 4) +
  theme_bw() +
  ylab("dh/dL") +
  xlab("Daily mean water level aggregated across catchment") +
  facet_wrap(vars(Site_IDs))

(correlation_plot)

ggsave(filename = "Catchment_scale_gradienttostage_correlations.png", 
       plot = correlation_plot, 
       path = paste0(plot_dir))

#Clean up environment
rm(correlation_plot, models, stats)





# Explore elevation gradients vs. head gradients correlation ------------------------------------------------------------

Month <- c(1:12)
ET_guess <- c("low", "low", "low", "medium", "high", "high", "high", "high", "high", "medium", "low", "low")

temp <- data.frame(Month, ET_guess) %>%
  mutate(Month = as.character(Month)) %>%
  as_tibble()

df <- hydro_heads %>%
  mutate(Month = as.character(as.numeric(str_sub(Date, 6, 7))))

df <- left_join(df, temp, by = "Month")
                
ET_elevation_head_gradient_correlation <- ggplot(data = hydro_heads,
                                                 mapping = aes(x = abs(elevation_gradient),
                                                               y = abs(head_gradient),
                                                               color = ET_guess)) +
  geom_point()

(ET_elevation_head_gradient_correlation)

rm(temp)


# XXX Head gradients correlated with aggregate wtr lvl ---------------------------------------



# XXX Correlation Matrix between head gradients ---------------------------



# ????? Scratch space -----------------------------------------------------------

# hmm <- hydro_heads %>% 
#   filter(Catchment == "Baltimore Corner") %>% 
#   mutate(head_gradient_magnitude = abs(head_gradient)) %>% 
#   select(c(head_gradient_magnitude, Date, dly_mean_wtrlvl_allsites))
# 
# 
# hmm_plot <- ggplot(data = hmm, 
#                    mapping = aes(x = dly_mean_wtrlvl_allsites,
#                                  y = head_gradient_magnitude)) +
#   geom_point() +
#   geom_smooth()
# 
# (hmm_plot)
# 
# hmm_model <- lm(data = hmm)



display.brewer.all()
