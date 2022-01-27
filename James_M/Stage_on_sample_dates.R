#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Stage_on_sample_dates
# Coder: James Maze
# Date: 25 Jan 2021
# Purpose: To enumerate hydrologic conditions durring sampling campaigns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Notes:
#   - Need to automate sampling dates on the stage time series
#   - Need to finish the summary table & plot for stage on sampling dates. 


data_dir <- "Sampledates_FR_Stage"
  
# Read the stage file 
FR_Stage <- read_excel(paste0(data_dir, "FR_Stage.xlsx"),
                       skip = 4)

#Select necessary columns
FR_Stage <- FR_Stage[ , 1:2]

#Rename columns to make code manageable
FR_Stage <- FR_Stage %>% 
  mutate(Stage_inch_FR = `Stage (in)`) %>% 
  mutate(Date_time = `Date Time, GMT-05:00`) %>% 
  select(c(Date_time, Stage_inch_FR))


#Plot stage with sampling campaings
FR_Stage_ts <- ggplot(data = FR_Stage, 
       mapping = aes(x = Date_time, 
                     y = Stage_inch_FR)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_x_datetime(limits = as.POSIXct(c("2020-01-01", "2022-01-01"))) +
  ### At some point, fix this manual entry
  geom_vline(xintercept = as.POSIXct(c("2020-01-15 23:00:00", 
                                       "2020-03-10 23:00:00",
                                       "2020-07-28 23:00:00",
                                       "2020-09-24 00:00:00",
                                       "2020-11-04 00:00:00",
                                       "2021-02-28 00:00:00",
                                       "2021-05-12 00:00:00",
                                       "2021-06-21 00:00:00",
                                       "2021-11-12 23:00:00")), 
             size = 3, 
             color = "darkred") +
  ### At some point, fix this manual entry
  geom_vline(xintercept = as.POSIXct(c("2020-09-20 12:00:00",
                                       "2020-11-01 12:00:00",
                                       "2021-02-25 12:00:00",
                                       "2021-05-09 12:00:00",
                                       "2021-06-18 12:00:00",
                                       "2021-09-10 12:00:00",
                                       "2021-10-15 12:00:00",
                                       "2021-12-13 12:00:00")),
             size = 3, 
             color = "seagreen") +
  ### At some point, fix this manual entry
  geom_vline(xintercept = as.POSIXct(c("2021-09-13 12:00:00",
                                       "2021-10-18 12:00:00",
                                       "2021-12-16 12:00:00")),
             size = 3, 
             color = "midnightblue") +
  ggtitle("Sampling campaigns plotted with stage at FR-SW")

(FR_Stage_ts)


# Create histogram matching sampling dates to stage value
#!!! Need to finish!!!###
sampling <- read_excel(paste0(data_dir, "sampling_schedule.xlsx")) %>% 
  mutate(Date_chr = as.character(Sample_date)) %>% 
  select(Date_chr, Sample_type)

FR_points <- FR_Stage %>% 
  mutate(Date_chr = as.character(Date_time)) %>% 
  select(Date_chr, Stage_inch_FR) %>% 
  mutate(Date_chr = str_trunc(Date_chr, width = 10, side = "right", ellipsis = ""))

points <- left_join(sampling, FR_points, by = "Date_chr")

stage_histo <- ggplot(data = points, 
                      mapping = aes(x = Stage_inch_FR),
                                    color = Sample_type) +
  theme_bw() +
  geom_histogram(color = red)





