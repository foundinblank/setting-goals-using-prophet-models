# Combine CSVs and calculate daily totals
# Data files downloaded from Los Angeles Metro Bike Share
# https://bikeshare.metro.net/about/data/

library(tidyverse)
library(lubridate)

# Get list of files
filelist <- list.files("./data", "csv") %>%
  paste0("./data/", .)

# Read all CSV files and set start_time to datetime format 
df <- filelist %>%
  map_dfr(read_csv, col_types = cols(start_time = col_character(),
                                     end_time = col_character(),
                                     bike_id = col_character(),
                                     end_lat = col_character(),
                                     end_lon = col_character())) %>%
  select(start_time) %>%
  mutate(start = mdy_hm(start_time))

# The start_time formatting changed from mdy_hm to ymd_hms 
# This handles rows that didn't parse above
df_no_secs <- df %>%
  filter(is.na(start)) %>%
  mutate(start = ymd_hms(start_time)) %>%
  select(start)

# Combine both sets
starts <- df %>%
  filter(!is.na(start)) %>%
  select(start) %>%
  bind_rows(df_no_secs)

# Calculate daily rides
daily_rides <- starts %>%
  mutate(date = as.Date(start)) %>%
  group_by(date) %>%
  summarise(rides = n())

# Visualize
daily_rides %>%
  ggplot(aes(x = date, y = rides)) +
  geom_line()

# There are three crazy spikes, once a year around late September to early October.
# I don't know why, so I'll just take these out for now, and write to a CSV file.
daily_rides <- daily_rides %>%
  filter(rides < 1485) %>%
  write_csv("./data/metrobike_daily_rides.csv")
  