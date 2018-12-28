# Modeling forecasts with Prophet (version 0.3)
# Then setting goals and finding a matching forecast
# Adam Stone - adamstone@gmail.com
# github.com/foundinblank/setting-goals-using-prophet-models

# Load packages
library(tidyverse)
library(prophet)
library(lubridate)
library(padr)

# Load bikeshare data
daily_rides <- read_csv("./data/metrobike_daily_rides.csv")

