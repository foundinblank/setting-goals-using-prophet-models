# Modeling forecasts with Prophet (version 0.3)
# Then setting goals and finding a matching forecast
# Adam Stone - adamstone@gmail.com
# http://www.github.com/foundinblank/setting-goals-using-prophet-models


# Setting Up --------------------------------------------------------------

# Load packages
library(tidyverse)
library(prophet)
library(lubridate)
library(padr)
library(hrbrthemes)

# Load bikeshare data
daily_rides <- read_csv("./data/metrobike_daily_rides.csv")
head(daily_rides)

# Visualize
daily_rides_p1 <- daily_rides %>%
  ggplot(aes(x = date, y = rides)) +
  geom_line(color = "medium blue") +
  scale_x_date(date_labels = "%b. %Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "", 
       title = "Daily LA Metro Bike Rides") +
  hrbrthemes::theme_ipsum_rc()
daily_rides_p1
ggsave("./figs/daily_rides.png", daily_rides_p1, width = 10, height = 5)

# Visualize weekend/weekday trends 
daily_rides <- daily_rides %>%
  mutate(weekday = wday(date, label = T)) %>%
  mutate(type = case_when(
    weekday == "Sat" ~ "weekend",
    weekday == "Sun" ~ "weekend",
    TRUE ~ "weekday"
  ))

daily_rides_p2 <- daily_rides %>%
  ggplot(aes(x = date, y = rides, color = type)) +
  geom_smooth(se = F) +
  scale_x_date(date_labels = "%b. %Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "", 
       title = "Daily LA Metro Bike Rides", 
       color = "") +
  hrbrthemes::theme_ipsum_rc()
daily_rides_p2
ggsave("./figs/daily_rides_by_weekday.png", daily_rides_p2, width = 10, height = 5)

# Forecasting with Prophet ------------------------------------------------
# Sales has set a target of 2,000 rides on August 1, 2019. 
# Will we get there? 

# Prophet requires columns `ds` and `y` for dates and values respectively
df <- daily_rides %>%
  rename(ds = date, y = rides) %>%
  select(ds, y)

# Create prophet model
# Reduce changepoints parameter from 0.05 to 0.01 to make the trend less flexible
m <- prophet(df, changepoint.prior.scale = 0.01)

# Extend dataframe to August 1, 2019 (306 days between end of df & 2018-08-01)
future <- make_future_dataframe(m, periods = 306)
tail(future)

# Predict to August 1, 2019
# yhat contains forecast predictions, with upper/lower ranges
forecast <- predict(m, future)
forecast %>% 
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail()

# Visualize
forecast_p <- plot(m, forecast) + 
  labs(x = "", 
       y = "rides", 
       title = "Projected Daily Rides", 
       subtitle = "To August 1, 2019") +
  theme_ipsum_rc()
forecast_p
ggsave("./figs/projected_daily_rides.png", forecast_p, width = 10, height = 5)

# Show plot trends 
prophet_plot_components(m, forecast) 
