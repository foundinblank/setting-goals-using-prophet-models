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
# What's our forecasted/projected rides for September 1, 2019?

# Prophet requires columns `ds` and `y` for dates and values respectively
df <- daily_rides %>%
  rename(ds = date, y = rides) %>%
  select(ds, y)

# Create prophet model
# Reduce changepoints parameter from 0.05 to 0.01 to make the trend less flexible
m <- prophet(df, changepoint.prior.scale = 0.01)

# Extend dataframe to September 1, 2019 (337 days between end of df & 2018-09-01)
future <- make_future_dataframe(m, periods = 337)
tail(future)

# Predict to September 1, 2019
# yhat contains forecast predictions, with upper/lower ranges
forecast <- predict(m, future)
forecast %>% 
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  tail()

# What's the forecast for September 1, 2019?
forecasted_rides <- forecast %>%
  arrange(desc(ds)) %>%
  slice(1) %>%
  pull(yhat) %>%
  round()
forecasted_rides

# Visualize
forecast_p <- plot(m, forecast) + 
  labs(x = "", 
       y = "rides", 
       title = "Projected Daily Rides", 
       subtitle = "To September 1, 2019") +
  theme_ipsum_rc()
forecast_p
ggsave("./figs/projected_daily_rides.png", forecast_p, width = 10, height = 5)

# Show plot trends 
prophet_plot_components(m, forecast)

# Setting Goals -----------------------------------------------------------
# Using Prophet, we forecasted 1,272 rides. We have a goal of 1,750.
# If we were aiming for 1,750, what should our trend look like, considering our model?

# Generate our predictive samples. The default is 1000 samples.
# Sampling can be adjusted by setting uncertainty.samples in the prophet() call
samples <- predictive_samples(m, future)
yhat_samples <- samples[['yhat']] %>%
  as_tibble() %>%
  bind_cols(future) %>%
  select(ds, everything())

# Now find the sample that contains the forecast closest to our goal
goal_rides <- 1350
end_forecast <- yhat_samples %>%
  filter(ds == ymd('2019-09-01')) %>%
  gather(key = sample, value = rides, V1:V1000) %>%
  mutate(difference = abs(rides - goal_rides)) %>%
  slice(which.min(difference))
goal_sample <- end_forecast$sample

# And combine that sample vector with the date vector
# But we'll remove all forecasts for dates we already have historical data for
goal_trend <- select(yhat_samples, !!goal_sample) %>%
  rename(goal = !!goal_sample) %>%
  bind_cols(future) %>%
  mutate(goal = if_else(ds > max(df$ds), goal, NA_real_))

# Now we'll combine the df df, forecast df, and goal df for comparison & plotting
forecast_and_goal <- left_join(forecast, goal_trend, by = "ds") %>%
  mutate(ds = ymd(ds)) %>%
  left_join(df, by = "ds") %>%
  mutate(ds = as.POSIXct.Date(ds)) # Necessary for plot()

# Visualize
plot(m, forecast) + 
  geom_point(data = forecast_and_goal, 
            aes(x = ds, y = goal), 
            color = "red",
            alpha = 0.5,
            size = 0.75) +
  labs(x = "", 
       y = "rides", 
       title = "Projected & Goal Daily Rides", 
       subtitle = "To September 1, 2019") +
  theme_ipsum_rc()

plot(m, forecast) + 
  geom_point(data = forecast_and_goal, 
             aes(x = ds, y = goal), 
             color = "red",
             alpha = 0.5,
             size = 0.75) +
  labs(x = "", 
       y = "rides", 
       title = "Projected & Goal Daily Rides", 
       subtitle = "To September 1, 2019") +
  theme_ipsum_rc()

forecast_and_goal %>%
  filter(is.na(y)) %>%
  ggplot(aes(x = ds)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, y = yhat), alpha = 0.2, fill = "#0072B2") +
  geom_line(aes(y = yhat), color = "#0072B2") +
  geom_point(aes(y = goal), color = "red")

target <- forecast_and_goal %>%
  filter(is.na(y)) %>%
  mutate(target = case_when(
    goal > yhat ~ goal,
    goal < yhat ~ yhat
  )) %>%
  select(ds, yhat, yhat_lower, yhat_upper, goal, target)

target %>%
  ggplot(aes(x = ds)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, y = yhat), alpha = 0.2, fill = "#0072B2") +
  geom_smooth(aes(y = yhat), color = "#0072B2") +
  geom_smooth(aes(y = goal), color = "red") +
  geom_smooth(aes(y = target), color = "green")
