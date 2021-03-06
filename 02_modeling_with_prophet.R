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
library(ggrepel)

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
  geom_smooth(color = "red") +
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
# Using Prophet, we forecasted 1,272 rides. We have a goal of 1,500.
# If we were aiming for 1,500, what should our trend look like, considering our model?

# Generate our predictive samples. The default is 1000 samples.
# Sampling can be adjusted by setting uncertainty.samples in the prophet() call
# m is our Prophet model, and future is our future dataframe (a column of dates)
samples <- predictive_samples(m, future)
yhat_samples <- samples[['yhat']] %>%
  as_tibble() %>%
  bind_cols(future) %>%
  select(ds, everything())

# Now find the sample that contains the forecast closest to our goal
goal_rides <- 1500
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
  mutate(ds = as.POSIXct.Date(ds)) %>% # Necessary for plot()
  mutate(difference = goal - yhat)

# What's the average differnce between our goal and the original projections? 
mean(forecast_and_goal$difference, na.rm = T)

# Visualize
adjusted_p <- plot(m, forecast) + 
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

adjusted_p
ggsave("./figs/projected_and_goal_daily_rides.png", adjusted_p, width = 10, height = 5)

# Zoom in chart to just future dates
adjusted_p_zoomed <- forecast_and_goal %>%
  filter(is.na(y)) %>%
  ggplot(aes(x = ds)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, y = yhat), alpha = 0.2, fill = "#0072B2") +
  geom_line(aes(y = yhat), color = "#0072B2") +
  geom_point(aes(y = goal), color = "red", alpha = 0.5) +
  labs(x = "", 
       y = "rides", 
       title = "Projected & Goal Daily Rides", 
       subtitle = "To September 1, 2019") +
  theme_ipsum_rc()

adjusted_p_zoomed
ggsave("./figs/projected_and_goal_daily_rides_zoomed.png", adjusted_p_zoomed, width = 10, height = 5)


# Build 3 projections visualizations
target <- forecast_and_goal %>%
  filter(is.na(y)) %>%
  mutate(target = case_when(
    goal > yhat ~ goal,
    goal < yhat ~ yhat
  )) %>%
  select(ds, yhat, yhat_lower, yhat_upper, goal, target)

target_lines <- target %>%
  select(ds, yhat, goal, target) %>%
  rename(original_projection = yhat,
         adjusted_projection = goal,
         high_target = target) %>%
  gather(key = projection_type, value = rides, original_projection:high_target) %>%
  mutate(projection_type = fct_relevel(projection_type, c("original_projection", "adjusted_projection", "high_target")))

target_labs <- target_lines %>%
  filter(ds >= "2019-09-01") %>%
  mutate(rides = round(rides,0))

three_targets <- target %>%
  ggplot(aes(x = ds)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, y = yhat), alpha = 0.2, fill = "#0072B2") +
  geom_smooth(data = target_lines, 
              aes(x = ds, y = rides, color = projection_type), 
              se = F, 
              method = "loess",
              span = 0.5) +
  scale_color_discrete() +
  labs(x = "", 
       y = "rides", 
       title = "Different Ridership Projections", 
       subtitle = "To September 1, 2019",
       color = "") +
  theme_ipsum_rc() +
  theme(legend.position = "top")

three_targets
ggsave("./figs/three_targets.png", three_targets, width = 10, height = 5)



