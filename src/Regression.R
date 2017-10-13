source("src/EDA.R")
# linear regression on num_of_trips ~ weather variables

# daily trip numbers in SF
trip_daily_san_francisco <- trip %>% 
  filter(start_station_id %in% station_id_san_francisco) %>% 
  mutate(start_date = as.Date(start_date)) %>% 
  group_by(start_date) %>% 
  summarise(num_trips = n()) %>% 
  mutate(start_date = as.Date(start_date))

weather <- weather %>% 
  left_join(trip_daily_san_francisco, by = c("pdt" = "start_date"))

weather <- weather %>% 
  mutate(day_of_week = pdt %>% strftime("%A"),
         week_nr = pdt %>% strftime("%W"),
         weekday = pdt %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend")) %>% 
  mutate(weekday = as.character(as.numeric(factor(weekday)) - 1)) %>%
  mutate(zip = factor(zip))

weather <- weather %>% 
  select(pdt, weekday, mean_temperature_f, mean_dew_point_f, mean_humidity,
         mean_sea_level_pressure_in, mean_visibility_miles, 
         mean_wind_speed_mph, precipitation_in, cloud_cover,
         num_trips, events)

weather <- weather %>% 
  mutate(weekday = as.character(weekday))

# variable selection
# 1. basic formula
formula1 <- formula(
  num_trips ~
    1 + weekday +
    mean_temperature_f +
    precipitation_in +
    mean_visibility_miles +
    mean_humidity +
    mean_wind_speed_mph +
    cloud_cover +
    events
)

l1 <- regsubsets(data = weather, formula1, nvmax = 15)
s1 <- summary(l1)
# data.frame(s$outmat, RSS = s$rss, R2 = s$rsq, Cp = s$cp, BIC = s$bic)
# s$which
s1$cp
s1$which[which.min(s1$cp),]

# after variable selection, fit the linear model on weather data
# fit model
formula2 <- formula(
  num_trips ~ 1 +
    weekday1 + 
    mean_temperature_f +
    mean_visibility_miles +
    cloud_cover +
    `eventsFog-Rain` +
    eventsNone +
    eventsRain +
    `eventsRain-Thunderstorm`)

weather_frame <- model.matrix(formula1, weather) %>% as.tibble()

fit <- lm(data = weather_frame %>% 
            mutate(num_trips = weather$num_trips), formula2)

summary(fit)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(fit)

# holiday may affect
p10 <- weather %>% 
  ggplot(aes(x = pdt, y = num_trips, color = events)) +
  geom_point(aes(shape = weekday, size = precipitation_in)) +
  scale_color_brewer(palette = "Set1")

# days near holidays have a lower num_trips
w1 <- weather %>% 
  filter(weekday == "0", num_trips < 1000) %>% 
  select(pdt, num_trips)

holidaydates <- holidays(c(2015, 2016))

weather_holiday_adjust <- weather %>% 
  mutate(weekday = ifelse(
    pdt %in% holidaydates, "1", weekday
  ))

l <- regsubsets(data = weather_holiday_adjust, formula1, nvmax = 15)
s <- summary(l)
# data.frame(s$outmat, RSS = s$rss, R2 = s$rsq, Cp = s$cp, BIC = s$bic)
# s$which
s$cp
s$which[which.min(s$cp),]

formula3 <- formula(
  sqrt(num_trips) ~ 1 + 
    weekday1 +
    mean_temperature_f +
    mean_visibility_miles +
    eventsRain +
    `eventsRain-Thunderstorm`
)

weather_holiday_adjust_frame <- 
  model.matrix(formula1, weather_holiday_adjust) %>% 
  as.tibble()

fit_holiday_adjust <- lm(data = weather_holiday_adjust_frame %>% 
                           mutate(num_trips = weather$num_trips), 
                         formula = formula3)

summary_fit_holiday_adjust <- summary(fit_holiday_adjust)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(fit_holiday_adjust)




