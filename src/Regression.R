# linear regression on num_of_trips ~ weather variables
trip_daily <- trip %>% 
  as_tbl_time(index = start_date) %>% 
  time_summarise(period = "daily",
                 num_trips = n()) %>% 
  mutate(start_date = as.Date(start_date))

weather <- weather %>% 
  left_join(trip_daily, by = c("pdt" = "start_date"))

weather <- weather %>% 
  mutate(day_of_week = pdt %>% strftime("%A"),
         week_nr = pdt %>% strftime("%W"),
         weekday = pdt %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend"))

weather_weekday <- weather %>% 
  filter(weekday == "weekday")

weather_weekend <- weather %>% 
  filter(weekday == "weekend")

# weekday
l1 <- regsubsets(data = weather_weekday,
                 num_trips ~ 
                   mean_temperature_f +
                   precipitation_in +
                   mean_visibility_miles +
                   mean_humidity +
                   mean_wind_speed_mph +
                   cloud_cover,
                 method = "forward", nvmax = 15)

s1 <- summary(l1)
s1
s1$cp

# select one will smallest Mallow's Cp
formula1 = "num_trips ~ 
  mean_temperature_f +
  precipitation_in +
  mean_visibility_miles +
  mean_wind_speed_mph"

fit1 <- lm(data = weather_weekday,
           formula = formula1)

summary.fit1 <- summary(fit1)
summary.fit1

# weekdend
l2 <- regsubsets(data = weather_weekend,
                 num_trips ~ 
                   mean_temperature_f +
                   precipitation_in +
                   mean_visibility_miles +
                   mean_humidity +
                   mean_wind_speed_mph +
                   cloud_cover,
                 method = "forward", nvmax = 15)

s2 <- summary(l2)
s2
s2$cp

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(fit1)

# select one will smallest Mallow's Cp
formula2 = "num_trips ~ 
  mean_temperature_f +
  cloud_cover +
  precipitation_in +
  mean_humidity"

fit2 <- lm(data = weather_weekend,
           formula = formula2)

summary.fit2 <- summary(fit2)
summary.fit2

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(fit2)

# plot(lm(data = weather_weekday,
#         formula = num_trips ~ mean_temperature_f))
# 
# plot(lm(data = weather_weekday,
#         formula = num_trips ~ mean_visibility_miles))
# 
# plot(lm(data = weather_weekday,
#         formula = num_trips ~ precipitation_in))
# 
# plot(lm(data = weather_weekday,
#         formula = num_trips ~ cloud_cover))

# season should be a variabled


