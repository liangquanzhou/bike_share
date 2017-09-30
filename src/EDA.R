source("config/settings.R")

#   ____________________________________________________________________________
#   Read data                                                               ####
station <- read_csv("data/201608_station_data.csv")
station <- station %>%
  mutate(installation = mdy(installation)) %>% 
  arrange(station_id)
colnames(station) <- c("id","name","lat","long","dock_count","city","installation_date")

trip <- read_csv("data/201608_trip_data.csv")
colnames(trip) <- c("id","duration","start_date","start_station_name","start_station_id","end_date","end_station_nanme","end_station_id","bike_id","subscription_type","zip_code")

trip <- trip %>% 
  mutate(start_date = mdy_hm(start_date),
         end_date = mdy_hm(end_date))

trip<- trip %>% 
  mutate(day_of_week = start_date %>% strftime("%A"),
         week_nr = start_date %>% strftime("%W"),
         weekday = start_date %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend"))

weather <- read_csv("data/201608_weather_data.csv") 
# rename
names(weather) <- 
  names(weather) %>% 
  gsub(" ", "_", .) %>% 
  gsub("([a-z])([A-Z])", "\\1\\_\\2", .) %>% 
  tolower()

weather <- weather %>% 
  mutate(events = ifelse(is.na(events), "None", events)) %>% 
  mutate(events = as.factor(events)) %>% 
  mutate(pdt = mdy(pdt))

weather <- weather %>% 
  mutate(precipitation_in = as.numeric(precipitation_in)) %>% 
  mutate(precipitation_in = ifelse(is.na(precipitation_in), 
                                   0, precipitation_in))

# only select the weather that for the SF. zip = 94107
weather <- weather %>% 
  filter(zip == 94107)

##  ............................................................................
##  Time - Based                                                            ####

# number of trips by day
trip %>% 
  as_tbl_time(index = start_date) %>%
  time_summarise(period = "daily",
                 num_trips = n()) %>% 
  mutate(day_of_week = start_date %>% strftime("%A"),
         week_nr = start_date %>% strftime("%W"),
         weekday = start_date %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend")) %>% 
  ggplot(aes(start_date, num_trips, color = weekday)) +
  geom_point() +
  geom_smooth()+
  labs(x = "Date",
       y = "Total Number of Bicycle Trips", 
       title = "Trips Each Day")

# number of trips by week days
trip %>% 
  mutate(day = wday(start_date, label = T, abbr = F)) %>% 
  count(day) %>% 
  ggplot(aes(day, n)) +
  geom_bar(stat = "identity") +
  labs(x = "Week Day",
       y = "Number of Trips", 
       title = "Number of Trips across a Week")

# number of trips in a day
trip %>% 
  mutate(time = hour(start_date) + minute(start_date)/60) %>% 
  ggplot(aes(time)) +
  geom_histogram(bins = 12*60) +
  geom_vline(xintercept = 9, color = 'orange')+
  geom_vline(xintercept = 17, color = 'red', alpha = 0.7) +
  annotate("text", x = 9, y = 0, label = "9:00 AM", color = "orange",
           size = 7) +
  annotate("text", x = 17, y = 0, label = "5:00 PM", color = "red", 
           size = 7) +
  labs(x = "Daytime 24 Hour Clock",
       y = "Number of Trips",
       title = "Number of Trips in a day")

# number of trip in a day - facet: weekday
trip %>% 
  mutate(time = hour(start_date) + minute(start_date)/60,
         weekday = start_date %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend")) %>% 
  ggplot(aes(time)) +
  geom_histogram(bins = 12*60) +
  labs(x = "Daytime 24 Hour Clock",
       y = "Number of Trips",
       title = "Number of Trips in a day") +
  facet_wrap(~weekday, scales = "free")

#   ____________________________________________________________________________
#   Duration - Based                                                        ####

# trip duration - in generall
trip %>% 
  mutate(duration_minutes = duration/60) %>% 
  ggplot(aes(duration_minutes)) +
  geom_histogram(bins = 60, fill = "white", color = "black") + 
  xlim(0,60) + 
  labs(x = "Minutes",
       y = "Number of Trips",
       title = "Trip Duration")

# trip duration - facet: weekday
trip %>% 
  mutate(duration_minutes = duration/60,
         weekday = start_date %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend")) %>% 
  ggplot(aes(duration_minutes)) +
  geom_histogram(bins = 100, fill = "white", color = "black") + 
  xlim(0,100) + 
  labs(x = "Minutes",
       y = "Number of Trips",
       title = "Trip Duration by weekday") +
  facet_wrap(~weekday, scales = "free", nrow = 2)

# mean trips duration ~ weekday
trip %>% 
  mutate(duration_minutes = duration/60, 
         weekday = start_date %>% strftime("%u") %>% as.numeric) %>% 
  mutate(weekday = if_else(weekday < 6, "weekday", "weekend")) %>% 
  group_by(day_of_week) %>% 
  summarise(mean_duration_minutes = mean(duration)/60) %>% 
  ggplot(aes(factor(day_of_week, weekdays(as.Date('1970-01-03') + 1:7)), 
             mean_duration_minutes)) +
  geom_bar(stat = "identity", width = 0.5, fill = "white", color = "black") +
  coord_flip() +
  labs(x = "",
       y = "Mean Trip Duration",
       title = "Mean Trip Duration ~ Weekdays")

#   ____________________________________________________________________________
#   Location - Based                                                        ####

# in this part only do analysis on San Jose
station_id_san_jose <- station_in_city("San Jose")

# add trip stat_long and end_long
trip_san_jose <- trip %>% 
  filter(start_station_id %in% station_id_san_jose) %>% 
  filter(end_station_id %in% station_id_san_jose) %>% 
  group_by(start_station_id, end_station_id) %>% 
  summarise(num_trip = n()) %>% 
  left_join(station %>% select(id, lat, long), 
            by = c("start_station_id" = "id")) %>% 
  rename(start_long = long, start_lat = lat) %>% 
  left_join(station %>% select(id, lat, long),
            by = c("end_station_id" = "id")) %>% 
  rename(end_long = long, end_lat = lat)

station_san_jose <- station %>% 
  filter(city == "San Jose") %>% 
  left_join(trip_san_jose %>% 
              summarise(num_trip = sum(num_trip)),
            by = c("id" = "start_station_id"))

# number of trips between stations in San Jose
g <- 
  ggplot(trip_san_jose) +
  geom_segment(aes(x=start_long, xend=end_long, y=start_lat, yend=end_lat, 
                   size=num_trip, colour=num_trip, alpha=num_trip)) +
  geom_point(data=station_san_jose, aes(x=long, y=lat), size=4) +
  geom_text_repel(data=station_san_jose, 
                  aes(x=long, y=lat, label=name), size=4) +
  theme_light(base_size=10) +
  xlab("") +
  ylab("") +
  scale_colour_gradientn(colors=c("#dddddd", "#20beff"), 
                         limits=c(0, max(trip_san_jose$num_trip)), 
                         name="Number of Trips") +
  scale_alpha(limits=c(0, max(trip_san_jose$num_trip)), guide=FALSE) +
  scale_size(limits=c(0, max(trip_san_jose$num_trip)), guide=FALSE) +
  coord_fixed() +
  theme_void()
  # theme(axis.line=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       panel.grid=element_blank(),
  #       panel.border=element_blank())
g

# using leaflet
l <- 
  leaflet(station) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = ~ long, lat = ~ lat, label = ~ name)
  # addCircleMarkers(
  #   radius = ~ num_trip*0.02,
  #   stroke = FALSE, 
  #   fillOpacity = 0.5,
  #   label = ~ name
  # ) %>% 
  # addPolylines(data = trip_san_jose,
  #              lng = ~ as.vector(rbind(start_long, end_long)),
  #              lat = ~ as.vector(rbind(start_lat, end_lat)),
  #              color = "grey"
  #                )
l
