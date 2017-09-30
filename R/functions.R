# all station ids in a city
station_in_city <- function(cityname) {
  station %>% 
    filter(city == cityname) %>% 
    select(id) %>% 
    t %>% 
    as.integer()
}

# holidays in a year
holidays <- function(year) {
  
  allholidays <- c(USNewYearsDay, USMLKingsBirthday, USMemorialDay, USIndependenceDay, USLaborDay, USThanksgivingDay, USChristmasDay, USPresidentsDay)
  
  holidaydates <- c()
  for (i in 1:length(allholidays)) {
    x <- allholidays[[i]](year)
    holidaydates <- append(holidaydates, x)
  }
  
  holidaydates <- holidaydates %>% 
    as.Date() %>% append(as.Date(c("2015-11-27", "2015-12-24", "2015-12-31")))
  
  return(sort(holidaydates))
}

# h1 <- weather1 %>% filter(weekday == "0") %>%  select(pdt, num_trips) %>% filter(num_trips < 1000) %>% select(pdt) %>% t %>% as.Date
