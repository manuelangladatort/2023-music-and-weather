################################################################################
# Music and weather associations
# Author: Manuel Anglada-Tort
# Script: Script to prepare Spotify data
################################################################################
library(tidyverse)

library(haven)
weather_data <- read_csv("../data/weather_data.csv") %>% 
  filter(year %in% 1953:2019) %>%
  select(-rainfall)
spotify_monthly_aggregate <- read_csv("../data/spotify_monthly_aggregate.csv") %>% filter(year %in% 1953:2019)


#########################################################
# merge weather with monthly aggregated Spotify features
#########################################################
cor.test(spotify_monthly_aggregate$year, spotify_monthly_aggregate$danceability)
cor.test(weather_data$year, weather_data$maxtemp)

# merge
clean_top100 <- left_join(weather_data, spotify_monthly_aggregate , by = c('year', 'month')) %>%
  group_by(year) %>%
  mutate(
    raw.maxtemp = maxtemp,
    raw.sunshine = sunshine,
    raw.daysrain = daysrain,
    raw.airfrost = airfrost
  ) %>%
  mutate_at(vars(maxtemp:duration_ms), scale)

write_csv(clean_top100, "weather-music-1953-2019.csv")

