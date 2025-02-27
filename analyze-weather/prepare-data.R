################################################################################
# RSOS : Music features of popular songs reflect prevailing weather conditions
# Author: Manuel Anglada-Tort
# Script: Prepare weather dataset
################################################################################
library(tidyverse)
library(readxl)

# Structure: (1) music features + (2) weather data + (3) merge both datasets

################################################################################
# 1, music features data (aggregated)
################################################################################
spotify_monthly_aggregate <- read_csv("../data/spotify_monthly_aggregate.csv") %>% 
  filter(year %in% 1953:2019)

################################################################################
# 2. weather data
################################################################################

## merge separate datasets
daysrain_1950_2021 <- read_excel("../data/weather/daysrain_1950.2021.xlsx")
maxtemp_1950_2021 <- read_excel("../data/weather/maxtemp_1950.2021.xlsx")
rainfall_1950_2021 <- read_excel("../data/weather/rainfall_1950.2021.xlsx")
sunshine_1950_2021 <- read_excel("../data/weather/sunshine_1950.2021.xlsx")

d.daysrain = daysrain_1950_2021 %>% 
  select(-win:-ann) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "daysrain")
d.rainfall = rainfall_1950_2021 %>% 
  select(-win:-ann) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "rainfall")
d.sunshine = sunshine_1950_2021 %>% 
  select(-win:-ann) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "sunshine")
d.maxtemp = maxtemp_1950_2021 %>% 
  select(-win:-ann) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "maxtemp")

## join
weather_data = d.maxtemp %>%
  left_join(d.sunshine, by = c('year', 'month')) %>%
  left_join(d.rainfall, by = c('year', 'month')) %>%
  left_join(d.daysrain, by = c('year', 'month')) 

weather_data$month <- factor(weather_data$month, 
                             levels = c("jan", "feb", "mar","apr", "may",
                                        "jun", "jul","aug", "sep", "oct", 
                                        "nov","dec"),
                             labels = c("January", "February", "March","April", "May",
                                        "June", "July","August", "September", "October", 
                                        "November","December"))

weather_data <- weather_data %>% 
  filter(year %in% 1953:2019) %>%
  select(-rainfall)


##  save
# write_csv(weather_data, "weather_data.csv")


################################################################################
# 3. merge weather with music features
################################################################################

# Temporal trends over years is an important confound!
cor.test(spotify_monthly_aggregate$year, spotify_monthly_aggregate$danceability)
cor.test(weather_data$year, weather_data$maxtemp)

finalDT <- left_join(weather_data, spotify_monthly_aggregate , by = c('year', 'month')) %>%
  group_by(year) %>%
  mutate(
    raw.maxtemp = maxtemp,
    raw.sunshine = sunshine,
    raw.daysrain = daysrain
  ) %>%
  # scale variables by year to remove the temporal confound
  mutate_at(vars(maxtemp:duration_ms), scale)


# Indeed, we removed the confound with yearly trends
cor.test(finalDT$year, finalDT$danceability)
cor.test(finalDT$year, finalDT$maxtemp)


# save
# write_csv(finalDT, "weather-music-1953-2019.csv")



