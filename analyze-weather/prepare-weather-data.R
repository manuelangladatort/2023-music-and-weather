# create weeather dataset
library(readxl)
airfrost_1960_2021 <- read_excel("../data/weather/airfrost_1960.2021.xlsx")
daysrain_1950_2021 <- read_excel("../data/weather/daysrain_1950.2021.xlsx")
maxtemp_1950_2021 <- read_excel("../data/weather/maxtemp_1950.2021.xlsx")
rainfall_1950_2021 <- read_excel("../data/weather/rainfall_1950.2021.xlsx")
sunshine_1950_2021 <- read_excel("../data/weather/sunshine_1950.2021.xlsx")

d.airfrost =airfrost_1960_2021 %>% 
  select(-win:-ann) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "airfrost")
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


weather_data = d.maxtemp %>%
  left_join(d.sunshine, by = c('year', 'month')) %>%
  left_join(d.rainfall, by = c('year', 'month')) %>%
  left_join(d.daysrain, by = c('year', 'month')) %>%
  left_join(d.airfrost, by = c('year', 'month'))

weather_data$month <- factor(weather_data$month, 
                             levels = c("jan", "feb", "mar","apr", "may",
                                        "jun", "jul","aug", "sep", "oct", 
                                        "nov","dec"),
                             labels = c("January", "February", "March","April", "May",
                                        "June", "July","August", "September", "October", 
                                        "November","December"))

# save
write_csv(weather_data, "weather_data.csv")


