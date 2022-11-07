library(tidyverse)
library(ggcorrplot)

library(haven)
# DT5_old <- read_sav("~/Documents/r-projects/Lyrics_Weather/music-and-weather/data/MONTHLY_working-timelags_2019.08.04.sav")
weather_data <- read_csv("../data/weather_data.csv") %>% 
  filter(year %in% 1953:2019) %>%
  select(-rainfall)
# DT100 <- read_csv("../data/uk_top100_spotify_match.csv")
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


####################################
# correlations by decade #
####################################
clean_top100_decades = clean_top100 %>%
  mutate(decade = case_when(
    year >= 1950 & year < 1960 ~ '50s-60s',
    year >= 1960 & year < 1970 ~ '60s-70s',
    year >= 1970 & year < 1980 ~ '70s-80s',
    year >= 1990 & year < 2000 ~ '90s-00s',
    year >= 2000 ~ 'modern'
  ))

music_var <- c('PCA1')
weather_var <- c('maxtemp', 'sunshine', 'daysrain', 'airfrost')
decades <- c('50s-60s', '60s-70s', '70s-80s', '90s-00s', 'modern')

plot_df = list()
for (d in 1:length(decades)) {
  ft_decade <- clean_top100_decades %>%
    filter(decade == decades[d])
  
  storage = list()
  for (w in 1:length(weather_var)) {
    
    cor_store = list()
    for (i in 1:length(music_var)) {
      cor_store[[i]] <- tibble(
        weather =  weather_var[w],
        music_feature = music_var[i],
        correlation = cor(ft_decade[,  weather_var[w]], ft_decade[, music_var[i]]) %>% as.numeric()
      )
    }
    storage[[w]] <- do.call(rbind, cor_store)
  }
  plot_df[[d]] <- cbind(
    decade = decades[d],
    do.call(rbind, storage))
}
plot_df <- do.call(rbind, plot_df)

plot_df %>%
  ggplot(aes(music_feature, correlation, fill = weather)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ weather + decade) +
  theme_minimal() +
  labs(x = '')
