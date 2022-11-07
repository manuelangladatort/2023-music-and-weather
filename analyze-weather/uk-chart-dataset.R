library(tidyverse)

uk_raw <- read.csv('/Users/harin/Library/Mobile Documents/com~apple~CloudDocs/Research/Projects/globalPOP/dataset/Latest/Week Raw/GBR_week_raw_04012021.csv')
spotify_search <- read.csv('UK-spotify-unique.csv')

uk_raw <- rename(uk_raw, songID = SongID, year = Year, date = Date, week_num = Week_num, rank = Rank)
uk_raw <- uk_raw %>% 
  group_by(songID, year, week_num) %>%
  slice(1) %>%
  ungroup()

spotify_search <- rename(spotify_search, songID = SongTextID)
spotify_search$songID <- spotify_search$songID %>% str_replace_all(' \\| ', '_')

uk_raw <- left_join(uk_raw, spotify_search) %>%
  arrange(date, rank)
uk_raw$month <- uk_raw$date %>% as.Date %>% format("%B")
uk_raw$month_num <- uk_raw$date %>% as.Date %>% format("%m") %>% as.numeric()


### Monthly Data ####
agg_month <- uk_raw %>%
  group_by(year, month, month_num, songID) %>%
  slice(1) %>% # take only unique songs in a given month
  group_by(year, month, month_num) %>%
  summarise_at(vars(danceability:duration_ms), mean, na.rm = T) %>%
  arrange(year, month_num) %>%
  ungroup()

# exploring monthly change on all features
agg_month %>%
  pivot_longer(cols = danceability:duration_ms) %>%
  ggplot(aes(month_num, value)) +
    geom_smooth() +
    facet_wrap(~ name, scales = 'free')

# save monthly
write.csv(agg_month, '../data/spotify_monthly_aggregate.csv', row.names = FALSE)



### Top Bottom Data ####
# check when top 100 starts
top_n <- uk_raw %>% 
  group_by(year, week_num) %>%
  summarise(n = n())

# it's top 100 since 1994-02-06	
from_1994 <- uk_raw %>%
  filter(date > '1994-02-06') %>%
  arrange(date, rank) %>%
  mutate(top_bottom = case_when(
    rank %in% c(1:20) ~ 'top20',
    rank %in% c(21:80) ~ 'middle',
    rank %in% c(81:100) ~ 'bottom20'
  ))

# exploring monthly trajectory differences
top_bottom_agg <- from_1994 %>%
  group_by(year, month, month_num, songID, top_bottom) %>%
  slice(1) %>%
  group_by(year, month, month_num, top_bottom) %>%
  summarise_at(vars(danceability:duration_ms), mean, na.rm = T) %>%
  arrange(year, month_num) %>%
  ungroup() # remove duplicate enteries in the top bottom category


# exploring monthly change on all features
top_bottom_agg %>%
  pivot_longer(cols = danceability:duration_ms) %>%
  ggplot(aes(month_num, value, colour = top_bottom)) +
  geom_smooth() +
  facet_wrap(~ name, scales = 'free')

write.csv(top_bottom_agg, '../data/spotify_top_bottom_aggregate.csv', row.names = FALSE)
  