## Here comes the sun: Music features of popular songs reflect prevailing weather conditions
Data and analysis code supporting the manuscript, "Here comes the sun: Music features of popular songs reflect prevailing weather conditions", accepted in Royal Society Open Science. 

Data and code have been archived within the Zenodo repository: https://zenodo.org/badge/latestdoi/562985935


## Analysis
1. *analyze-weather/analysis.R:* main analysis code
2. *analyze-weather/functions.R:* supporting methods
3. *analyze-weather/prepare-data.R:* code to prepare the raw data to the monthly aggregated data used in the main analysis

## Data
1. *data/spotify_monthly_aggregate.csv:* aggregated music featrures data from Spotify (month level)
2. *data/spotify_top10_bottom10_aggregate.csv:* aggregated music featrures data from Spotify (month level) of the top, middle, and bottom 10 songs
3. *data/UK-spotify-unique.csv:* song-level data with music features and metadata
4. *weather/...:* raw weather datasets from the Meteorological Office of the United Kingdom
4. *data/weather_data.csv:"" aggregated raw weather data
5. "data/weather-music-1953-2019.csv:"" aggregated data with weather and music features (generated with prepare-data.R) -> main dataset

