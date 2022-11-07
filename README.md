## Here comes the sun: Music features of popular songs reflect prevailing weather conditions
Data and analysis code supporting the manuscript, "Here comes the sun: Music features of popular songs reflect prevailing weather conditions" 

## Analysis Scripts
1. analyze-weather/analysis.R: main analysis code
2. analyze-weather/functions.R: methods supporting analysis and plotting
3. analyze-weather/prepare-spotify-data.R: script to prepare Spotify data for analysis
4. analyze-weather/prepare-weather-data.R: script to prepare weather data for analysis

## Datasets
1. data/spotify_monthly_aggregate.csv: aggregated Spotify data (month level)
2. data/spotify_top10_bottom10_aggregate.csv: aggregated Spotify data (month level) of top and bottom 10 songs
3. data/UK-spotify-unique.csv: song-level data with music features and metadata
4. data/weather_data.csv: aggregated weather data (generated with prepare-weather-data.R)
5. data/weather-music-1953-2019.csv: aggregated data with weather and music features data (generated with prepare-spotify-data.R)
