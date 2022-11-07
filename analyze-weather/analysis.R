################################################################################
# Music and weather associations
# Author: Manuel Anglada-Tort
# Script: Main analysis paper
################################################################################


##################
# import section
##################
# load libraries
library(readr)
library(tidyverse)
library(ggridges)
library(mgcv)
library(mgcViz) #visuals for mgcv
library(cowplot)
library(tidymv)
library(viridis)
library(factoextra)
library(psych)


#######################
# data and  functions
#######################
source("functions.R") # main functions for plotting and analysis

weather_music_1953_2019 <- read_csv("../data/weather-music-1953-2019.csv")

weather_music_1953_2019$month <- factor(weather_music_1953_2019$month, 
                     levels = c("January", "February", "March","April", "May",
                                "June", "July","August", "September", "October", 
                                "November","December")) # sort order months

###################
# PCA
###################
set.seed(1234)
data_to_pca = weather_music_1953_2019 %>%
  select(danceability:duration_ms) %>%
  select(-duration_ms, -mode, -key, -liveness, -instrumentalness) # low-level features excluded: key, mode, duration

res.pca <- prcomp(data_to_pca, center = TRUE,scale. = TRUE) # exclude strictly music features: mode, tmepo, duration
summary(res.pca)
fviz_eig(res.pca)
plot_PCA = fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

ggsave("plot_PCA_main.pdf", plot_PCA, 
       width = 15, height = 15, units = "cm",
       dpi=300)

weather_music_1953_2019$PCA1 = res.pca$x[,1] # add PCA1: energetic & positive
weather_music_1953_2019$PCA2 = res.pca$x[,2] # add PCA2: interpreation unclear

# controls
eig.val <- get_eigenvalue(res.pca)
eig.val
data_matrix = cor(data_to_pca, use = 'complete.obs')
round(data_matrix, 2)
cortest.bartlett(data_to_pca)
KMO(data_matrix)


###################
# analysis section
###################
library(viridis)
library(scales)
show_col(viridis_pal(option = "C")(3))

rain_color = "#0D0887FF"
sun_color = "#F0F921FF"
temp_color = "#CC4678FF"
weather_cols = c(temp_color, sun_color, rain_color)


#####################
# Figure 1: Weather
#####################
weather_data = weather_music_1953_2019 %>% select(year,month, raw.maxtemp:raw.airfrost) %>%
  rename(maxtemp=raw.maxtemp, sunshine=raw.sunshine, daysrain=raw.daysrain, airfrost=raw.airfrost)

## Weather data over time
temp_time = plot_weather_time(weather_data, temp_color, "Temperature","Temperature", 10) 
sun_time = plot_weather_time(weather_data, sun_color, "Sunshine", "Sunshine",10) 
rain_time = plot_weather_time(weather_data, rain_color,"Rain", "Days of Rain", 10) 
# airfrost_time = plot_weather_time(weather_data, airfrost_color,"Airfrost", "Days of air frost", 10) 

## Weather data over months
temperature_hist = plot_hist_weather(weather_data, maxtemp, "Temperature", 10)
sun_hist = plot_hist_weather(weather_data, sunshine,"Sunshine", 10)
rain_hist = plot_hist_weather(weather_data, daysrain, "Rainfall", 10)
# airfrost_hist = plot_hist_weather(weather_data, airfrost, "Air frost", 10)

## make final figure
figure1 = plot_grid(temp_time, temperature_hist,
          sun_time, sun_hist,
          # airfrost_time, airfrost_hist,
          rain_time, rain_hist,
          rel_widths = c(1.35,1),
          ncol = 2, align = "vh")

# save high resolution
ggsave("figure1.R.pdf", figure1, 
       width = 20, height = 30, units = "cm",
       dpi = 500) 

#################################
# Figure 2: GAM + Season pattern
#################################
# scaling by year works
cor.test(weather_music_1953_2019$year, weather_music_1953_2019$raw.maxtemp)
cor.test(weather_music_1953_2019$year, weather_music_1953_2019$maxtemp)

# long format
data_long = weather_music_1953_2019 %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
data_long$measure <- factor(data_long$measure, levels = c("maxtemp", 
                                                          "sunshine", 
                                                          # "airfrost", 
                                                          "daysrain")) # reorder

out1 = gam(PCA1 ~ measure + s(value, by = measure), data = data_long, method = "REML") 
s1 = summary(out1)
concurvity(out1, full = TRUE)

par(mfrow = c(2, 2))
gam.check(out1)

out2 = gam(PCA2 ~ measure + s(value, by = measure), data = data_long, method = "REML") 
s2 = summary(out2)
gam.check(out2)

plot_out1 = plot_gam_facet(out1, 
                          paste0("GAM PCA1 - R2 = ", round(s1$r.sq*100, 2), "%"),
                          "Weather measure (z-score)", # x label
                          "Positive & Intense Music", #  y label
                          weather_cols,
                          10, # size font ,
                          c(-3,2) # y lims
)

plot_out2 = plot_gam_facet(out2, 
                           paste0("GAM PCA2 - R2 = ", round(s2$r.sq*100, 2), "%"),
                           "Weather measure (z-score)", # x label
                           "Negative & Calm Music", #  y label
                           weather_cols,
                           10, # size font ,
                           c(-3,2) # y lims
)


sum_weather_music_month = 
  weather_music_1953_2019 %>% 
  pivot_longer(cols=PCA1:PCA2, names_to="pca_factor", values_to="pca_score") %>%
  group_by(month, pca_factor) %>%
  dplyr::summarise(
    n=n(),
    mean=mean(pca_score),
    sd=sd(pca_score),
    se=sd/sqrt(n),
    lower_ci = lower_ci(mean, se, n),
    upper_ci = upper_ci(mean, se, n)
  ) 

sum_weather_music_month$pca_factor = factor(sum_weather_music_month$pca_factor,
                                            levels = c("PCA1", "PCA2"),
                                            labels = c("Positive & Intense",
                                                        "Negative & Calm"))

plot_months = plot_PCA_month(sum_weather_music_month, 10)

figure2 = plot_grid(plot_out1, 
                    plot_out2,
                    plot_months, ncol = 1, 
          rel_heights = c(1.2,1.2,1))

# save high resolution
ggsave("figure2.R.pdf", figure2, 
       width = 20, height = 25, units = "cm",
       dpi = 300) 


# controlling for month
## 1. removing the effect of month
data_long2 = weather_music_1953_2019 %>%
  group_by(month) %>%
  mutate_at(vars(maxtemp:daysrain, PCA1:PCA2), scale) %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
data_long2$measure = as.factor(data_long2$measure)
# data_long2$measure <- factor(data_long2$measure, levels = c("temperature", "sun", "rain")) # reorder 

out1b = gam(PCA1 ~ measure + s(value, by = measure), data = data_long2, method = "REML") 
s1b = summary(out1b)

plot_out1b = plot_gam_facet(out1b, 
                           paste0("GAM PCA1 (control) - R2 = ", round(s1b$r.sq*100, 2), "%"),
                           "Weather measure (z-score)", # x label
                           "Negative & Calm Music", #  y label
                           weather_cols,
                           10, # size font ,
                           c(-3,2) # y lims
)

## 2. GAMS with season
data_seasons <- mutate(weather_music_1953_2019, 
                      season = ifelse(month %in% c("January", "February", "December"), "Winter",
                                      ifelse(month %in% c("March", "April", "May"), "Spring",
                                             ifelse(month %in% c("June", "July", "August"), "Summer","Autumn"))))
data_seasons$season <- factor(data_seasons$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

out_maxtemp = gam(PCA1 ~ season + s(maxtemp, by = season), data = data_seasons, method = "REML") 
out_sunshine = gam(PCA1 ~ season + s(sunshine, by = season), data = data_seasons, method = "REML") 
out_daysrain = gam(PCA1 ~ season + s(daysrain, by = season), data = data_seasons, method = "REML") 

summary(out_maxtemp)
plot(out_maxtemp)
summary(out_sunshine)
plot(out_sunshine)
summary(out_daysrain)
plot(out_daysrain)


#################################
# Figure 3: Top10 vs Bottom10
#################################
spotify_top_bottom_aggregate <- read_csv("~/Documents/r-projects/Lyrics_Weather/music-and-weather/data/spotify_top_bottom_aggregate.csv")
spotify_top10_bottom10 <- read_csv("~/Documents/r-projects/Lyrics_Weather/music-and-weather/data/spotify_top10_bottom10_aggregate.csv")

# prepare data
weather_data <- read_csv("../data/weather_data.csv") %>% 
  select(-rainfall, -airfrost)

clean_top_bottom <- left_join(spotify_top10_bottom10, weather_data , by = c('year', 'month')) %>%
  group_by(year) %>%
  filter(year %in% 1994:2019) %>%
  mutate_at(vars(danceability:daysrain), scale)

clean_top_bottom %>% 
  group_by(top_bottom) %>%
  dplyr::summarise(
    n=n(), 
    mean_dance = mean(danceability),sd_d = sd(danceability),
    mean_energy = mean(energy),sd_e = sd(energy)
  )

# PCA
data_to_pca2 = clean_top_bottom %>%
  select(danceability:duration_ms) %>%
  select(-duration_ms, -mode, -key, -liveness, -instrumentalness) 

res.pca2 <- prcomp(data_to_pca2[,-1], center = TRUE,scale. = TRUE) # exclude strictly music features: mode, tmepo, duration
summary(res.pca2)
plot_PCA_topbot = fviz_pca_var(res.pca2,
                               col.var = "contrib", # Color by contributions to the PC
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE     # Avoid text overlapping
)


# controls
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2
data_matrix2 = cor(data_to_pca2[,-1], use = 'complete.obs')
round(data_matrix2, 2)
cortest.bartlett(data_to_pca2[,-1])
KMO(data_matrix2)


ggsave("plot_PCA_top-bottom.pdf", plot_PCA_topbot, 
       width = 15, height = 15, units = "cm",
       dpi=300)

clean_top_bottom$PCA1 = res.pca2$x[,1] # add PCA1: energetic & positive
clean_top_bottom$PCA2 = res.pca2$x[,2] # add PCA2: interpretation unclear

clean_top_bottom %>% 
  group_by(top_bottom) %>%
  dplyr::summarise(
    n=n(), 
    mean_pca1 = mean(PCA1),sd_pca1 = sd(PCA1), min_pca1 = min(PCA1),max_pca1 = max(PCA1),
    mean_pca2 = mean(PCA2),sd_pca2 = sd(PCA2),min_pca2 = min(PCA2),max_pca2 = max(PCA2),
  )


# Figure 3A-B
clean_top100_long = clean_top_bottom %>%
  filter(top_bottom != "middle") %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
clean_top100_long$measure = as.factor(clean_top100_long$measure)

clean_top100_long$measure <- factor(clean_top100_long$measure, levels = c("maxtemp", 
                                                                          "sunshine", 
                                                                          "daysrain")) # reorder

clean_top100_long_top = 
  clean_top100_long %>% filter(top_bottom == "top10")
clean_top100_long_bottom = 
  clean_top100_long %>% filter(top_bottom == "bottom10")

out.top = gam(PCA1 ~ measure + s(value, by = measure), data = clean_top100_long_top)
s1b = summary(out.top)

out.bot = gam(PCA1 ~ measure + s(value, by = measure), data = clean_top100_long_bottom)
s2b = summary(out.bot)

plot_out1b = plot_gam_facet(out.top, 
                            paste0("TOP 20 - R2 = ", round(s1b$r.sq*100, 2), "%"),
                            "Weather measure (z-score)", # x label
                            "Positive & Intense Music", #  y label
                            weather_cols,
                            10, # size font
                            c(-4.5,4)
)
plot_out2b = plot_gam_facet(out.bot, 
                            paste0("BOTTOM 20 - R2 = ", round(s2b$r.sq*100, 2), "%"),
                            "Weather measure (z-score)", # x label
                            "Positive & Intense Music", #  y label
                            weather_cols,
                            10, # size font 
                            c(-4.5,4)
)

topvsbot = plot_grid(plot_out1b, plot_out2b, ncol = 1)


# Figure 3C
clean_top_bottom$month <- factor(clean_top_bottom$month, 
                                 levels = c("January", "February", "March","April", "May",
                                            "June", "July","August", "September", "October", 
                                            "November","December")) # sort order months

clean_top_bottom$top_bottom <- factor(clean_top_bottom$top_bottom, 
                                      levels = c("top10", "bottom10"))

plot_month_pcas= make_plot_top10bottom10_months(clean_top_bottom)

# Making Figure 3
figure3 = plot_grid(topvsbot, plot_month_pcas, ncol = 1, rel_heights = c(2,1), scale = c(1,0.95))

# save high resolution
figure3 = plot_grid(plot_out1b, 
                    plot_out2b,
                    plot_month_pcas, 
                    ncol = 1, 
                    rel_heights = c(1.2,1.2,1))

ggsave("figure3.R.pdf", figure3, 
       width = 20, height = 25, units = "cm",
       dpi = 300) 

