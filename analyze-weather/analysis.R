################################################################################
# RSOS : Music features of popular songs reflect prevailing weather conditions
# Author: Manuel Anglada-Tort
# Script: Analysis reported in paper
################################################################################


# Structure: (1) global + (2) PCA + (3) Figure 1 + (4) Figure 2 + (5) Figure 3


################################################################################
# 1. global
################################################################################
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
library(scales)
library(ggpubr)

source("functions.R") # supporting methods

set.seed(1234)

# import data
weather_music_DT <- read_csv("../data/weather-music-1953-2019.csv")
weather_music_DT$month <- factor(weather_music_DT$month, 
                     levels = c("January", "February", "March","April", "May",
                                "June", "July","August", "September", "October", 
                                "November","December")) # sort order months

################################################################################
# 2. PCA
################################################################################
data_to_pca = weather_music_DT %>%
  select(danceability:duration_ms) %>%
  # exclude low-level features: key, mode, duration
  select(-duration_ms, -mode, -key, -liveness, -instrumentalness) 

res.pca <- prcomp(data_to_pca, center = TRUE,scale. = TRUE) 
summary(res.pca)
fviz_eig(res.pca)
plot_PCA = fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


# save
# ggsave("plot_PCA_main.pdf", plot_PCA, 
#        width = 15, height = 15, units = "cm", dpi=300)


weather_music_DT$PCA1 = res.pca$x[,1] # add PCA1: high-arousal positive
weather_music_DT$PCA2 = res.pca$x[,2] # add PCA2: low-arousal negative


# controls (Appendix A)
eig.val <- get_eigenvalue(res.pca)
eig.val
data_matrix = cor(data_to_pca, use = 'complete.obs')
round(data_matrix, 2)
cortest.bartlett(data_to_pca)
KMO(data_matrix)


################################################################################
# 3. Figure 1: Weather
################################################################################

# colors
show_col(viridis_pal(option = "C")(3))
rain_color = "#0D0887FF"
sun_color = "#F0F921FF"
temp_color = "#CC4678FF"
weather_cols = c(temp_color, sun_color, rain_color)


weather_data = weather_music_DT %>% 
  select(year, month, raw.maxtemp:raw.daysrain) %>%
  rename(maxtemp=raw.maxtemp, 
         sunshine=raw.sunshine, 
         daysrain=raw.daysrain
         )


# weather data over time
temp_time = plot_weather_time(weather_data, temp_color, "Temperature","Temperature", 10) 
sun_time = plot_weather_time(weather_data, sun_color, "Sunshine", "Sunshine",10) 
rain_time = plot_weather_time(weather_data, rain_color,"Rain", "Days of Rain", 10) 

# weather data over months
temperature_hist = plot_hist_weather(weather_data, maxtemp, "Temperature", 10)
sun_hist = plot_hist_weather(weather_data, sunshine,"Sunshine", 10)
rain_hist = plot_hist_weather(weather_data, daysrain, "Rainfall", 10)


# Figure 1
figure1 = plot_grid(temp_time, temperature_hist,
          sun_time, sun_hist,
          rain_time, rain_hist,
          rel_widths = c(1.35,1),
          ncol = 2, align = "vh")


# save high resolution
# ggsave("figure1.R.pdf", figure1, 
#        width = 20, height = 30, units = "cm", dpi = 500) 


################################################################################
# 4. Figure 2: GAM + seasonal pattern
################################################################################

# scaling by year effectively removes the confound of year-level trends

## raw weather variables are correlated with year
cor.test(weather_music_DT$year, weather_music_DT$raw.maxtemp)
ggscatter(weather_music_DT, x = "year", y = "raw.maxtemp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

## normalized weather variables are not
cor.test(weather_music_DT$year, weather_music_DT$maxtemp)
ggscatter(weather_music_DT, x = "year", y = "maxtemp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


# long format
data_long = weather_music_DT %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
data_long$measure <- factor(data_long$measure, 
                            levels = c("maxtemp", "sunshine", "daysrain")) 


# GAM 1
out1 = gam(PCA1 ~ measure + s(value, by = measure), data = data_long, method = "REML") 
s1 = summary(out1)
s1 # see results. (Adj-R2 = 6.09 %)

# checks
par(mfrow = c(2, 2))
gam.check(out1) # full convergence after 6 iterations + and good k selection
concurvity(out1, full = TRUE) # no high values of concurvity


# GAM 2
out2 = gam(PCA2 ~ measure + s(value, by = measure), data = data_long, method = "REML") 
s2 = summary(out2)
s2 # see results. (Adj-R2 = 6.10 %)

# checks
par(mfrow = c(2, 2))
gam.check(out2) # full convergence after 9 iterations + and good k selection
concurvity(out2, full = TRUE) # no high values of concurvity


# plots
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
  weather_music_DT %>% 
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


# Figure 2
figure2 = plot_grid(
  plot_out1,  plot_out2,
  plot_months, ncol = 1, rel_heights = c(1.2,1.2,1))


# save high resolution
# ggsave("figure2.R.pdf", figure2, 
#        width = 20, height = 25, units = "cm",
#        dpi = 300) 


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# control analysis

## First, run separate GAMS for each weather var add season as a covartiate
data_seasons <- mutate(weather_music_DT, 
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


## Second (stricter analysis), remove the seasonal trends by normalizing by month
data_long2 = weather_music_DT %>%
  select(year, month, maxtemp:daysrain, PCA1:PCA2) %>% 
  group_by(month) %>%
  mutate_at(vars(maxtemp:daysrain, PCA1:PCA2), scale) %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")

data_long2$measure = as.factor(data_long2$measure)

out1a = gam(PCA1 ~ measure + s(value, by = measure), data = data_long2, method = "REML") 
s1a = summary(out1a)
s1a  # see results. (Adj-R2 = 0.5 %)


plot_gam_facet(out1a, 
              paste0("GAM PCA1 (control) - R2 = ", round(s1a$r.sq*100, 2), "%"),
              "Weather measure (z-score)", # x label
              "Negative & Calm Music", #  y label
              weather_cols,
              10, # size font ,
              c(-3,2) # y lims
)

# end control analysis
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##


# you can also replicate these results with linear models

## for example, PCA1 correlates positively with max temp
ggscatter(weather_music_DT, x = "PCA1", y = "maxtemp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

## but PCA2 does not
ggscatter(weather_music_DT, x = "PCA2", y = "maxtemp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

## and this cannot be explained by a confound of year
ggscatter(weather_music_DT, x = "PCA1", y = "year", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(weather_music_DT, x = "year", y = "maxtemp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


################################################################################
# 5. Figure 3: Top10 vs Bottom10
################################################################################

# prepare new data

## music features data, monthly aggregated by bottom vs middle vs top 10 songs
spotify_top10_bottom10 <- read_csv("../data/spotify_top10_bottom10_aggregate.csv")

## weather data
weather_data <- read_csv("../data/weather_data.csv") 

top10_DT <- spotify_top10_bottom10 %>% 
  # merge datasets
  left_join(weather_data , by = c('year', 'month')) %>%
  # only include years for which top100 songs data is available (>= 1994)
  filter(year %in% 1994:2019) %>%
  # normalize by year to remove temporal trends
  group_by(year) %>%
  mutate_at(vars(danceability:daysrain), scale)


# PCA on the new data
data_to_pca2 = top10_DT %>%
  select(danceability:duration_ms) %>%
  select(-duration_ms, -mode, -key, -liveness, -instrumentalness) 


res.pca2 <- prcomp(data_to_pca2[,-1], center = TRUE,scale. = TRUE) 
summary(res.pca2)
plot_PCA_topbot = fviz_pca_var(res.pca2,
                               col.var = "contrib", # Color by contributions to the PC
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE     # Avoid text overlapping
)


## controls (Appendix B)
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2
data_matrix2 = cor(data_to_pca2[,-1], use = 'complete.obs')
round(data_matrix2, 2)
cortest.bartlett(data_to_pca2[,-1])
KMO(data_matrix2)


# save
# ggsave("plot_PCA_top-bottom.pdf", plot_PCA_topbot, 
#        width = 15, height = 15, units = "cm",
#        dpi=300)


top10_DT$PCA1 = res.pca2$x[,1] # add PCA1: high-arousal positive
top10_DT$PCA2 = res.pca2$x[,2] # add PCA2: low-arousal negative


# Figure 3A-B

# prepare data
top10_DT_long = top10_DT %>%
  filter(top_bottom != "middle") %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
top10_DT_long$measure = as.factor(top10_DT_long$measure)

top10_DT_long$measure <- factor(top10_DT_long$measure, 
                                levels = c("maxtemp", "sunshine", "daysrain")) 

top10_DT_long_top = 
  top10_DT_long %>% filter(top_bottom == "top10")
top10_DT_long_bottom = 
  top10_DT_long %>% filter(top_bottom == "bottom10")

# GAMS
out.top = gam(PCA1 ~ measure + s(value, by = measure), data = top10_DT_long_top)
s.out.top = summary(out.top)
s.out.top  # see results. (Adj-R2 = 12.2 %)


out.bot = gam(PCA1 ~ measure + s(value, by = measure), data = top10_DT_long_bottom)
s.out.bot = summary(out.bot)
s.out.bot  # see results. (Adj-R2 = 0.24 %)


# plots
plot_out1b = plot_gam_facet(out.top, 
                            paste0("TOP 20 - R2 = ", round(s.out.top$r.sq*100, 2), "%"),
                            "Weather measure (z-score)", # x label
                            "Positive & Intense Music", #  y label
                            weather_cols,
                            10, # size font
                            c(-4.5,4)
)
plot_out2b = plot_gam_facet(out.bot, 
                            paste0("BOTTOM 20 - R2 = ", round(s.out.bot$r.sq*100, 2), "%"),
                            "Weather measure (z-score)", # x label
                            "Positive & Intense Music", #  y label
                            weather_cols,
                            10, # size font 
                            c(-4.5,4)
)

topvsbot = plot_grid(plot_out1b, plot_out2b, ncol = 1)


# Figure 3C
top10_DT$month <- factor(top10_DT$month, 
                                 levels = c("January", "February", "March",
                                            "April", "May", "June", "July",
                                            "August", "September", "October", 
                                            "November","December"))

top10_DT$top_bottom <- factor(top10_DT$top_bottom, 
                              levels = c("top10", "bottom10"))

plot_month_pcas= make_plot_top10bottom10_months(top10_DT)


# Figure 3
figure3 = plot_grid(plot_out1b, plot_out2b, plot_month_pcas, 
                    ncol = 1, rel_heights = c(1.2,1.2,1))


# save high resolution
# ggsave("figure3.R.pdf", figure3, 
#        width = 20, height = 25, units = "cm",
#        dpi = 300) 

