library(tidyverse)
library(ggcorrplot)
################################################################################
# Top vs bottom analysis
################################################################################

spotify_top_bottom_aggregate <- read_csv("~/Documents/r-projects/Lyrics_Weather/music-and-weather/data/spotify_top_bottom_aggregate.csv")
spotify_top10_bottom10 <- read_csv("~/Documents/r-projects/Lyrics_Weather/music-and-weather/data/spotify_top10_bottom10_aggregate.csv")

weather_data <- read_csv("../data/weather_data.csv") %>% 
  select(-rainfall, -airfrost)

clean_top_bottom <- left_join(spotify_top10_bottom10, weather_data , by = c('year', 'month')) %>%
  filter(top_bottom != "middle") %>%
  group_by(year) %>%
  filter(year %in% 1994:2019) %>%
  mutate_at(vars(danceability:daysrain), scale)


data_to_pca = clean_top_bottom %>%
  select(danceability:duration_ms) %>%
  select(-duration_ms, -mode, -key) 

res.pca2 <- prcomp(data_to_pca[,-1], center = TRUE,scale. = TRUE) # exclude strictly music features: mode, tmepo, duration
summary(res.pca2)
plot_PCA_topbot = fviz_pca_var(res.pca2,
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE     # Avoid text overlapping
)

ggsave("plot_PCA_top-bottom.pdf", plot_PCA_topbot, 
       width = 15, height = 15, units = "cm",
       dpi=300)

clean_top_bottom$PCA1 = res.pca2$x[,1] # add PCA1: energetic & positive
clean_top_bottom$PCA2 = res.pca2$x[,2] # add PCA2: interpretaion unclear


clean_top_bottom$month <- factor(clean_top_bottom$month, 
                                        levels = c("January", "February", "March","April", "May",
                                                   "June", "July","August", "September", "October", 
                                                   "November","December")) # sort order months

clean_top_bottom$top_bottom <- factor(clean_top_bottom$top_bottom, 
                                 levels = c("top10", "middle", "bottom10"))
                             

plot_months_topbottom = clean_top_bottom %>%
  filter(top_bottom != "middle") %>%
  group_by(month, top_bottom) %>%
  # filter(hit_class == "middle") %>%
  dplyr::summarise(
    n=n(),
    mean=mean(PCA1),
    sd=sd(PCA1),
    se=sd/sqrt(n),
    lower_ci = lower_ci(mean, se, n),
    upper_ci = upper_ci(mean, se, n)
  ) %>%
  ggplot(aes(x= month, y = mean, group = top_bottom, fill = top_bottom)) + 
  geom_ribbon(aes(ymin=mean - se, 
                  ymax=mean + se),  alpha = 0.5)  +
  geom_line()+
  geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 1) +
  ggtitle("SPACE") +
  ylab("Musical Intensity (PCA)") +
  geom_hline(aes(yintercept = 0), linetype="dashed", color = "darkred") +
  # scale_fill_viridis(discrete = TRUE, option = "A", direction = -1) +
  scale_fill_manual(values=c("darkred", "grey"))  +
  theme_classic() +
  facet_wrap(~ top_bottom) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot0 = plot_months_topbottom +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), 
        axis.title=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=10), 
        legend.position = "none"
        # legend.position = c(0.9, 0.9)
  )



################################################################################
clean_top100_long = clean_top_bottom %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
clean_top100_long$measure = as.factor(clean_top100_long$measure)

clean_top100_long$measure <- factor(clean_top100_long$measure, levels = c("maxtemp", 
                                                          "sunshine", 
                                                          # "airfrost", 
                                                          "daysrain")) # reorder

clean_top100_long_top = 
  clean_top100_long %>% filter(top_bottom == "top10")
clean_top100_long_bottom = 
  clean_top100_long %>% filter(top_bottom == "bottom10")

out.top = gam(PCA2 ~ measure + s(value, by = measure), data = clean_top100_long_top)
s1b = summary(out.top)

out.bot = gam(PCA2 ~ measure + s(value, by = measure), data = clean_top100_long_bottom)
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

figure3 = plot_grid(topvsbot, plot0, ncol = 1, rel_heights = c(1.5,1), scale = c(1,0.95))

# save high resolution
ggsave("figure3.R.pdf", figure3, 
       width = 17, height = 20, units = "cm",
       dpi = 300) 
