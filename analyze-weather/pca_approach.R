library(factoextra)

data_to_pca = weather_music_1953_2019 %>%
  select(danceability:duration_ms) %>%
  select(-duration_ms, -mode, -key)

res.pca <- prcomp(data_to_pca, center = TRUE,scale. = TRUE) # exclude strictly music features: mode, tmepo, duration
summary(res.pca)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

weather_music_1953_2019$PCA1 = res.pca$x[,1] # musical intensity + positive
weather_music_1953_2019$PCA2 = res.pca$x[,2] # sad

data_long = weather_music_1953_2019 %>%
  pivot_longer(cols = maxtemp:daysrain, names_to = "measure", values_to = "value")
data_long$measure = as.factor(data_long$measure)

out1 = gam(PCA1 ~ measure + s(value, by = measure), data = data_long, method = "REML") 
s1 = summary(out1)

out2 = gam(PCA2 ~ measure + s(value, by = measure), data = data_long, method = "REML") 
s2 = summary(out2)


plot_out1.11 = plot_gam_facet(out1, 
               paste0("GAM PCA1 (11 features) - R2 = ", round(s1$r.sq*100, 2), "%"),
               paste0("p.values: temp = ", round(s1$s.table[14],2),
                      "; sun = ", round(s1$s.table[15],2),
                      "; airfrost = ", round(s1$s.table[16],2), 
                      "; rain = ", round(s1$s.table[16],2)), 
               "Weather measure (z-score)", # x label
               "PCA1: Energetic & Positive", #  y label
               weather_cols
)
plot_out1.8 = plot_gam_facet(out1, 
                              paste0("GAM PCA1 (8 features) - R2 = ", round(s1$r.sq*100, 2), "%"),
                              paste0("p.values: temp = ", round(s1$s.table[14],2),
                                     "; sun = ", round(s1$s.table[15],2),
                                     "; airfrost = ", round(s1$s.table[16],2), 
                                     "; rain = ", round(s1$s.table[16],2)), 
                              "Weather measure (z-score)", # x label
                              "PCA1: Energetic & Positive", #  y label
                              weather_cols
)

pcas_gam_plots = plot_grid(plot_out1.11, plot_out1.8, ncol = 1)

clean_top100$month <- factor(clean_top100$month, 
                     levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     labels = c("January", "February", "March","April", "May",
                                "June", "July","August", "September", "October", 
                                "November","December")) # sort months


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
  ) %>%
  ggplot(aes(x= month, y = mean, group = pca_factor, fill = pca_factor)) + 
  geom_line()+
  geom_point() +
  ylab("PCA Score") +
  geom_hline(aes(yintercept = 0), linetype="dashed", color = "darkred") +
  geom_ribbon(aes(ymin=mean - se, 
                  ymax=mean + se),  alpha = 0.5)  +
  # scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme_classic() 

# plots = plot_grid(plot1, plot2, rel_heights = c(1.5,1),
#                   ncol = 1, labels = c("A", "B"))
# 
# ggsave("plotsB.png", dpi = 300)


################################################################################
################################################################################

# correcting for month
## 1. GAMs
data_long2 = weather_music_1953_2019 %>%
  group_by(month) %>%
  mutate_at(vars(maxtemp:daysrain, PCA1:PCA2), scale) %>%
  pivot_longer(cols = maxtemp:airfrost, names_to = "measure", values_to = "value")
data_long2$measure = as.factor(data_long2$measure)
# data_long2$measure <- factor(data_long2$measure, levels = c("temperature", "sun", "rain")) # reorder 

out1b = gam(PCA1 ~ measure + s(value, by = measure), data = data_long2, method = "REML") 
s1b = summary(out1b)

out2b = gam(PCA2 ~ measure + s(value, by = measure), data = data_long2, method = "REML") 
s2b = summary(out2b)


plot_out1b = plot_gam_facet(out1b, 
                           paste0("GAM PCA1 - R2 = ", round(s1b$r.sq*100, 2), "%"),
                           paste0("p.values: temp = ", round(s1b$s.table[10],2),
                                  "; sun = ", round(s1b$s.table[11],2),
                                  "; rain = ", round(s1b$s.table[12],2)), 
                           "Weather measure (z-score)", # x label
                           "PCA1: Energetic & Positive", #  y label
                           weather_cols
)
plot_out2b = plot_gam_facet(out2b, 
                           paste0("GAM PCA2 - R2 = ", round(s2b$r.sq*100, 2), "%"),
                           paste0("p.values: temp = ", round(s2b$s.table[10],2),
                                  "; sun = ", round(s2b$s.table[11],2),
                                  "; rain = ", round(s2b$s.table[12],2)), 
                           "Weather measure (z-score)", # x label
                           "PCA2: Relaxed & Negative Valence", #  y label
                           weather_cols
)


pcas_gam_plotsb = plot_grid(plot_out1b, plot_out2b, ncol = 1)


## 2. correlations
weather_var <- c('temperature', 'sun', 'rain')

months = c("January", "February", "March","April", "May",
           "June", "July","August", "September", "October", 
           "November","December")

library(boot)
storage = list()
for (i in months) {
  print(i)
  month_data = clean_top100 %>%
    dplyr::filter(month == i) 
  
  
  b3 <- boot(month_data, 
             statistic = function(month_data, i) {
               cor(month_data[i, "PCA1"], month_data[i, "temperature"], method='spearman')
             },
             R = 1000
  )
  b3.ci = boot.ci(b3, type = c("norm", "basic", "perc", "bca")) #bootstrapped CI. 
  
  
  storage[[i]] <- tibble(
    month = i %>% as.character(),
    correlation = cor(month_data$PCA1, month_data$temperature, method = "spearman") %>% as.numeric(),
    pvalue = cor.test(month_data$PCA1,month_data$temperature, method = "spearman")$p.value %>% as.numeric(),
    bt_cor = b3$t0[1],
    bt_low95 =  b3.ci$normal[2],
    bt_up95 =  b3.ci$normal[3],
  )
}

storage0 <- do.call(rbind, storage)

