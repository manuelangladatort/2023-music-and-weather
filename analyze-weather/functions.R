# supporting function analysis
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


plot_weather_time = function(data, color, filtered_metric, title, SIZE){
  data_to_plot0 <- weather_data %>%
    # filter(year %in% 1953:2019) %>%
    group_by(year) %>%
    dplyr::summarise(n = n(),
              m_temperature = mean(maxtemp),
              se_maxtemp = sd(maxtemp) / sqrt(n),
              m_sunshine = mean(sunshine),
              se_sun = sd(sunshine) / sqrt(n),
              m_daysrain = mean(daysrain),
              se_rain = sd(daysrain) / sqrt(n),
              m_airfrost = mean(airfrost),
              se_airfrost = sd(airfrost) / sqrt(n)) %>%
    pivot_longer(cols = m_temperature:se_airfrost, names_to = "metric", values_to = "value")
  
  data_to_plot_means = data_to_plot0 %>%
    filter(substr(metric,1,2) == "m_") %>%
    dplyr::rename(mean = value) %>%
    select(year, metric, mean)
 
  data_to_plot_means$metric <- factor(data_to_plot_means$metric, 
                                      levels = c("m_temperature", "m_sunshine", "m_daysrain", "m_airfrost"),
                                      labels = c("Temperature", "Sunshine", "Rain", "Airfrost"))
  
  plot <- data_to_plot_means %>%
    filter(metric == filtered_metric) %>%
    ggplot(aes(year, mean, color = metric)) +
    geom_line() +
    geom_point()+
    geom_smooth(fill=color, alpha = 0.25) +
    scale_color_manual(values = color) +
    scale_x_continuous(breaks=seq(1950,2020,10),
                       limits = c(1950,2020)) +
    ggtitle(title) +
    theme_bw()
  
  plot_publish = 
    plot +   theme(axis.text.x = element_text(size=SIZE),
                   axis.text.y = element_text(size=SIZE),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   # axis.ticks.x=element_blank(),
                   legend.position = "none",
                   legend.text = element_blank(),
                   legend.title = element_blank())
  return(plot_publish)
}

plot_hist_weather = function(data, x_var, title, SIZE){
  x_var_encoded = enquo(x_var)
  
  plot = ggplot(data, aes(x = !! x_var_encoded, y = as.factor(month), fill = ..x..)) +
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(option = "plasma") +
    ggtitle(title) +
    theme_bw() 
  
  plot_publish = 
    plot +   theme(axis.text.x = element_text(size=SIZE),
                   axis.text.y=element_text(size=SIZE),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   # axis.ticks.x=element_blank(),
                   legend.position = "none",
                   legend.text = element_blank(),
                   legend.title = element_blank(),
                   strip.background = element_blank(),
                   strip.text.x = element_blank())
  return(plot_publish)
}

plot_gam <- function(gam_output, x_var, title, x_lab, y_lab){
  x_var_encoded = enquo(x_var)
  
  plot_data_gam = plot_smooths(
    model = gam_output,
    series = !! x_var_encoded
  ) +
    ggtitle(title) +
    labs(x=x_lab, y = y_lab) +
    theme_bw() 
  
  plot <- plot_data_gam + 
    theme(axis.text = element_text(size=10), axis.title=element_text(size=10),
          plot.title = element_text(size=11))
  return(plot)
}

plot_gam_facet <- function(gam_output, title, x_lab, y_lab, colors, SIZE, x_lims){
  plot_data_gam = plot_smooths(
    model = gam_output,
    series = value,
    comparison = measure,
    facet_terms = measure,
  ) +
    ggtitle(title) +
    labs(x=x_lab, y = y_lab) +
    # facet_wrap(~measure, scales = "free") +
    ylim(x_lims) +
    geom_line(aes(colour = measure, linetype = "dashed")) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    geom_hline(aes(yintercept = 0), linetype="dashed", color = "darkred") +
    theme_bw() 
  
  plot <- plot_data_gam + 
    theme(axis.text = element_text(size=SIZE), 
          axis.title=element_blank(),
          plot.title = element_text(size=(SIZE+2)), 
          legend.position = "none",
          strip.background =element_rect(fill="gray92")
          )
  return(plot)
}
         

plot_PCA_month = function(data_sum, SIZE){
  plot = data_sum %>% 
    ggplot(aes(x= month, y = mean, group = pca_factor, fill = pca_factor)) + 
    ylab("PCA Score") +
    geom_ribbon(aes(ymin=lower_ci, 
                    ymax=upper_ci),  alpha = 0.5)  +
    geom_line()+
    geom_point(shape = 21, colour = "black", fill = "white", size = 1, stroke = 1) +
    scale_fill_manual(values=c("darkred", "grey"))  +
    ggtitle("SPACE") +
    geom_hline(aes(yintercept = 0), linetype="dashed", color = "darkred") +
    theme_classic()  +
    facet_wrap(~pca_factor)
  plot0 = plot +
    theme(
          axis.text.y = element_text(size=SIZE), 
          axis.title=element_blank(),
          plot.title = element_blank(), 
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=(SIZE))
          
    )
  return(plot0)
}

make_plot_top10bottom10_months = function(data){
  plot_months_topbottom = data %>%
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
    geom_ribbon(aes(ymin=lower_ci, 
                    ymax=upper_ci),  alpha = 0.5)  +
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
  return(plot0)
}
