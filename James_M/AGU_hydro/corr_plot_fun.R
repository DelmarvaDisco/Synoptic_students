corr_plot_fun <- function(data) {
  
  data <- {{data}} 
  
  models <- data %>% 
    group_by(Site_IDs) %>% 
    nest() %>% 
    mutate(gradient_models = map(.x = data, 
                                 ~lm(gradient ~ dly_mean_wtrlvl_allsites,
                                     data = .x) %>% 
                                   tidy())) %>% 
    unnest(gradient_models) %>% 
    select(-c(data)) %>% 
    as_tibble() %>% 
    filter(term == "dly_mean_wtrlvl_allsites")
  
  stats <- data %>% 
    group_by(Site_IDs) %>% 
    nest() %>% 
    mutate(gradient_stats = map(.x = data, 
                                ~lm(gradient ~ dly_mean_wtrlvl_allsites, 
                                    data = .x) %>% 
                                  glance())) %>%
    unnest(gradient_stats) %>% 
    select(-c(data)) %>% 
    as_tibble()
  
  data <- data %>% 
    mutate(month_yr = as.numeric(str_sub(Date, 6, 7)))
  
  corr_plot <- ggplot() +
    geom_point(data = data,
               mapping = aes(x = dly_mean_wtrlvl_allsites,
                             y = gradient,
                             color = month_yr)) +
    geom_text(data = stats,
              aes(label = paste0("r^2 = ", round(r.squared, digits = 2))),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2,
              inherit.aes = FALSE,
              color = "red",
              size = 8) +
    geom_text(data = models,
              aes(label = paste0("slope = ", round(estimate, digits = 5))),
              x = -Inf, y = Inf, hjust = -0.1, vjust = 2.5,
              inherit.aes = FALSE,
              color = "red",
              size = 8) +
    geom_hline(yintercept = 0, 
               size = 2, 
               color = "#993300") +
    theme_bw() +
    xlab("Daily mean water lvl (m)") +
    ylab("dh/dL") +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 18,
                                   face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.text = element_text(size = 14,
                                     face = "bold"),
          strip.text = element_text(size = 18,
                                    face = "bold"),
          title = element_text(size = 24)) +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    facet_wrap(vars(Site_IDs),
               scales = "free") +
    ggtitle("Aggregate water level and head gradient correlations")
  
  return(corr_plot)
  
}
