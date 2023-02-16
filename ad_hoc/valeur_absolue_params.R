library(targets)
library(tidyverse)
library(tidymodels)
library(broom)
theme_set(theme_bw())

fit_global_maha_class_dist <- tar_read(fit_global_maha_class_dist)

plot_enet_coefs <- function(fitted_wf, title = NULL, subtitle = NULL, caption = NULL) {
  fitted_wf %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% 
    filter(term != "(Intercept)", estimate != 0) %>% 
    mutate(Sign = if_else(estimate > 0, "+", "-")) %>% 
    mutate(abs_estimate = abs(estimate)) %>% 
    mutate(term = fct_reorder(term, abs_estimate)) %>% 
    ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
    geom_col(alpha = 1, col = "white") +
    xlab(NULL) +
    ylab("Absolute value of coefficient") +
    scale_fill_manual(values = c("#00adee", "#0855a0")) +
    coord_flip() +
    labs(subtitle = subtitle, caption = caption) +
    ggtitle(title)
}

plot_enet_coefs(fit_global_maha_class_dist)
