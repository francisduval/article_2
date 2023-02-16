library(targets)
library(tidyverse)

theme_set(theme_bw())

local_maha_tune <- tar_read(local_maha_tune)
global_maha_tune <- tar_read(global_maha_tune)
local_lof_tune <- tar_read(local_lof_tune)
global_lof_tune <- tar_read(global_lof_tune)
local_if_tune <- tar_read(local_if_tune)
global_if_tune <- tar_read(global_if_tune)

local_lof_grid <- tar_read(local_lof_grid)
global_lof_grid <- tar_read(global_lof_grid)
local_if_grid <- tar_read(local_if_grid)
global_if_grid <- tar_read(global_if_grid)

plot_tuning <- function(tuning_ls, param_vec) {
  tuning_ls %>% 
    reduce(bind_rows) %>% 
    mutate(param = param_vec) %>% 
    ggplot(aes(x = param, y = mean)) +
    geom_point() +
    geom_line(linetype = "dashed", alpha = 0.8) +
    geom_pointrange(aes(ymin = mean - std_err, ymax = mean + std_err)) +
    ylab("5-fold cross-validation AUC") +
    labs(subtitle = NULL)
}


plot_tuning(local_lof_tune, param_vec = local_lof_grid) +
  xlab(expression(k[frac])) +
  theme(text = element_text(size = 18)) 

plot_tuning(local_if_tune, param_vec = local_if_grid) +
  xlab(expression(b[frac])) +
  scale_x_continuous(breaks = seq(0.2, 1, by = 0.2)) +
  theme(text = element_text(size = 18))

# ===============================================================================================================================

plot_tuning(global_lof_tune, param_vec = global_lof_grid) +
  xlab(expression(k)) +
  theme(text = element_text(size = 18)) 

plot_tuning(global_if_tune, param_vec = global_if_grid) +
  xlab(expression(b)) +
  theme(text = element_text(size = 18))
