library(targets)
library(tidyverse)
library(tidymodels)

tune_en_class_dist <- tar_read(tune_en_class_dist)

tune_en_local_maha <- tar_read(tune_en_local_maha)
tune_en_local_lof <- tar_read(tune_en_local_lof)
tune_en_local_if <- tar_read(tune_en_local_if)

tune_en_global_maha <- tar_read(tune_en_global_maha)
tune_en_global_lof <- tar_read(tune_en_global_lof)
tune_en_global_if <- tar_read(tune_en_global_if)


tune_list <- 
  list(
    tune_en_class_dist = tune_en_class_dist,
    tune_en_local_maha = tune_en_local_maha,
    tune_en_local_lof = tune_en_local_lof,
    tune_en_local_if = tune_en_local_if,
    tune_en_global_maha = tune_en_global_maha,
    tune_en_global_lof = tune_en_global_lof,
    tune_en_global_if = tune_en_global_if
  )

pull_best_auc <- function(tuning) {
  collect_metrics(tuning) %>% 
    filter(
      penalty == select_best(tuning, metric = "roc_auc")$penalty,
      mixture == select_best(tuning, metric = "roc_auc")$mixture,
      .metric == "roc_auc"
    ) %>% 
    pull(mean)
}

pull_best_auc_sd <- function(tuning) {
  collect_metrics(tuning) %>% 
    filter(
      penalty == select_best(tuning, metric = "roc_auc")$penalty,
      mixture == select_best(tuning, metric = "roc_auc")$mixture,
      .metric == "roc_auc"
    ) %>% 
    pull(std_err)
}

pen <- map_dbl(tune_list, ~ select_best(., metric = "roc_auc")$penalty)
mix <- map_dbl(tune_list, ~ select_best(., metric = "roc_auc")$mixture)
auc <- map_dbl(tune_list, pull_best_auc)
auc_sd <- map_dbl(tune_list, pull_best_auc_sd)

tibble(
  name = names(tune_list),
  penalty = pen,
  mixture = mix,
  auc = auc,
  auc_sd = auc_sd
)
