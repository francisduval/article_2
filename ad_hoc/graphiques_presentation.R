library(targets)
library(tarchetypes)
library(tidyverse)
library(tidymodels)
library(glue)
library(dbscan)
library(e1071)
library(fs)
library(xgboost)
library(embed)
library(tictoc)
library(ranger)
library(vip)
library(solitude)
theme_set(theme_bw())

walk(dir_ls("R"), source)

make_plot <- function(metric, boot_res_opt_df, boot_res_sum_opt_df) {
  lev <-
    c(
      "class",
      "class_dist",
      "class_dist_global_maha",
      "class_dist_local_maha",
      "class_dist_local_lof",
      "class_dist_local_10_lof",
      "class_dist_global_10_lof",
      "class_dist_global_20_lof",
      "class_dist_global_30_lof",
      "class_dist_global_40_lof"
    )
  
  df1 <- filter(boot_res_opt_df, .metric == metric)
  df2 <- filter(boot_res_sum_opt_df, .metric == metric)
  
  ggplot(df1, aes(x = covariates, y = .estimate, color = covariates, fill = covariates)) +
    ggdist::stat_halfeye(
      adjust = .33,
      width = .67, 
      color = NA,
      position = position_nudge(x = .1)
    ) +
    geom_point(aes(x = covariates, y = mean), data = df2) +
    geom_point(
      position = position_nudge(x = -.22),
      shape = 95, size = 4, alpha = 0.7
    ) +
    ylab(metric) +
    xlab(NULL) +
    guides(fill = "none", color = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


glmnet_tuning_ls <- tar_read(glmnet_tuning_ls)
names(glmnet_tuning_ls) <- names(tar_read(ml_df_ls))

collect_optimal_metrics <- function(tuning, metric, summarize = T) {
  best_params <- select_best(tuning, metric = metric)
  tuning %>% 
    filter_parameters(parameters = best_params) %>% 
    collect_metrics(summarize = summarize)
}

make_tuning_res_df <- function(tuning_ls, metric, summarize = T) {
  map(tuning_ls, collect_optimal_metrics, metric = metric, summarize = summarize) %>% 
    imap_dfr(~ mutate(.x, covariates = .y))
}

glmnet_res_sum <- make_tuning_res_df(glmnet_tuning_ls, metric = "roc_auc", summarize = T)


covs_local <- c("class", "class_dist", "class_dist_local_maha", "class_dist_local_lof", "class_dist_local_if")
covs_global <- c("class", "class_dist", "class_dist_global_maha", "class_dist_global_30_lof", "class_dist_global_if")

glmnet_res_sum %>% 
  filter(covariates %in% covs_local) %>% 
  mutate(covariates = factor(covariates, levels = covs_local)) %>% 
  mutate(
    covariates = 
      fct_recode(
        covariates,
        "Variables classiques" = "class",
        "Variables classiques +\n distance" = "class_dist",
        "Variables classiques +\n distance +\n Mahalanobis" = "class_dist_local_maha",
        "Variables classiques +\n distance +\n Local outlier factor" = "class_dist_local_lof",
        "Variables classiques +\n distance +\n Isolation forest" = "class_dist_local_if"
      )
  ) %>% 
  group_split(.metric) %>% 
  map(~ mutate(., amelioration = mean - first(mean))) %>% 
  reduce(bind_rows) %>% 
  filter(!(covariates %in% "Variables classiques")) %>% 
  mutate(
    .metric = 
      fct_recode(
        .metric, 
        "Accuracy" = "accuracy", 
        "AUC" = "roc_auc", 
        "Sensibilité" = "sens", 
        "Spécificité" = "spec" 
      )
  ) %>% 
  ggplot(aes(y = amelioration, x = covariates, fill = .metric)) +
  geom_col(position = "dodge", width = 0.6) +
  xlab(NULL) +
  ylab(NULL) +
  labs(fill = NULL, subtitle = "Amélioration par rapport au modèle classique - Scores locaux")

glmnet_res_sum %>% 
  filter(covariates %in% covs_global) %>% 
  mutate(covariates = factor(covariates, levels = covs_global)) %>% 
  mutate(
    covariates = 
      fct_recode(
        covariates,
        "Variables classiques" = "class",
        "Variables classiques +\n distance" = "class_dist",
        "Variables classiques +\n distance +\n Mahalanobis" = "class_dist_global_maha",
        "Variables classiques +\n distance +\n Local outlier factor" = "class_dist_global_30_lof",
        "Variables classiques +\n distance +\n Isolation forest" = "class_dist_global_if",
      )
  ) %>% 
  group_split(.metric) %>% 
  map(~ mutate(., amelioration = mean - first(mean))) %>% 
  reduce(bind_rows) %>% 
  filter(!(covariates %in% "Variables classiques")) %>% 
  mutate(
    .metric = 
      fct_recode(
        .metric, 
        "Accuracy" = "accuracy", 
        "AUC" = "roc_auc", 
        "Sensibilité" = "sens", 
        "Spécificité" = "spec" 
      )
  ) %>% 
  ggplot(aes(y = amelioration, x = covariates, fill = .metric)) +
  geom_col(position = "dodge", width = 0.6) +
  xlab(NULL) +
  ylab(NULL) +
  labs(fill = NULL, subtitle = "Amélioration par rapport au modèle classique - Scores globaux")
