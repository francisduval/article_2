library(targets)
library(tidyverse)
library(stringr)
library(jtools)
library(fs)
library(tidymodels)
library(gridExtra)

walk(dir_ls("R"), source)
theme_set(theme_bw())
options(scipen = 999)
set.seed(1004)

global_maha_class_dist_train_ml <- tar_read(global_maha_class_dist_train_ml)
global_lof_class_dist_train_ml <- tar_read(global_lof_class_dist_train_ml)
global_if_class_dist_train_ml <- tar_read(global_if_class_dist_train_ml)

lr_data_maha <- 
  global_maha_class_dist_train_ml %>% 
  select(claim_ind_cov_1_2_3_4_5_6, starts_with("q_"))

lr_data_lof <- 
  global_lof_class_dist_train_ml %>% 
  select(claim_ind_cov_1_2_3_4_5_6, starts_with("q_"))

lr_data_if <- 
  global_if_class_dist_train_ml %>% 
  select(claim_ind_cov_1_2_3_4_5_6, starts_with("q_"))


lr_fit_maha <- glm(claim_ind_cov_1_2_3_4_5_6 ~ ., family = binomial, data = lr_data_maha)
lr_fit_lof <- glm(claim_ind_cov_1_2_3_4_5_6 ~ ., family = binomial, data = lr_data_lof)
lr_fit_if <- glm(claim_ind_cov_1_2_3_4_5_6 ~ ., family = binomial, data = lr_data_if)


rec_1 <- 
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = lr_data_maha) %>% 
  step_normalize(all_predictors())

rec_2 <- 
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = lr_data_maha) %>% 
  step_interact(terms = ~ starts_with("q_"):starts_with("q_")) %>%
  step_normalize(all_predictors())

# ===============================================================================================================================

tune_en <- function(data, recipe) {
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  set.seed(1994)
  grid <- grid_regular(penalty(range = c(-10, 1)), mixture(), levels = c(60, 5))
  cv <- vfold_cv(data, v = 5, strata = claim_ind_cov_1_2_3_4_5_6)
  
  tune_spec <-
    logistic_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
    set_engine("glmnet")
  
  wf <- 
    workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(recipe)
  
  tuning <- 
    tune_grid(
      wf,
      resamples = cv,
      grid = grid,
      metrics = metric_set(roc_auc, accuracy, mn_log_loss, f_meas, sensitivity, specificity),
      control = control_grid(save_pred = F)
    )
  
  return(tuning)
}

# ===============================================================================================================================

maha_tuned <- tune_en(lr_data_maha, recipe = rec_1)
lof_tuned <- tune_en(lr_data_lof, recipe = rec_1)
if_tuned <- tune_en(lr_data_if, recipe = rec_1)

opt_param_maha <- select_best(maha_tuned, metric = "roc_auc")
opt_param_lof <- select_best(lof_tuned, metric = "roc_auc")
opt_param_if <- select_best(if_tuned, metric = "roc_auc")

fit_maha <- fit_en(lr_data_maha, recipe = rec_1, penalty = opt_param_maha$penalty, mixture = opt_param_maha$mixture) 
fit_lof <- fit_en(lr_data_lof, recipe = rec_1, penalty = opt_param_lof$penalty, mixture = opt_param_lof$mixture) 
fit_if <- fit_en(lr_data_if, recipe = rec_1, penalty = opt_param_if$penalty, mixture = opt_param_if$mixture) 

# ===============================================================================================================================

maha_tuned_int <- tune_en(lr_data_maha, recipe = rec_2)
lof_tuned_int <- tune_en(lr_data_lof, recipe = rec_2)
if_tuned_int <- tune_en(lr_data_if, recipe = rec_2)

opt_param_maha_int <- select_best(maha_tuned_int, metric = "roc_auc")
opt_param_lof_int <- select_best(lof_tuned_int, metric = "roc_auc")
opt_param_if_int <- select_best(if_tuned_int, metric = "roc_auc")

fit_maha_int <- fit_en(lr_data_maha, recipe = rec_2, penalty = opt_param_maha_int$penalty, mixture = opt_param_maha_int$mixture) 
fit_lof_int <- fit_en(lr_data_lof, recipe = rec_2, penalty = opt_param_lof_int$penalty, mixture = opt_param_lof_int$mixture) 
fit_if_int <- fit_en(lr_data_if, recipe = rec_2, penalty = opt_param_if_int$penalty, mixture = opt_param_if_int$mixture) 

# ===============================================================================================================================

plot_enet_coefs <- function(fitted_wf, title = NULL, subtitle = NULL, caption = NULL) {
  fitted_wf %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% 
    filter(term != "(Intercept)", estimate != 0) %>% 
    mutate(Sign = if_else(estimate > 0, "+", "-")) %>% 
    mutate(abs_estimate = abs(estimate)) %>% 
    mutate(term = fct_reorder(term, abs_estimate)) %>% 
    ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
    geom_col(alpha = 0.8, col = "black") +
    xlab(NULL) +
    ylab("Absolute value of coefficient") +
    scale_fill_manual(values = c("white", "black")) +
    coord_flip() +
    labs(subtitle = subtitle, caption = caption) +
    ggtitle(title)
}


plot_enet_coefs_2 <- function(fitted_wf, subtitle = NULL, caption = NULL) {
  fitted_wf %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% 
    filter(term != "(Intercept)") %>%
    filter(str_detect(term, "q_")) %>% 
    mutate(term = as.numeric(str_sub(term, start = 3L))) %>% 
    ggplot(aes(x = term, y = estimate)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    xlab("Percentile") +
    ylab("Coefficient estimate") +
    labs(subtitle = subtitle, caption = caption)
}

# ===============================================================================================================================

p2_1 <- plot_enet_coefs_2(fit_maha, subtitle = "Global mahalanobis scores")
p2_2 <- plot_enet_coefs_2(fit_lof, subtitle = "Global LOF scores")
p2_3 <- plot_enet_coefs_2(fit_if, subtitle = "Global isolation forest scores")

grid.arrange(p2_1, p2_2, p2_3, nrow = 1, top = "Elastic-net coefficients", bottom = "Fitted using global scores only")


p3_1 <- plot_enet_coefs(fit_maha, subtitle = "Global mahalanobis scores")
p3_2 <- plot_enet_coefs(fit_lof, subtitle = "Global LOF scores")
p3_3 <- plot_enet_coefs(fit_if, subtitle = "Global isolation forest scores")

grid.arrange(p3_1, p3_2, p3_3, nrow = 1, top = "Elastic-net coefficients", bottom = "Fitted using all covariates")

# ===============================================================================================================================

p2_1_int <- plot_enet_coefs(fit_maha_int, subtitle = "Global mahalanobis scores")
p2_2_int <- plot_enet_coefs(fit_lof_int, subtitle = "Global LOF scores")
p2_3_int <- plot_enet_coefs(fit_if_int, subtitle = "Global isolation forest scores")

grid.arrange(p2_1_int, p2_2_int, p2_3_int, nrow = 1, top = "Elastic-net coefficients", bottom = "Fitted using global scores only")

