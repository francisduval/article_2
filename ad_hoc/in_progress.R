library(conflicted)
library(targets)
library(tarchetypes)
library(tidyverse)
library(tidymodels)
library(here)
library(rsample)
library(lubridate)
library(qs)
library(fastDummies)
library(here)
library(dtplyr)
library(hms)
library(fs)
library(embed)
library(glmnet)
library(glue)
library(dbscan)
library(e1071)
library(ranger)
library(solitude)
library(mixOmics)
map <- purrr::map
filter <- dplyr::filter
select <- dplyr::select
theme_set(theme_bw())

walk(dir_ls("R"), source)

local_maha_class_dist_train_ml <- tar_read(local_maha_class_dist_train_ml)
x <- slice_head(local_maha_class_dist_train_ml, n = 1000)

rec <- 
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = x) %>%
  update_role(vin, new_role = "id") %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_lencode_glm(all_nominal_predictors(), outcome = "claim_ind_cov_1_2_3_4_5_6") %>%
  step_impute_bag(commute_distance, years_claim_free) %>%
  step_YeoJohnson(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(q_0:q_100, threshold = 0.95) %>%
  step_normalize(all_predictors())

fit <- fit_en(x, recipe = rec, penalty = 0.01, mixture = 1)
test_en(fit, new_data = slice_tail(local_maha_class_dist_train_ml, n = 1000))



preds <- predict(fit, new_data = x, type = "prob")
preds_hard <- predict(fit, new_data = slice_tail(local_maha_class_dist_train_ml, n = 1000))

roc_auc_vec(x$claim_ind_cov_1_2_3_4_5_6, preds$.pred_0)
mn_log_loss_vec(x$claim_ind_cov_1_2_3_4_5_6, preds$.pred_0)
f_meas_vec(x$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class)
specificity_vec(x$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class)
sensitivity_vec(x$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class)
accuracy_vec(x$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class)
