library(targets)
library(tarchetypes)
library(tidyverse)
library(glue)
library(embed)
library(tidymodels)
library(FactoMineR)

theme_set(theme_bw())

test_preds <- tar_read(test_preds)
class_dist_test_ml <- tar_read(class_dist_test_ml)

preds_df <- 
  tibble(
    pred_class_dist = test_preds$class_dist$.pred_1,
    pred_class_dist_local_maha = test_preds$local_maha_class_dist$.pred_1,
    pred_class_dist_global_maha = test_preds$global_maha_class_dist$.pred_1,
    pred_class_dist_local_lof = test_preds$local_lof_class_dist$.pred_1,
    pred_class_dist_global_lof = test_preds$global_lof_class_dist$.pred_1,
    pred_class_dist_local_if = test_preds$local_if_class_dist$.pred_1,
    pred_class_dist_global_if = test_preds$global_if_class_dist$.pred_1,
    pred_class_dist_globa_maha_local_lof = test_preds$global_maha_local_lof_class_dist$.pred_1,
    claim_ind = class_dist_test_ml$claim_ind_cov_1_2_3_4_5_6
  )

preds_hard_df <- 
  preds_df %>% 
  mutate_at(vars(pred_class_dist:pred_class_dist_globa_maha_local_lof), ~  factor(as.numeric(. >= 0.5)))
  
conf_mat(preds_hard_df, claim_ind, pred_class_dist)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_local_maha)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_global_maha)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_local_lof)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_global_lof)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_local_if)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_global_if)
conf_mat(preds_hard_df, claim_ind, pred_class_dist_globa_maha_local_lof)


preds_hard_df %>% 
  bind_cols(class_dist_test_ml) %>% 
  mutate(distance = cut(distance, breaks = seq(0, 30000, by = 5000))) %>% 
  group_by(distance) %>% 
  summarise(
    acc_class_dist = accuracy_vec(claim_ind, pred_class_dist), 
    acc_class_dist_global_maha = accuracy_vec(claim_ind, pred_class_dist_global_maha), 
    n = n()
  ) %>%
  pivot_longer(acc_class_dist:acc_class_dist_global_maha) %>% 
  ggplot(aes(x = distance, y = value, col = name)) +
  geom_point()



preds_hard_df %>% 
  bind_cols(class_dist_test_ml) %>% 
  group_by(marital_status) %>% 
  summarise(
    acc_class_dist = accuracy_vec(claim_ind, pred_class_dist), 
    acc_class_dist_global_maha = accuracy_vec(claim_ind, pred_class_dist_global_maha), 
    n = n()
  ) %>%
  pivot_longer(acc_class_dist:acc_class_dist_global_maha) %>% 
  ggplot(aes(x = marital_status, y = value, col = name)) +
  geom_point() +
  geom_label(aes(label = n), nudge_x = 0.2)


preds_hard_df %>% 
  bind_cols(class_dist_test_ml) %>% 
  group_by(annual_distance) %>% 
  summarise(
    acc_class_dist = accuracy_vec(claim_ind, pred_class_dist), 
    acc_class_dist_global_maha = accuracy_vec(claim_ind, pred_class_dist_global_maha), 
    n = n()
  ) %>%
  pivot_longer(acc_class_dist:acc_class_dist_global_maha) %>% 
  ggplot(aes(x = annual_distance, y = value, col = name)) +
  geom_point()







