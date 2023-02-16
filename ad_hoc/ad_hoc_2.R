library(targets)
library(tidyverse)
library(tidymodels)
library(here)
library(fs)
library(glue)
library(gridExtra)

walk(dir_ls("R"), source)
options(scipen = 999)
theme_set(theme_bw())

# ===============================================================================================================================

global_maha_train_ml <- tar_read(global_maha_train_ml_stats)
global_lof_train_ml <- tar_read(global_lof_train_ml_stats)[[4]]
global_if_train_ml <- tar_read(global_if_train_ml_stats)[[4]]

global_maha_class_dist_test_ml <- tar_read(global_maha_class_dist_test_ml_stats)
global_lof_class_dist_test_ml <- tar_read(global_lof_class_dist_test_ml_stats)
global_if_class_dist_test_ml <- tar_read(global_if_class_dist_test_ml_stats)

# ===============================================================================================================================

names <-
  c(
    "mean",
    "var",
    "min",
    "q05",
    "q25",
    "median",
    "q75",
    "q95",
    "max",
    "range",
    "IQR",
    "skewness",
    "kurtosis"
  )

data_maha_ls <- map(2:14, ~ global_maha_train_ml[ , c(., 15)]) %>% set_names(names)
data_lof_ls <- map(2:14, ~ global_lof_train_ml[ , c(., 15)]) %>% set_names(names)
data_if_ls <- map(2:14, ~ global_if_train_ml[ , c(., 15)]) %>% set_names(names)

model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

rec_maha_ls <- map(data_maha_ls, ~ recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = .x) %>% step_normalize(all_predictors()))
rec_lof_ls <- map(data_lof_ls, ~ recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = .x) %>% step_normalize(all_predictors()))
rec_if_ls <- map(data_if_ls, ~ recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = .x) %>% step_normalize(all_predictors()))

wf_maha_ls <- map(rec_maha_ls, ~ workflow() %>% add_model(model) %>% add_recipe(.x))
wf_lof_ls <- map(rec_lof_ls, ~ workflow() %>% add_model(model) %>% add_recipe(.x))
wf_if_ls <- map(rec_if_ls, ~ workflow() %>% add_model(model) %>% add_recipe(.x))

fit_maha_ls <- map2(data_maha_ls, wf_maha_ls, ~ fit(.y, data = .x))
fit_lof_ls <- map2(data_lof_ls, wf_lof_ls, ~ fit(.y, data = .x))
fit_if_ls <- map2(data_if_ls, wf_if_ls, ~ fit(.y, data = .x))

test_maha_ls <- map(fit_maha_ls, ~ test_en(., new_data = global_maha_class_dist_test_ml))
test_lof_ls <- map(fit_lof_ls, ~ test_en(., new_data = global_lof_class_dist_test_ml))
test_if_ls <- map(fit_if_ls, ~ test_en(., new_data = global_if_class_dist_test_ml))

auc_maha_vec <- map_dbl(test_maha_ls, "roc_auc")
auc_lof_vec <- map_dbl(test_lof_ls, "roc_auc")
auc_if_vec <- map_dbl(test_if_ls, "roc_auc")

auc_tib <- 
  tibble(
    stat = names,
    maha = auc_maha_vec,
    lof = auc_lof_vec,
    ifo = auc_if_vec
  )

auc_tib %>% 
  pivot_longer(-stat) %>% 
  ggplot(aes(x = stat, y = value - 0.5)) +
  geom_col() +
  facet_wrap(vars(name)) +
  ggtitle("AUC au-dessus de 0.5") +
  labs(subtitle = "Régressions logistiques à 1 variable") +
  xlab("Statistique") +
  ylab("AUC - 0.5") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ===============================================================================================================================

extract_coef <- function(fit) {
  fit %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% 
    slice(2) %>% 
    select(2) %>% 
    pull()
}

coefs_maha_vec <- map_dbl(fit_maha_ls, extract_coef)
coefs_lof_vec <- map_dbl(fit_lof_ls, extract_coef)
coefs_if_vec <- map_dbl(fit_if_ls, extract_coef)


coefs_tib <- 
  tibble(
    stat = names,
    maha = coefs_maha_vec,
    lof = coefs_lof_vec,
    ifo = coefs_if_vec
  )

coefs_tib %>% 
  pivot_longer(-stat) %>% 
  ggplot(aes(x = stat, y = value)) +
  geom_col() +
  facet_wrap(vars(name)) +
  ggtitle("Coefficients") +
  labs(subtitle = "Régressions logistiques à 1 variable") +
  xlab("Centile") +
  ylab("Valeur du coefficient") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ===============================================================================================================================

rec_maha <- 
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = global_maha_train_ml) %>% 
  step_rm(vin) %>% 
  step_normalize(all_predictors())

rec_lof <- 
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = global_lof_train_ml) %>% 
  step_rm(vin) %>% 
  step_normalize(all_predictors())

rec_if <- 
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = global_if_train_ml) %>% 
  step_rm(vin) %>% 
  step_normalize(all_predictors())

en_maha_tune <- tune_en(global_maha_train_ml, recipe = rec_maha)
en_lof_tune <- tune_en(global_lof_train_ml, recipe = rec_lof)
en_if_tune <- tune_en(global_if_train_ml, recipe = rec_if)


fit_maha <-  
  fit_en(
    data = global_maha_train_ml, 
    recipe = rec_maha,
    penalty = select_best(en_maha_tune, metric = "roc_auc")$penalty,
    mixture = select_best(en_maha_tune, metric = "roc_auc")$mixture
  )

fit_lof <-  
  fit_en(
    data = global_lof_train_ml, 
    recipe = rec_lof,
    penalty = select_best(en_lof_tune, metric = "roc_auc")$penalty,
    mixture = select_best(en_lof_tune, metric = "roc_auc")$mixture
  )

fit_if <-  
  fit_en(
    data = global_if_train_ml, 
    recipe = rec_if,
    penalty = select_best(en_if_tune, metric = "roc_auc")$penalty,
    mixture = select_best(en_if_tune, metric = "roc_auc")$mixture
  )


plot_enet_coefs <- function(fitted_wf, title = NULL, subtitle = NULL, caption = NULL) {
  fitted_wf %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% 
    filter(term != "(Intercept)") %>% 
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


p1 <- plot_enet_coefs(fit_maha, subtitle = "Global Mahalanobis")
p2 <- plot_enet_coefs(fit_lof, subtitle = "Global LOF")
p3 <- plot_enet_coefs(fit_if, subtitle = "Global Isolation Forest")

grid.arrange(p1, p2, p3, nrow = 1)

