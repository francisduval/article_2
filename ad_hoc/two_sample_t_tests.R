library(targets)
library(tidyverse)
library(kableExtra)
options(scipen = 999)

# ===============================================================================================================================

global_maha_train <- tar_read(global_maha_train)
local_maha_train <- tar_read(local_maha_train)

global_lof_train <- tar_read(global_lof_train)[[10]]
local_lof_train <- tar_read(local_lof_train)[[7]]

global_if_train <- tar_read(global_if_train)[[4]]
local_if_train <- tar_read(local_if_train)[[17]]

aug_trip_sample_train <- tar_read(aug_trip_sample_train)
ml_data_train <- tar_read(ml_data_train)

# ===============================================================================================================================

compute_stats <- function(data, vars) {
  p <- seq(0, 1, by = 0.1)
  p_names <- map_chr(p, ~ paste0("q_", .x * 100))
  p_funs <- map(p, ~ partial(quantile, probs = ., na.rm = T)) %>% set_names(nm = p_names)
  
  data %>% 
    group_by(vin) %>% 
    summarize_at(all_of(vars), c(mean = mean, sd = sd, p_funs)) %>% 
    ungroup()
}

# ===============================================================================================================================

compute_t_tests <- function(data1, data2) {
  t_tests <- map2(data1, data2, t.test)
  
  means_0 <- t_tests %>% map("estimate") %>% map_dbl(1)
  means_1 <- t_tests %>% map("estimate") %>% map_dbl(2)
  p_values <- t_tests %>% map_dbl("p.value")
  
  tib <- 
    tibble(
      name = names(means_0),
      mean_0 = means_0,
      mean_1 = means_1,
      p_value = p_values
    )
  
  return(tib)
}

# ===============================================================================================================================

global_maha_stats <- 
  aug_trip_sample_train %>%
  bind_cols(global_maha = global_maha_train) %>%
  compute_stats(vars = "global_maha") %>%
  bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)

global_maha_stats_0 <- filter(global_maha_stats, claim_ind_cov_1_2_3_4_5_6 == "0") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)
global_maha_stats_1 <- filter(global_maha_stats, claim_ind_cov_1_2_3_4_5_6 == "1") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)

# ----------

local_maha_stats <- 
  aug_trip_sample_train %>%
  bind_cols(local_maha = local_maha_train) %>%
  compute_stats(vars = "local_maha") %>%
  bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)

local_maha_stats_0 <- filter(local_maha_stats, claim_ind_cov_1_2_3_4_5_6 == "0") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)
local_maha_stats_1 <- filter(local_maha_stats, claim_ind_cov_1_2_3_4_5_6 == "1") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)

# ----------

global_lof_stats <- 
  aug_trip_sample_train %>%
  bind_cols(global_lof = global_lof_train) %>%
  compute_stats(vars = "global_lof") %>%
  bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)

global_lof_stats_0 <- filter(global_lof_stats, claim_ind_cov_1_2_3_4_5_6 == "0") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)
global_lof_stats_1 <- filter(global_lof_stats, claim_ind_cov_1_2_3_4_5_6 == "1") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)

# ----------

local_lof_stats <- 
  aug_trip_sample_train %>%
  bind_cols(local_lof = local_lof_train) %>%
  compute_stats(vars = "local_lof") %>%
  bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)

local_lof_stats_0 <- filter(local_lof_stats, claim_ind_cov_1_2_3_4_5_6 == "0") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)
local_lof_stats_1 <- filter(local_lof_stats, claim_ind_cov_1_2_3_4_5_6 == "1") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)

# ----------

global_if_stats <- 
  aug_trip_sample_train %>%
  bind_cols(global_if = global_if_train) %>%
  compute_stats(vars = "global_if") %>%
  bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)

global_if_stats_0 <- filter(global_if_stats, claim_ind_cov_1_2_3_4_5_6 == "0") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)
global_if_stats_1 <- filter(global_if_stats, claim_ind_cov_1_2_3_4_5_6 == "1") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)

# ----------

local_if_stats <- 
  aug_trip_sample_train %>%
  bind_cols(local_if = local_if_train) %>%
  compute_stats(vars = "local_if") %>%
  bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)

local_if_stats_0 <- filter(local_if_stats, claim_ind_cov_1_2_3_4_5_6 == "0") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)
local_if_stats_1 <- filter(local_if_stats, claim_ind_cov_1_2_3_4_5_6 == "1") %>% select(-vin, -claim_ind_cov_1_2_3_4_5_6)

# ===============================================================================================================================

global_maha <- compute_t_tests(global_maha_stats_0, global_maha_stats_1)
local_maha <- compute_t_tests(local_maha_stats_0, local_maha_stats_1)

global_lof <- compute_t_tests(global_lof_stats_0, global_lof_stats_1)
local_lof <- compute_t_tests(local_lof_stats_0, local_lof_stats_1)

global_if <- compute_t_tests(global_if_stats_0, global_if_stats_1)
local_if <- compute_t_tests(local_if_stats_0, local_if_stats_1)

# ===============================================================================================================================

global_maha %>% kable("latex", digits = 4)
local_maha %>% kable("latex", digits = 4)

global_lof %>% kable("latex", digits = 4)
local_lof %>% kable("latex", digits = 4)

global_if %>% kable("latex", digits = 4)
local_if %>% kable("latex", digits = 4)
