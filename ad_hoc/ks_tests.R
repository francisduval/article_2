library(targets)
library(tidyverse)
library(glue)

theme_set(theme_bw())

global_maha_train <- tar_read(global_maha_train)
global_lof_train <- tar_read(global_lof_train)[[4]]
global_if_train <- tar_read(global_if_train)[[4]]

aug_trip_sample_train <- tar_read(aug_trip_sample_train)

x <- 
  aug_trip_sample_train %>% 
  bind_cols(
    global_maha = global_maha_train,
    global_lof = global_lof_train,
    global_if = global_if_train
  )


t.test(rnorm(10000), rnorm(10000))
t.test(rnorm(10000), rnorm(10000, mean = 10))

t.test(x$global_maha[x$claim_ind_cov_1_2_3_4_5_6 == "0"], x$global_maha[x$claim_ind_cov_1_2_3_4_5_6 == "1"])
t.test(x$global_lof[x$claim_ind_cov_1_2_3_4_5_6 == "0"], x$global_lof[x$claim_ind_cov_1_2_3_4_5_6 == "1"])
t.test(x$global_if[x$claim_ind_cov_1_2_3_4_5_6 == "0"], x$global_if[x$claim_ind_cov_1_2_3_4_5_6 == "1"])


ks.test(x$global_maha[1:2000000], x$global_maha[200001:4000000])

ks.test(x$global_if[x$claim_ind_cov_1_2_3_4_5_6 == "0"], x$global_if[x$claim_ind_cov_1_2_3_4_5_6 == "1"])



