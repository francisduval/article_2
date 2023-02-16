library(targets)
library(tidyverse)
library(fs)
library(tidymodels)
library(gridExtra)
library(rlang)
library(gridExtra)
library(ggpubr)
library(here)

walk(dir_ls("R"), source)
theme_set(theme_bw())
options(scipen = 999)
set.seed(1004)

# ===============================================================================================================================

aug_trip_sample_train <- tar_read(aug_trip_sample_train)

global_maha_train <- tar_read(global_maha_train)
global_lof_train <- tar_read(global_lof_train)[[10]]
global_if_train <- tar_read(global_if_train)[[4]]

local_maha_train <- tar_read(local_maha_train)
local_lof_train <- tar_read(local_lof_train)[[7]]
local_if_train <- tar_read(local_if_train)[[17]]

# ===============================================================================================================================

dat <- 
  aug_trip_sample_train %>% 
  bind_cols(
    global_maha = global_maha_train,
    global_lof = global_lof_train,
    global_if = global_if_train,
    local_maha = local_maha_train,
    local_lof = local_lof_train,
    local_if = local_if_train
  ) %>% 
  select(
    claim_ind = claim_ind_cov_1_2_3_4_5_6,
    starts_with("global_"),
    starts_with("local_")
  )

# ===============================================================================================================================

make_plot <- function(data, score, xlab = NULL) {
  ggplot(data, aes(x = {{score}}, linetype = claim_ind)) +
    geom_density() +
    xlim(c(NA, quantile(data[[as_name(enquo(score))]], probs = 0.99))) +
    ylab("Density") +
    xlab(xlab) +
    scale_linetype_manual(labels = c("Non-claimants", "Claimants"), values = c("solid", "dashed"), name = NULL)
}

make_plot(dat, global_maha)

# ===============================================================================================================================

p1 <- make_plot(dat, score = global_maha, xlab = "Global Mahalanobis Score")
p2 <- make_plot(dat, score = global_lof, xlab = "Global LOF Score")
p3 <- make_plot(dat, score = global_if, xlab = "Global Isolation Forest Score")

p4 <- make_plot(dat, score = local_maha, xlab = "Local Mahalanobis Score")
p5 <- make_plot(dat, score = local_lof, xlab = "Local LOF Score")
p6 <- make_plot(dat, score = local_if, xlab = "Local Isolation Forest Score")

plot <- ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, common.legend = T, legend = "bottom")
plot
