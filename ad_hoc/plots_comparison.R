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

normal <- 
  aug_trip_sample_train %>% 
  bind_cols(
    global_maha = global_maha_train,
    global_lof = global_lof_train,
    global_if = global_if_train
  ) %>% 
  filter(vin == "3CZRU5H36HM101893")

peculier <- 
  aug_trip_sample_train %>% 
  bind_cols(
    global_maha = global_maha_train,
    global_lof = global_lof_train,
    global_if = global_if_train
  ) %>% 
  filter(vin == "KM8J3CA44HU363349")

# ----------

routinier <- 
  aug_trip_sample_train %>% 
  bind_cols(
    local_maha = local_maha_train,
    local_lof = local_lof_train,
    local_if = local_if_train
  ) %>% 
  filter(vin == "WBA3B3C59EJ979497")

non_routinier <- 
  aug_trip_sample_train %>% 
  bind_cols(
    local_maha = local_maha_train,
    local_lof = local_lof_train,
    local_if = local_if_train
  ) %>% 
  filter(vin == "1C4NJRCB8FD194248")

# ===============================================================================================================================

dat_pec <- 
  normal %>% 
  bind_rows(peculier) %>% 
  select(vin, starts_with("global_"))

dat_rout <- 
  routinier %>% 
  bind_rows(non_routinier) %>% 
  select(vin, starts_with("local_"))


# ===============================================================================================================================

make_plot <- function(data, score, xlab = NULL) {
  data %>% 
    ggplot(aes(x = {{score}}, linetype = vin)) +
    geom_density() +
    xlim(c(NA, quantile(data[[as_name(enquo(score))]], probs = 0.99))) +
    ylab("Density") +
    xlab(xlab) +
    scale_linetype_manual(labels = c("Non-peculiar vehicle", "Peculiar vehicle"), values = c("solid", "dashed"), name = NULL)
}

make_plot_2 <- function(data, score, xlab = NULL) {
  data %>% 
    ggplot(aes(x = {{score}}, linetype = vin)) +
    geom_density() +
    xlim(c(NA, quantile(data[[as_name(enquo(score))]], probs = 0.99))) +
    ylab("Density") +
    xlab(xlab) +
    scale_linetype_manual(labels = c("Non-routine vehicle", "Routine vehicle"), values = c("solid", "dashed"), name = NULL)
}

# ===============================================================================================================================

p1 <- make_plot(dat_pec, score = global_maha, xlab = "Global Mahalanobis Score")
p2 <- make_plot(dat_pec, score = global_lof, xlab = "Global LOF Score")
p3 <- make_plot(dat_pec, score = global_if, xlab = "Global Isolation Forest Score")

plot_1 <- ggarrange(p1, p2, p3, nrow = 1, common.legend = T, legend = "bottom")

p1_2 <- make_plot_2(dat_rout, score = local_maha, xlab = "Local Mahalanobis Score")
p2_2 <- make_plot_2(dat_rout, score = local_lof, xlab = "Local LOF Score")
p3_2 <- make_plot_2(dat_rout, score = local_if, xlab = "Local Isolation Forest Score")

plot_2 <- ggarrange(p1_2, p2_2, p3_2, nrow = 1, common.legend = T, legend = "bottom")

# ===============================================================================================================================

ggsave(here("figures", "plot_pec.eps"), plot = plot_1, width = 16, bg = "transparent")
ggsave(here("figures", "plot_rout.eps"), plot = plot_2, width = 16, bg = "transparent")

# ===============================================================================================================================

p <- seq(0, 1, by = 0.01)
p_names <- map_chr(p, ~ paste0("q_", .x * 100))
p_funs <- map(p, ~ partial(quantile, probs = ., na.rm = T)) %>% set_names(nm = p_names)

x <- 
  dat_rout %>% 
  select(vin, local_maha) %>% 
  group_by(vin) %>% 
  summarize_at(vars(local_maha), p_funs) %>% 
  pivot_longer(cols = starts_with("q_")) %>% 
  mutate(name = as.numeric(str_sub(name, start = 3)))

ggplot(x, aes(x = name, y = value, linetype = vin)) +
  geom_line()
