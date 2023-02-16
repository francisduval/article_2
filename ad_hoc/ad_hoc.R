library(targets)
library(tidyverse)
library(tidymodels)
library(dbscan)
theme_set(theme_bw())

# ===============================================================================================================================

aug_trip_sample <- tar_read(aug_trip_sample)

routinier <- 
  aug_trip_sample %>% 
  filter(vin == "WBA3B3C59EJ979497")

non_routinier <- 
  aug_trip_sample %>% 
  filter(vin == "1C4NJRCB8FD194248")

ggplot(routinier, aes(x = cos_time_start, y = sin_time_start)) +
  geom_point(size = 0.3, alpha = 0.3)

ggplot(non_routinier, aes(x = cos_time_start, y = sin_time_start)) +
  geom_point(size = 0.3, alpha = 0.3)

# ===============================================================================================================================

bake_data_lof <- function(data) {
  rec_prep <- 
    recipe(~ ., data = data) %>%
    step_rm(
      -cos_time_start,
      -sin_time_start
    ) %>% 
    step_normalize(all_predictors()) %>% 
    prep()
  
  data_baked <- bake(rec_prep, new_data = data)
  
  return(data_baked)
}

# ===============================================================================================================================

compute_local_lofs <- function(data, k_frac) {
  data %>% 
    group_split(vin) %>% 
    map(bake_data_lof) %>% 
    map(~ lof(., minPts = round(k_frac * nrow(.)))) %>%
    # map(~ lof(., minPts = k_frac)) %>% 
    unlist()
}

# ===============================================================================================================================

k_frac <- 0.01

scores_routinier <- tibble(score = compute_local_lofs(routinier, k_frac = k_frac), driver = "routinier")
scores_non_routinier <- tibble(score = compute_local_lofs(non_routinier, k_frac = k_frac), driver = "non_routinier")

dat <- 
  bind_rows(
    scores_routinier,
    scores_non_routinier
  )

ggplot(dat, aes(x = score, linetype = driver)) +
  geom_density() +
  xlim(c(NA, quantile(dat[["score"]], probs = 0.99)))



