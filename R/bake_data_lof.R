bake_data_lof <- function(data) {
  rec_prep <- 
    recipe(~ ., data = data) %>%
    step_rm(
      -duration,
      -distance,
      -avg_speed,
      -max_speed,
      -cos_time_start,
      -sin_time_start,
      -cos_nb_days_since_monday,
      -sin_nb_days_since_monday
    ) %>% 
    step_normalize(all_predictors()) %>% 
    prep()
  
  data_baked <- bake(rec_prep, new_data = data)
}
