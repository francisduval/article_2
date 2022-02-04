tune_rf <- function(split, recipe, resamples, nb_trees = 1000) {
  # --------------
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  # --------------
  
  train <- training(split)
  test <- testing(split)
  
  tune_spec <- 
    rand_forest(
      mtry = tune(),
      trees = nb_trees,
      min_n = tune()
    ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  wf <- 
    workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(recipe)
  
  # --------------
  
  tuning <- 
    tune_grid(
      wf,
      resamples = resamples,
      grid = 20,
      metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
      control = control_grid(save_pred = T)
    )
  
  best_params <-
    tuning %>%
    select_best(metric = "roc_auc")
  
  # --------------
  
  res <- 
    list(
      outcome = outcome,
      predictors = predictors,
      tuning = tuning,
      best_params = best_params
    )
  
  # --------------
  
  return(res)
}
