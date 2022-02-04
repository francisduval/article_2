tune_train_xgboost <- function(split, recipe, resamples) {
  # --------------
  
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  # --------------
  
  train <- training(split)
  test <- testing(split)
  
  set.seed(1994)
  grid <- 
    grid_latin_hypercube(
      tree_depth(),
      min_n(),
      loss_reduction(),
      sample_size = sample_prop(),
      finalize(mtry(), train),
      learn_rate(),
      size = 30
    )
  
  tune_spec <- 
    boost_tree(
      trees = 1000, 
      tree_depth = tune(), 
      min_n = tune(), 
      loss_reduction = tune(),
      sample_size = tune(), 
      mtry = tune(),
      learn_rate = tune(),
    ) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  wf <- 
    workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(recipe)
  
  # --------------
  
  tuning <- 
    tune_grid(
      wf,
      resamples = resamples,
      grid = grid,
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