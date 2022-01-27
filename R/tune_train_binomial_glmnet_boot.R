tune_train_binomial_glmnet_boot <- function(split, recipe, nboot = 50) {
  # --------------
  
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  train <- training(split)
  test <- testing(split)
  
  set.seed(1994)
  resamples <- bootstraps(train, times = nboot, strata = !!enquo(outcome))
  grid <- grid_regular(penalty(), mixture(), levels = c(50, 5))
  
  tune_spec <-
    logistic_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
    set_engine("glmnet")
  
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
  
  cv_res <- 
    collect_metrics(tuning) %>% 
    filter(
      penalty == best_params$penalty, 
      mixture == best_params$mixture
    )
  
  cv_pred <- 
    tuning %>% 
    collect_predictions(parameters = best_params)
  
  cv_roc <- 
    cv_pred %>% 
    roc_curve(!!enquo(outcome), .pred_0)
  
  # --------------
  
  last_fit <- 
    wf %>% 
    finalize_workflow(best_params) %>% 
    last_fit(
      split = split, 
      metrics = metric_set(roc_auc, accuracy, sensitivity, specificity)
    )
  
  fit <- last_fit$.workflow[[1]]
  
  test_res <- collect_metrics(last_fit)
  test_pred <- collect_predictions(last_fit)
  
  test_roc <- 
    test_pred %>% 
    roc_curve(!!enquo(outcome), .pred_0)
  
  # --------------
  
  res <- 
    list(
      predictors = predictors,
      outcome = outcome,
      grid = grid,
      tuning = tuning,
      best_params = best_params,
      cv_res = cv_res,
      cv_pred = cv_pred,
      cv_roc = cv_roc,
      last_fit = last_fit,
      fit = fit,
      test_res = test_res,
      test_pred = test_pred,
      test_roc = test_roc
    )
  
  class(res) <- c("tune_train_binomial_glmnet", class(res))
  
  # --------------
  
  return(res)
}
