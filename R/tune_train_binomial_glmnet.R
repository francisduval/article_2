tune_train_binomial_glmnet <- function(train_df, recipe, test_df) {
  # --------------
  
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  # --------------
  
  set.seed(1994)
  resamples <- vfold_cv(train_df, v = 5, strata = !!enquo(outcome))
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
      metrics = metric_set(roc_auc),
      control = control_grid(save_pred = T)
    )
  
  best_params <-
    tuning %>%
    select_best(metric = "roc_auc")
  
  # --------------
  
  pred_cv <- 
    tuning %>% 
    collect_predictions(parameters = best_params)
  
  roc_cv <- 
    pred_cv %>% 
    roc_curve(!!enquo(outcome), .pred_0)
  
  auc_cv <- 
    pred_cv %>% 
    roc_auc(!!enquo(outcome), .pred_0) %>% 
    pull(.estimate)
  
  # --------------
  
  fit <- 
    wf %>%
    finalize_workflow(best_params) %>%
    fit(data = train_df)
  
  pred_test <- predict(fit, new_data = test_df, type = "prob")
  
  roc_test <- 
    pred_test %>% 
    bind_cols(test_df[outcome]) %>% 
    roc_curve(!!enquo(outcome), .pred_0)
  
  auc_test <- 
    pred_test %>% 
    bind_cols(test_df[outcome]) %>% 
    roc_auc(!!enquo(outcome), .pred_0) %>% 
    pull(.estimate)
  
  # --------------
  
  res <- 
    list(
      train_df = train_df,
      test_df = test_df,
      recipe = recipe,
      predictors = predictors,
      outcome = outcome,
      resamples = resamples,
      grid = grid,
      tuning = tuning,
      best_params = best_params,
      pred_cv = pred_cv,
      pred_test = pred_test,
      roc_cv = roc_cv,
      roc_test = roc_test,
      auc_cv = auc_cv,
      auc_test = auc_test,
      fit = fit
    )
  
  class(res) <- c("tune_train_binomial_glmnet", class(res))
  
  # --------------
  
  return(res)
}
