tune_en <- function(data, recipe) {
  # --------------
  
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  set.seed(1994)
  grid <- grid_regular(penalty(), mixture(), levels = c(50, 5))
  cv <- vfold_cv(data, v = 5, strata = claim_ind_cov_1_2_3_4_5_6)
  
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
      resamples = cv,
      grid = grid,
      metrics = metric_set(roc_auc, accuracy, mn_log_loss, f_meas, sensitivity, specificity),
      control = control_grid(save_pred = F)
    )
  
  # --------------
  
  return(tuning)
}
