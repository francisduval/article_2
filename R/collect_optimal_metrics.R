collect_optimal_metrics <- function(tune_train_binomial_glmnet) {
  collect_metrics(tune_train_binomial_glmnet$tuning) %>% 
    filter(
      mixture == tune_train_binomial_glmnet$best_params$mixture, 
      penalty == tune_train_binomial_glmnet$best_params$penalty
    ) %>% 
    mutate(
      outcome = str_replace(tune_train_binomial_glmnet$outcome, "claim_ind_", ""),
      nb_train_obs = nrow(tune_train_binomial_glmnet$train_df),
      auc_test = tune_train_binomial_glmnet$auc_test
    )
}