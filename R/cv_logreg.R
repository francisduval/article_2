cv_logreg <- function(data, recipe) {
  set.seed(1994)
  cv <- vfold_cv(data, v = 5, strata = claim_ind_cov_1_2_3_4_5_6)
  
  model <-
    logistic_reg() %>%
    set_engine("glm")
  
  wf <- 
    workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)
  
  res <- 
    fit_resamples(
      wf,
      resamples = cv,
      metrics = metric_set(roc_auc)
    )
  
  metrics <- collect_metrics(res)
  
  return(metrics)
}
