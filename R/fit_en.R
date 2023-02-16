fit_en <- function(data, recipe, penalty, mixture) {
  model <-
    logistic_reg(
      penalty = penalty,
      mixture = mixture
    ) %>%
    set_engine("glmnet")
  
  wf <- 
    workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)
  
  fit <- fit(wf, data = data)
  
  return(fit)
}
