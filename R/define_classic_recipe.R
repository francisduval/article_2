define_classic_recipe <- function(data, outcome) {
  recipe(~ ., data = data) %>%
    update_role(-starts_with("c_"), new_role = "id") %>%
    update_role(!!enquo(outcome), new_role = "outcome") %>%
    step_other(all_nominal_predictors(), threshold = 0.05) %>%
    step_lencode_glm(all_nominal_predictors(), outcome = vars(!!enquo(outcome))) %>%
    step_impute_bag(c_commute_distance) %>%
    step_normalize(all_predictors()) %>%
    step_YeoJohnson(all_predictors())
}
