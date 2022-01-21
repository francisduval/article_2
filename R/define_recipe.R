define_recipe <- function(data, covariates) {
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = data) %>%
    update_role(-all_of(covariates), -claim_ind_cov_1_2_3_4_5_6, new_role = "id") %>%
    step_other(all_nominal_predictors(), threshold = 0.05) %>%
    step_lencode_glm(all_nominal_predictors(), outcome = "claim_ind_cov_1_2_3_4_5_6") %>%
    step_impute_bag(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    step_YeoJohnson(all_predictors())
}
