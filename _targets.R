# Importer les librairies =======================================================================================================
library(targets)
library(tarchetypes)
library(tidyverse)
library(tidymodels)
library(here)
library(rsample)
library(lubridate)
library(qs)
library(fastDummies)
library(here)
library(dtplyr)
library(hms)
library(fs)
library(embed)
library(glmnet)
library(glue)
library(dbscan)
library(e1071)
library(ranger)


# Lire les fonctions ============================================================================================================
walk(dir_ls("R"), source)


# Options et thème ==============================================================================================================
options(scipen = 999)
theme_set(theme_bw())


# Options =======================================================================================================================
tar_option_set(
  garbage_collection = T,
  memory = "transient",
  format = "qs",
  workspace_on_error = F
)


# Targets =======================================================================================================================
list(
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Préparation des données -----------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(contract_file, "data/Contrat_Nov2020.csv", format = "file"),
  tar_target(contract_data, prepare_contract_data(contract_file)),
  tar_target(claim_data, prepare_claim_data(contract_data)),
  tar_files_input(trip_files, list.files(here("data"), pattern = "TRIP_VIN", full.names = T), format  = "file"),
  tar_target(trip_data, clean_trip_file(trip_files), pattern = map(trip_files)),
  tar_target(
    augmented_trip_data,
    {
      join_contracts_claims_trips(contract_data, claim_data, trip_data) %>%
        group_by(vin) %>%
        filter(n() >= 100) %>%
        ungroup() %>%
        compute_extra_trip_vars()
    },
    pattern = map(trip_data)
  ),
  tar_target(
    ml_data_classic,
    create_ml_data_classic(augmented_trip_data)
  ),
  tar_target(
    ml_data_classic_split,
    {
      set.seed(2021)
      split <- initial_split(ml_data_classic, prop = 0.7)
      return(split)
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Elastic-net avec chacune des variables réponse ------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    outcome_vec,
    c("claim_ind_cov_2", "claim_ind_cov_3", "claim_ind_cov_4", "claim_ind_cov_2_4", "claim_ind_cov_1_2_3_4_5_6")
  ),
  
  tar_target(
    resamples_classic_ls, 
    vfold_cv(training(ml_data_classic_split), v = 5, strata = outcome_vec),
    pattern = map(outcome_vec),
    iteration = "list"
  ),
  
  tar_target(
    recettes_classic_ls,
    recipe(~ ., data = training(ml_data_classic_split)) %>%
      update_role(-starts_with("c_"), new_role = "id") %>%
      update_role(outcome_vec, new_role = "outcome") %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_lencode_glm(all_nominal_predictors(), outcome = outcome_vec) %>%
      step_impute_bag(c_commute_distance) %>%
      step_normalize(all_predictors()) %>%
      step_YeoJohnson(all_predictors()),
    pattern = map(outcome_vec),
    iteration = "list"
  ),
  
  tar_target(
    glmnet_classic_ls,
    tune_train_binomial_glmnet(ml_data_classic_split, recipe = recettes_classic_ls, resamples = resamples_classic_ls),
    pattern = map(recettes_classic_ls, resamples_classic_ls),
    iteration = "list"
  ),

  tar_target(
    glmnet_classic_auc_plot,
    {
      test_auc_vec <- 
        map(glmnet_classic_ls, "last_fit") %>% 
        map(collect_metrics) %>% 
        map_df(filter, .metric == "roc_auc") %>% 
        pull(.estimate)  
     
       df <- 
        map(glmnet_classic_ls, "tuning") %>% 
        map(collect_metrics) %>% 
        map(filter, .metric == "roc_auc") %>% 
        map_df(filter, mean == max(mean)) %>% 
        mutate(outcome = outcome_vec, auc_test = test_auc_vec)
        
      plot <-
        ggplot(df, aes(x = outcome, y = mean)) +
        geom_pointrange(aes(ymin = mean - std_err, ymax = mean + std_err)) +
        geom_point(aes(x = outcome, y = auc_test), shape = 8) +
        ggtitle("Performance de validation-croisée du elastic-net") +
        labs(subtitle = "Prédicteurs classiques", caption = "Le signe * indique l'AUC obtenu sur l'ensemble test") +
        xlab("Variable réponse utilisée") +
        ylab("AUC")

      ggsave(here("figures", "glmnet_classic_auc_plot.png"), plot, width = 10)
      here("figures", "glmnet_classic_auc_plot.png")
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Elastic-net avec quantité variable d'observations ---------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------

  tar_target(fractions, seq(0.025, 1, by = 0.025)),
  
  tar_target(
    ml_data_classic_split_frac_ls,
    {
      set.seed(1994)
      initial_split(slice_sample(ml_data_classic, prop = fractions), prop = 0.7)
    },
    pattern = map(fractions),
    iteration = "list"
  ),
  
  tar_target(
    resamples_classic_frac_ls,
    vfold_cv(training(ml_data_classic_split_frac_ls), v = 5, strata = "claim_ind_cov_1_2_3_4_5_6"),
    pattern = map(ml_data_classic_split_frac_ls),
    iteration = "list"
  ),
  
  tar_target(
    glmnet_classic_frac_ls,
    tune_train_binomial_glmnet(
      ml_data_classic_split_frac_ls, 
      recipe = recettes_classic_ls[[which(outcome_vec == "claim_ind_cov_1_2_3_4_5_6")]], 
      resamples = resamples_classic_frac_ls
    ),
    pattern = map(ml_data_classic_split_frac_ls, resamples_classic_frac_ls),
    iteration = "list"
  ),
  
  tar_target(
    glmnet_classic_frac_auc_plot,
    {
      test_auc_vec <- 
        map(glmnet_classic_frac_ls, "last_fit") %>% 
        map(collect_metrics) %>% 
        map_df(filter, .metric == "roc_auc") %>% 
        pull(.estimate)
      
      df <- 
        map(glmnet_classic_frac_ls, "tuning") %>% 
        map_df(show_best, metric = "roc_auc", n = 1) %>%
        mutate(outcome = nrow(training(ml_data_classic_split)) * fractions, auc_test = test_auc_vec)
      
      plot <-
        ggplot(df, aes(x = outcome, y = mean)) +
        geom_pointrange(aes(ymin = mean - std_err, ymax = mean + std_err)) +
        geom_point(aes(x = outcome, y = auc_test), shape = 8) +
        geom_line(size = 0.3, linetype = "dashed") +
        ggtitle("Performance de validation-croisée du elastic-net") +
        labs(
          subtitle = "Prédicteurs classiques, variable réponse couvertures 1-2-3-4-5-6", 
          caption = "Le signe * indique l'AUC obtenu sur l'ensemble test"
        ) +
        xlab("Nombre d'observations dans l'ensemble d'entrainement") +
        ylab("AUC")
      
      ggsave(here("figures", "glmnet_classic_frac_auc_plot.png"), plot, width = 10)
      here("figures", "glmnet_classic_frac_auc_plot.png")
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Créer un échantillon équilibré de classification ----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    vins_no_claim,
    extract_vins_claim_status(augmented_trip_data, response = claim_ind_cov_1_2_3_4_5_6, claim = F),
    pattern = map(augmented_trip_data)
  ),
  
  tar_target(
    vins_with_claim,
    extract_vins_claim_status(augmented_trip_data, response = claim_ind_cov_1_2_3_4_5_6, claim = T),
    pattern = map(augmented_trip_data)
  ),

  tar_target(
    aug_trip_sample,
    filter(augmented_trip_data, vin %in% c(vins_no_claim[1:length(vins_with_claim)], vins_with_claim))
  ),
  
  tar_target(
    aug_trip_small_sample,
    filter(aug_trip_sample, vin %in% c(vins_no_claim[1:10], vins_with_claim[1:10]))
  ),
  
  tar_target(
    aug_trip_sample_baked,
    bake_data_lof(aug_trip_sample)
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Distance de Mahalanobis pour aug_trip_sample --------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    global_maha,
    mahalanobis(aug_trip_sample_baked, center = colMeans(aug_trip_sample_baked), cov = cov(aug_trip_sample_baked))
  ),
  
  tar_target(
    local_maha,
    aug_trip_sample %>% 
      group_split(vin) %>% 
      map(bake_data_lof) %>% 
      map(~ mahalanobis(.x, center = colMeans(.x), cov = cov(.x))) %>% 
      unlist()
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # LOF locaux pour aug_trip_sample ---------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    aug_trip_sample_group_vin,
    aug_trip_sample %>% 
      group_by(vin) %>% 
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    local_lofs_tuning,
    autotune_lof(
      data = aug_trip_sample_group_vin,
      grid_c = seq(0.01, 0.2, length.out = 20),
      grid_k_frac = exp(seq(log(0.005), log(0.2), length.out = 10))
    ),
    pattern = map(aug_trip_sample_group_vin),
    iteration = "list"
  ),
  
  tar_target(
    local_lofs,
    aug_trip_sample %>% 
      group_split(vin) %>% 
      map(bake_data_lof) %>% 
      map2(map_dbl(local_lofs_tuning, "k_opt"), ~ lof(.x, minPts = .y)) %>% 
      unlist()
  ),
  
  tar_target(
    local_lofs_10,
    aug_trip_sample %>% 
      group_split(vin) %>% 
      map(bake_data_lof) %>% 
      map(~ lof(.x, minPts = 10)) %>% 
      unlist()
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # LOF globaux pour aug_trip_sample --------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(global_lofs_k_val, c(10, 20, 30, 40)),
  tar_target(
    global_lofs,
    lof(bake_data_lof(aug_trip_sample), minPts = global_lofs_k_val),
    pattern = map(global_lofs_k_val),
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Un jeu de données pour chaque groupe de prédicteurs -------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    ml_data_class,
    aug_trip_sample %>% 
      group_by(vin) %>% 
      slice(1) %>% 
      select(vin, class_vars_vec()) %>% 
      ungroup()
  ),
  
  tar_target(
    ml_data_dist,
    aug_trip_sample %>% 
      group_by(vin) %>% 
      summarise(distance = sum(distance)) %>% 
      ungroup()
  ),
  
  tar_target(
    ml_data_global_maha,
    aug_trip_sample %>% 
      bind_cols(global_maha = global_maha) %>% 
      compute_stats(group = vin, vars = "global_maha") %>% 
      rename_with(~ glue("global_maha_{.x}"), -vin)
  ),
  
  tar_target(
    ml_data_local_maha,
    aug_trip_sample %>% 
      bind_cols(local_maha = local_maha) %>% 
      compute_stats(group = vin, vars = "local_maha") %>% 
      rename_with(~ glue("local_maha_{.x}"), -vin)
  ),
  
  tar_target(
    ml_data_local_lofs,
    aug_trip_sample %>% 
      bind_cols(local_lofs = local_lofs) %>% 
      compute_stats(group = vin, vars = "local_lofs") %>% 
      rename_with(~ glue("local_lof_{.x}"), -vin)
  ),
  
  tar_target(
    ml_data_global_lofs,
    aug_trip_sample %>%
      bind_cols(global_lofs = global_lofs) %>%
      compute_stats(group = vin, vars = "global_lofs") %>%
      rename_with(~ glue("global_lof_{global_lofs_k_val}_{.x}"), -vin),
    pattern = map(global_lofs, global_lofs_k_val),
    iteration = "list"
  ),
  
  tar_target(
    ml_data_response,
    aug_trip_sample %>% 
      group_by(vin) %>% 
      slice(1) %>% 
      select(vin, "claim_ind_cov_1_2_3_4_5_6") %>% 
      ungroup()
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  # Liste de jeux de données pour la classification -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    ml_df_ls,
    list(
      class = reduce(list(ml_data_class, ml_data_response), left_join, by = "vin"),
      class_dist = reduce(list(ml_data_class, ml_data_dist, ml_data_response), left_join, by = "vin"),
      class_dist_global_maha = reduce(list(ml_data_class, ml_data_dist, ml_data_global_maha, ml_data_response), left_join, by = "vin"),
      class_dist_local_maha = reduce(list(ml_data_class, ml_data_dist, ml_data_local_maha, ml_data_response), left_join, by = "vin"),
      class_dist_local_lof = reduce(list(ml_data_class, ml_data_dist, ml_data_local_lofs, ml_data_response), left_join, by = "vin"),
      class_dist_global_10_lof = reduce(list(ml_data_class, ml_data_dist, ml_data_global_lofs[[which(global_lofs_k_val == 10)]], ml_data_response), left_join, by = "vin"),
      class_dist_global_20_lof = reduce(list(ml_data_class, ml_data_dist, ml_data_global_lofs[[which(global_lofs_k_val == 20)]], ml_data_response), left_join, by = "vin"),
      class_dist_global_30_lof = reduce(list(ml_data_class, ml_data_dist, ml_data_global_lofs[[which(global_lofs_k_val == 30)]], ml_data_response), left_join, by = "vin"),
      class_dist_global_40_lof = reduce(list(ml_data_class, ml_data_dist, ml_data_global_lofs[[which(global_lofs_k_val == 40)]], ml_data_response), left_join, by = "vin")
    ),
    iteration = "list"
  ),
  
  tar_target(
    ml_split_ls,
    {
      set.seed(2021)
      initial_split(ml_df_ls, prop = 0.7)
    },
    pattern = map(ml_df_ls),
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Liste de recettes de prétraitement pour la classification -------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    recettes_ls,
    recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = training(ml_split_ls)) %>%
      update_role(vin, new_role = "id") %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_lencode_glm(all_nominal_predictors(), outcome = "claim_ind_cov_1_2_3_4_5_6") %>%
      step_impute_bag(all_predictors()) %>%
      step_normalize(all_predictors()) %>%
      step_YeoJohnson(all_predictors()),
    pattern = map(ml_split_ls),
    iteration = "list"
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  # Liste de resamples ----------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    bootstrap_ls,
    {
      set.seed(1994)
      bootstraps(training(ml_split_ls), times = 50, strata = "claim_ind_cov_1_2_3_4_5_6")
    },
    pattern = map(ml_split_ls),
    iteration = "list"
  ),
  
  tar_target(
    bootstrap_xgb_ls,
    {
      set.seed(1994)
      bootstraps(training(ml_split_ls), times = 25, strata = "claim_ind_cov_1_2_3_4_5_6")
    },
    pattern = map(ml_split_ls),
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Calibrer et entrainer les modèles -------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    glmnet_ls,
    tune_train_binomial_glmnet(ml_split_ls, recipe = recettes_ls, resamples = bootstrap_ls),
    pattern = map(ml_split_ls, recettes_ls, bootstrap_ls),
    iteration = "list"
  ),

  tar_target(
    xgb_ls,
    tune_train_xgboost(ml_split_ls, recipe = recettes_ls, resamples = bootstrap_xgb_ls),
    pattern = map(ml_split_ls, recettes_ls, bootstrap_xgb_ls),
    iteration = "list"
  ),
  
  tar_target(
    rf_ls,
    tune_rf(ml_split_ls, recipe = recettes_ls, resamples = bootstrap_xgb_ls),
    pattern = map(ml_split_ls, recettes_ls, bootstrap_xgb_ls),
    iteration = "list"
  ),

  # ----------
  
  tar_target(
    glmnet_tuning_ls,
    glmnet_ls[["tuning"]],
    pattern = map(glmnet_ls),
    iteration = "list"
  ),
  
  tar_target(
    xgb_tuning_ls,
    xgb_ls[["tuning"]],
    pattern = map(xgb_ls),
    iteration = "list"
  ),
  
  tar_target(
    rf_tuning_ls,
    rf_ls[["tuning"]],
    pattern = map(rf_ls),
    iteration = "list"
  ),
  
  # ----------
  
  tar_target(
    glmnet_last_fit_ls,
    glmnet_ls[["last_fit"]],
    pattern = map(glmnet_ls),
    iteration = "list"
  )
  
  # =============================================================================================================================
)
