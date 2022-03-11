# Importer les librairies =======================================================================================================
library(conflicted)
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
library(solitude)
library(mixOmics)
map <- purrr::map
filter <- dplyr::filter
select <- dplyr::select


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
  workspace_on_error = F,
  iteration = "vector"
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
    train_test_vins,
    {
      set.seed(2022)
      train_idx <- sample(seq_along(vins_with_claim), size = round(0.7 * length(vins_with_claim)))
      test_idx <- seq_along(vins_with_claim)[-train_idx]
      
      train_vins <- c(vins_no_claim[train_idx], vins_with_claim[train_idx])
      test_vins <- c(vins_no_claim[test_idx], vins_with_claim[test_idx])
      
      return(list(train = train_vins, test = test_vins))
    }
  ),
  
  tar_target(aug_trip_sample_train, filter(augmented_trip_data, vin %in% train_test_vins$train)),
  tar_target(aug_trip_sample_test, filter(augmented_trip_data, vin %in% train_test_vins$test)),
  tar_target(aug_trip_sample_complete, bind_rows(aug_trip_sample_train, aug_trip_sample_test)),
  
  tar_target(ml_data_train, create_ml_data(aug_trip_sample_train)),
  tar_target(ml_data_test, create_ml_data(aug_trip_sample_test)),
  
  tar_target(
    distance_train,
    aug_trip_sample_train %>% 
      group_by(vin) %>% 
      summarise(distance = sum(distance))
  ),
  
  tar_target(
    distance_test,
    aug_trip_sample_test %>% 
      group_by(vin) %>% 
      summarise(distance = sum(distance))
  ),

  tar_target(
    aug_trip_sample,
    filter(augmented_trip_data, vin %in% c(vins_no_claim[1:length(vins_with_claim)], vins_with_claim))
  ),
  
  tar_target(
    aug_trip_sample_baked,
    bake_data_lof(aug_trip_sample)
  ),
  

  # -----------------------------------------------------------------------------------------------------------------------------
  # Grilles de calibration des méthodes de détection d'anomalies ---------------------------------------------------------------- 
  # -----------------------------------------------------------------------------------------------------------------------------
    
  tar_target(local_lof_grid, seq(0.05, 0.6, by = 0.05)),
  tar_target(global_lof_grid, seq(5, 50, by = 5)),
  tar_target(local_if_grid, seq(0.05, 1, by = 0.05)),
  tar_target(global_if_grid, 2^seq(6, 10)),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Listes de vecteurs de scores d'anomalies (un vecteur pour chaque méthode-paramètre) ----------------------------------------- 
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_maha_train,
    compute_local_maha(aug_trip_sample_train)
  ),
  
  tar_target(
    global_maha_train,
    compute_global_maha(aug_trip_sample_train)
  ),
  
  tar_target(
    local_lof_train,
    compute_local_lofs(aug_trip_sample_train, k_frac = local_lof_grid),
    pattern = map(local_lof_grid),
    iteration = "list"
  ),
  
  tar_target(
    global_lof_train,
    compute_global_lofs(aug_trip_sample_train, k = global_lof_grid),
    pattern = map(global_lof_grid),
    iteration = "list"
  ),
  
  tar_target(
    local_if_train,
    compute_local_if(aug_trip_sample_train, k_frac = local_if_grid),
    pattern = map(local_if_grid),
    iteration = "list"
  ),
  
  tar_target(
    global_if_train,
    compute_global_if(aug_trip_sample_train, sample_size = global_if_grid),
    pattern = map(global_if_grid),
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Jeux de données d'entrainement avec seulement les scores d'anomalie (pas de variables classiques et distance) --------------- 
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # Mahalanobis -----------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_maha_train_ml,
    aug_trip_sample_train %>% 
      bind_cols(local_maha = local_maha_train) %>% 
      compute_percentiles(vars = "local_maha") %>% 
      bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)
  ),
  
  tar_target(
    global_maha_train_ml,
    aug_trip_sample_train %>% 
      bind_cols(global_maha = global_maha_train) %>% 
      compute_percentiles(vars = "global_maha") %>% 
      bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6)
  ),
  
  # LOF -------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_lof_train_ml,
    aug_trip_sample_train %>% 
      bind_cols(local_lof = local_lof_train) %>% 
      compute_percentiles(vars = "local_lof") %>% 
      bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6),
    pattern = map(local_lof_train),
    iteration = "list"
  ),
  
  tar_target(
    global_lof_train_ml,
    aug_trip_sample_train %>% 
      bind_cols(global_lof = global_lof_train) %>% 
      compute_percentiles(vars = "global_lof") %>% 
      bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6),
    pattern = map(global_lof_train),
    iteration = "list"
  ),
  
  # Isolation Forest ------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_if_train_ml,
    aug_trip_sample_train %>% 
      bind_cols(local_if = local_if_train) %>% 
      compute_percentiles(vars = "local_if") %>% 
      bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6),
    pattern = map(local_if_train),
    iteration = "list"
  ),
  
  tar_target(
    global_if_train_ml,
    aug_trip_sample_train %>% 
      bind_cols(global_if = global_if_train) %>% 
      compute_percentiles(vars = "global_if") %>% 
      bind_cols(claim_ind_cov_1_2_3_4_5_6 = ml_data_train$claim_ind_cov_1_2_3_4_5_6),
    pattern = map(global_if_train),
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Faire la validation croisée pour déterminer le meilleur paramètre pour chaque méthode --------------------------------------- 
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    recipe_tune_anomaly,
    recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = local_lof_train_ml[[1]]) %>%
      update_role(vin, new_role = "id") %>%
      step_normalize(all_predictors()) %>% 
      step_pca(all_numeric_predictors(), threshold = 0.95)
  ),
  
  # Mahalanobis -----------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_maha_tune,
    cv_logreg(local_maha_train_ml, recipe = recipe_tune_anomaly)
  ),
  
  tar_target(
    global_maha_tune,
    cv_logreg(global_maha_train_ml, recipe = recipe_tune_anomaly)
  ),
  
  # LOF -------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_lof_tune,
    cv_logreg(local_lof_train_ml, recipe = recipe_tune_anomaly),
    pattern = map(local_lof_train_ml),
    iteration = "list"
  ),
  
  tar_target(
    global_lof_tune,
    cv_logreg(global_lof_train_ml, recipe = recipe_tune_anomaly),
    pattern = map(global_lof_train_ml),
    iteration = "list"
  ),
  
  # Isolation Forest ------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_if_tune,
    cv_logreg(local_if_train_ml, recipe = recipe_tune_anomaly),
    pattern = map(local_if_train_ml),
    iteration = "list"
  ),
  
  tar_target(
    global_if_tune,
    cv_logreg(global_if_train_ml, recipe = recipe_tune_anomaly),
    pattern = map(global_if_train_ml),
    iteration = "list"
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  # Jeu de données training classique + distance + anomalie optimale ------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    class_dist_train_ml,
    ml_data_train %>% 
      left_join(distance_train, by = "vin")
  ),
  
  # Mahalanobis -----------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_maha_class_dist_train_ml,
    select(ml_data_train, -claim_ind_cov_1_2_3_4_5_6) %>% 
      left_join(distance_train, by = "vin") %>% 
      left_join(local_maha_train_ml, by = "vin")
  ),
  
  tar_target(
    global_maha_class_dist_train_ml,
    select(ml_data_train, -claim_ind_cov_1_2_3_4_5_6) %>% 
      left_join(distance_train, by = "vin") %>% 
      left_join(global_maha_train_ml, by = "vin")
  ),
  
  # LOF -------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_lof_class_dist_train_ml,
    select(ml_data_train, -claim_ind_cov_1_2_3_4_5_6) %>% 
      left_join(distance_train, by = "vin") %>% 
      left_join(local_lof_train_ml[[which.max(map(local_lof_tune, "mean"))]], by = "vin")
  ),
  
  tar_target(
    global_lof_class_dist_train_ml,
    select(ml_data_train, -claim_ind_cov_1_2_3_4_5_6) %>% 
      left_join(distance_train, by = "vin") %>% 
      left_join(global_lof_train_ml[[which.max(map(global_lof_tune, "mean"))]], by = "vin")
  ),
  
  # Isolation Forest ------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_if_class_dist_train_ml,
    select(ml_data_train, -claim_ind_cov_1_2_3_4_5_6) %>% 
      left_join(distance_train, by = "vin") %>% 
      left_join(local_if_train_ml[[which.max(map(local_if_tune, "mean"))]], by = "vin")
  ),
  
  tar_target(
    global_if_class_dist_train_ml,
    select(ml_data_train, -claim_ind_cov_1_2_3_4_5_6) %>% 
      left_join(distance_train, by = "vin") %>% 
      left_join(global_if_train_ml[[which.max(map(global_if_tune, "mean"))]], by = "vin")
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Calibrer les hyperparamètres du elastic-net ---------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    rec_en,
      recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = local_maha_class_dist_train_ml) %>%
      update_role(vin, new_role = "id") %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_lencode_glm(all_nominal_predictors(), outcome = "claim_ind_cov_1_2_3_4_5_6") %>%
      step_impute_bag(commute_distance, years_claim_free) %>%
      step_YeoJohnson(all_predictors()) %>% 
      step_normalize(all_predictors()) %>% 
      step_pca(q_0:q_100, threshold = 0.95) %>% 
      step_normalize(all_predictors())
  ),
  
  tar_target(
    rec_en_class_dist,
    recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = class_dist_train_ml) %>%
      update_role(vin, new_role = "id") %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_lencode_glm(all_nominal_predictors(), outcome = "claim_ind_cov_1_2_3_4_5_6") %>%
      step_impute_bag(commute_distance, years_claim_free) %>%
      step_YeoJohnson(all_predictors()) %>% 
      step_normalize(all_predictors())
  ),
  
  # ----------
  
  tar_target(tune_en_class_dist, tune_en(class_dist_train_ml, recipe = rec_en_class_dist)),
  tar_target(tune_en_local_maha, tune_en(local_maha_class_dist_train_ml, recipe = rec_en)),
  tar_target(tune_en_global_maha, tune_en(global_maha_class_dist_train_ml, recipe = rec_en)),
  tar_target(tune_en_local_lof, tune_en(local_lof_class_dist_train_ml, recipe = rec_en)),
  tar_target(tune_en_global_lof, tune_en(global_lof_class_dist_train_ml, recipe = rec_en)),
  tar_target(tune_en_local_if, tune_en(local_if_class_dist_train_ml, recipe = rec_en)),
  tar_target(tune_en_global_if, tune_en(global_if_class_dist_train_ml, recipe = rec_en)),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Entrainer les modèles optimaux sur l'ensemble d'entrainement ----------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    fit_class_dist, 
    fit_en(
      data = class_dist_train_ml, 
      recipe = rec_en_class_dist,
      penalty = select_best(tune_en_class_dist, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_class_dist, metric = "roc_auc")$mixture
    )
  ),
  
  # Mahalanobis -----------------------------------------------------------------------------------------------------------------
  
  tar_target(
    fit_local_maha_class_dist, 
    fit_en(
      data = local_maha_class_dist_train_ml, 
      recipe = rec_en,
      penalty = select_best(tune_en_local_maha, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_local_maha, metric = "roc_auc")$mixture
    )
  ),
  
  tar_target(
    fit_global_maha_class_dist, 
    fit_en(
      data = global_maha_class_dist_train_ml, 
      recipe = rec_en,
      penalty = select_best(tune_en_global_maha, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_global_maha, metric = "roc_auc")$mixture
    )
  ),
  
  # LOF -------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    fit_local_lof_class_dist, 
    fit_en(
      data = local_lof_class_dist_train_ml, 
      recipe = rec_en,
      penalty = select_best(tune_en_local_lof, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_local_lof, metric = "roc_auc")$mixture
    )
  ),
  
  tar_target(
    fit_global_lof_class_dist, 
    fit_en(
      data = global_lof_class_dist_train_ml, 
      recipe = rec_en,
      penalty = select_best(tune_en_global_lof, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_global_lof, metric = "roc_auc")$mixture
    )
  ),
  
  # Isolation Forest ------------------------------------------------------------------------------------------------------------
  
  tar_target(
    fit_local_if_class_dist, 
    fit_en(
      data = local_if_class_dist_train_ml, 
      recipe = rec_en,
      penalty = select_best(tune_en_local_if, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_local_if, metric = "roc_auc")$mixture
    )
  ),
  
  tar_target(
    fit_global_if_class_dist, 
    fit_en(
      data = global_if_class_dist_train_ml, 
      recipe = rec_en,
      penalty = select_best(tune_en_global_if, metric = "roc_auc")$penalty,
      mixture = select_best(tune_en_global_if, metric = "roc_auc")$mixture
    )
  ),

  # -----------------------------------------------------------------------------------------------------------------------------
  # Calcul des scores d'anomalie optimaux sur ensemble test ---------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(local_maha_test, compute_local_maha(aug_trip_sample_test)),
  tar_target(local_lof_test, compute_local_lofs(aug_trip_sample_test, k_frac = local_lof_grid[which.max(map(local_lof_tune, "mean"))])),
  tar_target(local_if_test, compute_local_if(aug_trip_sample_test, k_frac = local_if_grid[which.max(map(local_if_tune, "mean"))])),
  
  tar_target(global_maha_test,
    aug_trip_sample_complete %>% 
      mutate(global_maha = compute_global_maha(aug_trip_sample_complete)) %>% 
      filter(vin %in% train_test_vins$test) %>% 
      pull(global_maha)
  ),
  
  tar_target(global_lof_test,
    aug_trip_sample_complete %>% 
      mutate(global_lof = compute_global_lofs(aug_trip_sample_complete, k = global_lof_grid[which.max(map(global_lof_tune, "mean"))])) %>% 
      filter(vin %in% train_test_vins$test) %>% 
      pull(global_lof)
  ),
  
  tar_target(global_if_test,
    aug_trip_sample_complete %>% 
      mutate(global_if = compute_global_if(aug_trip_sample_complete, sample_size = global_if_grid[which.max(map(global_if_tune, "mean"))])) %>% 
      filter(vin %in% train_test_vins$test) %>% 
      pull(global_if)
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Jeux de données test --------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    class_dist_test_ml,
    left_join(ml_data_test, distance_test, by = "vin")
  ),
  
  # Mahalanobis -----------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_maha_class_dist_test_ml,
    aug_trip_sample_test %>% 
      bind_cols(local_maha = local_maha_test) %>% 
      compute_percentiles(vars = "local_maha") %>% 
      left_join(distance_test, by = "vin") %>% 
      left_join(ml_data_test, by = "vin")
  ),
  
  tar_target(
    global_maha_class_dist_test_ml,
    aug_trip_sample_test %>% 
      bind_cols(global_maha = global_maha_test) %>% 
      compute_percentiles(vars = "global_maha") %>% 
      left_join(distance_test, by = "vin") %>% 
      left_join(ml_data_test, by = "vin")
  ),
  
  # LOF -------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_lof_class_dist_test_ml,
    aug_trip_sample_test %>% 
      bind_cols(local_lof = local_lof_test) %>% 
      compute_percentiles(vars = "local_lof") %>% 
      left_join(distance_test, by = "vin") %>% 
      left_join(ml_data_test, by = "vin")
  ),
  
  tar_target(
    global_lof_class_dist_test_ml,
    aug_trip_sample_test %>% 
      bind_cols(global_lof = global_lof_test) %>% 
      compute_percentiles(vars = "global_lof") %>% 
      left_join(distance_test, by = "vin") %>% 
      left_join(ml_data_test, by = "vin")
  ),
  
  # Isolation Forest ------------------------------------------------------------------------------------------------------------
  
  tar_target(
    local_if_class_dist_test_ml,
    aug_trip_sample_test %>% 
      bind_cols(local_if = local_if_test) %>% 
      compute_percentiles(vars = "local_if") %>% 
      left_join(distance_test, by = "vin") %>% 
      left_join(ml_data_test, by = "vin")
  ),
  
  tar_target(
    global_if_class_dist_test_ml,
    aug_trip_sample_test %>% 
      bind_cols(global_if = global_if_test) %>% 
      compute_percentiles(vars = "global_if") %>% 
      left_join(distance_test, by = "vin") %>% 
      left_join(ml_data_test, by = "vin")
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Résultats sur le jeu de données test ----------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    test_results,
    list(
      class_dist = test_en(fit_class_dist, new_data = class_dist_test_ml),
      local_maha_class_dist = test_en(fit_local_maha_class_dist, new_data = local_maha_class_dist_test_ml),
      global_maha_class_dist = test_en(fit_global_maha_class_dist, new_data = global_maha_class_dist_test_ml),
      local_lof_class_dist = test_en(fit_local_lof_class_dist, new_data = local_lof_class_dist_test_ml),
      global_lof_class_dist = test_en(fit_global_lof_class_dist, new_data = global_lof_class_dist_test_ml),
      local_if_class_dist = test_en(fit_local_if_class_dist, new_data = local_if_class_dist_test_ml),
      global_if_class_dist = test_en(fit_global_if_class_dist, new_data = global_if_class_dist_test_ml)
    )
  )
  
  # =============================================================================================================================
)
