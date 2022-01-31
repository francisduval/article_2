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
    classic_recipes_ls,
    {
      recipes_ls <- map(outcome_vec, ~ define_classic_recipe(training(ml_data_classic_split), outcome = all_of(.x)))
      names(recipes_ls) <- outcome_vec
      
      return(recipes_ls)
    }
  ),
  
  tar_target(
    tuned_glmnet_ls,
    map(classic_recipes_ls, ~ tune_train_binomial_glmnet(ml_data_classic_split, recipe = .))
  ),
  
  tar_target(
    auc_plot_responses,
    {
      df <- 
        map_df(tuned_glmnet_ls, collect_optimal_metrics) %>% 
        filter(.metric == "roc_auc")
      
      plot <- 
        ggplot(df, aes(x = outcome, y = mean)) + 
        geom_pointrange(aes(ymin = mean - std_err, ymax = mean + std_err)) +
        geom_point(aes(x = outcome, y = auc_test), shape = 8) +
        ggtitle("Performance de validation-croisée du elastic-net") +
        labs(subtitle = "Prédicteurs classiques", caption = "Le signe * indique l'AUC obtenu sur l'ensemble test") +
        xlab("Variable réponse utilisée") +
        ylab("AUC")
      
      ggsave(here("figures", "auc_plot_responses.png"), plot, width = 10)
      here("figures", "auc_plot_responses.png")
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Elastic-net avec quantité variable d'observations ---------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------

  tar_target(
    ml_data_classic_frac_split,
    {
      set.seed(1994)
      split_ls <- map(seq(0.025, 1, by = 0.025), ~ initial_split(slice_sample(ml_data_classic, prop = .), prop = 0.7))
      return(split_ls)
    }
  ),
  
  tar_target(
    tuned_glmnet_frac_ls,
    {
      tune_ls <-
        map(
          ml_data_classic_frac_split,
          ~ tune_train_binomial_glmnet(.x, recipe = classic_recipes_ls$claim_ind_cov_1_2_3_4_5_6)
        )
      names(tune_ls) <- seq(0.025, 1, by = 0.025)

      return(tune_ls)
    }
  ),
  
  tar_target(
    auc_plot_tuned_glmnet_frac,
    {
      df <- 
        map_df(tuned_glmnet_frac_ls, collect_optimal_metrics) %>% 
        filter(.metric == "roc_auc")
      nrow_test <- nrow(ml_data_classic$test)

      plot <-
        ggplot(df, aes(x = nb_train_obs, y = mean)) +
        geom_pointrange(aes(ymin = mean - std_err, ymax = mean + std_err), size = 0.2) +
        geom_line(size = 0.3, linetype = "dashed") +
        geom_point(aes(y = auc_test), shape = 8, size = 0.7) +
        ggtitle("Performance de validation-croisée du elastic-net") +
        labs(
          subtitle = "Prédicteurs classiques, variable réponse couvertures 1-2-3-4-5-6",
          caption = glue("- Le signe * indique l'AUC obtenu sur l'ensemble test\n- #obs_test = {nrow_test}")
        ) +
        xlab("Nombre d'observations dans l'ensemble d'entrainement") +
        ylab("AUC")

      ggsave(here("figures", "auc_plot_observations.png"), plot, width = 10)
      here("figures", "auc_plot_observations.png")
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
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # LOF globaux pour aug_trip_sample --------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(global_lofs_k_val, c(3, 10, 20)),
  tar_target(
    global_lofs,
    lof(bake_data_lof(aug_trip_sample), minPts = global_lofs_k_val), 
    pattern = map(global_lofs_k_val), 
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Jeu de données pour faire du machine learning -------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    ml_data,
    {
      distance_df <- 
        aug_trip_sample %>% 
        group_by(vin) %>% 
        summarise(distance = sum(distance)) %>% 
        ungroup()
      
      local_lofs_df <- 
        aug_trip_sample %>% 
        bind_cols(local_lofs = local_lofs) %>% 
        compute_stats(group = vin, vars = "local_lofs") %>% 
        rename_with(~ glue("local_lof_{.x}"), -vin)
      
      ml_data <- 
        aug_trip_sample["vin"] %>% 
        bind_cols(set_names(reduce(global_lofs, bind_cols), nm = glue("global_lof_{tar_read(global_lofs_k_val)}"))) %>% 
        compute_stats(group = vin, vars = glue("global_lof_{tar_read(global_lofs_k_val)}")) %>% 
        left_join(aug_trip_sample, by = "vin") %>% 
        group_by(vin) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(-all_of(make_trip_related_vars_vec())) %>% 
        left_join(distance_df, by = "vin") %>% 
        left_join(local_lofs_df, by = "vin") %>% 
      
      return(ml_data)
    }
  ),
  
  tar_target(
    ml_data_split,
    {
      set.seed(2021)
      split <- initial_split(ml_data, prop = 0.7)
      return(split)
    }
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Définir les vecteurs de variables explicatives ------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    covariates_vecs,
    list(
      class = make_classic_vars_vec(),
      class_dist = c(make_classic_vars_vec(), "distance"),
      class_dist_global_3 = c(make_classic_vars_vec(), "distance", names(ml_data)[str_detect(names(ml_data), "global_lof_3")]),
      class_dist_global_10 = c(make_classic_vars_vec(), "distance", names(ml_data)[str_detect(names(ml_data), "global_lof_10")]),
      class_dist_global_20 = c(make_classic_vars_vec(), "distance", names(ml_data)[str_detect(names(ml_data), "global_lof_20")]),
      global_3 = names(ml_data)[str_detect(names(ml_data), "global_lof_3")],
      global_10 = names(ml_data)[str_detect(names(ml_data), "global_lof_10")],
      global_20 = names(ml_data)[str_detect(names(ml_data), "global_lof_20")],
      class_global_3 = c(make_classic_vars_vec(), names(ml_data)[str_detect(names(ml_data), "global_lof_3")]),
      class_global_10 = c(make_classic_vars_vec(), names(ml_data)[str_detect(names(ml_data), "global_lof_10")]),
      class_global_20 = c(make_classic_vars_vec(), names(ml_data)[str_detect(names(ml_data), "global_lof_20")])
    ),
    iteration = "list"
  ),
  
  tar_target(
    recipes_ls,
    define_recipe(training(ml_data_split), covariates = covariates_vecs),
    pattern = map(covariates_vecs),
    iteration = "list"
  ),
  
  tar_target(
    glmnet_ls,
    tune_train_binomial_glmnet_boot(ml_data_split, recipe = recipes_ls),
    pattern = map(recipes_ls),
    iteration = "list"
  ),
  
  tar_target(
    glmnet_cv_res_ls,
    glmnet_ls[["tuning"]],
    pattern = map(glmnet_ls),
    iteration = "list"
  ),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # RMarkdown -------------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_render(
    rmd_anomaly_detection,
    path = "RMarkdown/anomaly_detection/anomaly_detection.Rmd"
  ),
  
  tar_render(
    rmd_automatic_tuning_lof,
    path = "RMarkdown/automatic_tuning_lof/automatic_tuning_lof.Rmd"
  )#,
  
  # tar_render(
  #   rmd_glmnet_results,
  #   path = "RMarkdown/glmnet_results/glmnet_results.Rmd"
  # )
  
  # =============================================================================================================================
)
