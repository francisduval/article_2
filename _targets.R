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
  workspace_on_error = T
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
    {
      ml_data <- create_ml_data_classic(augmented_trip_data)
      
      set.seed(2021)
      split <- initial_split(ml_data, prop = 0.7)
      
      ml_data_train <- training(split)
      ml_data_test <- testing(split)
      
      return(list(complete = ml_data, train = ml_data_train, test = ml_data_test))
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
      recipes_ls <- map(outcome_vec, ~ define_classic_recipe(ml_data_classic$train, outcome = all_of(.x)))
      names(recipes_ls) <- outcome_vec
      
      return(recipes_ls)
    }
  ),
  
  tar_target(
    tuned_glmnet_ls,
    map(classic_recipes_ls, ~ tune_train_binomial_glmnet(ml_data_classic$train, recipe = ., test_df = ml_data_classic$test))
  ),
  
  tar_target(
    auc_plot_responses,
    {
      df <- map_df(tuned_glmnet_ls, collect_optimal_metrics)
      
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
    ml_data_classic_frac,
    {
      set.seed(1994)
      train <- map(seq(0.025, 1, by = 0.025), ~ slice_sample(ml_data_classic$train, prop = .))
      test <- map(seq(0.025, 1, by = 0.025), ~ slice_sample(ml_data_classic$test, prop = .))
      
      return(list(complete = complete, train = train, test = test))
    }
  ),
  
  tar_target(
    tuned_glmnet_frac_ls,
    {
      tune_ls <- 
        map(
          ml_data_classic_frac$train,
          ~ tune_train_binomial_glmnet(.x, recipe = classic_recipes_ls$claim_ind_cov_1_2_3_4_5_6, test_df = ml_data_classic$test)
        )
      names(tune_ls) <- seq(0.025, 1, by = 0.025)
      
      return(tune_ls)
    }
  ),
  
  tar_target(
    auc_plot_tuned_glmnet_frac,
    {
      df <- map_df(tuned_glmnet_frac_ls, collect_optimal_metrics)
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
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # LOF global pour aug_trip_sample ---------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(
    aug_trip_small_sample,
    filter(aug_trip_sample, vin %in% c(vins_no_claim[1:10], vins_with_claim[1:10]))
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
  )
  
  
  
  # =============================================================================================================================
)
