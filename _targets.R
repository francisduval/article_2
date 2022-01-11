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
  )
  
  # =============================================================================================================================
)