create_ml_data_classic <- function(augmented_trip_data) {
  augmented_trip_data %>% 
    group_by(vin) %>% 
    slice(1) %>% 
    select(
      vin,
      c_annual_distance = annual_distance,
      c_commute_distance = commute_distance,
      c_conv_count_3_yrs_minor = conv_count_3_yrs_minor,
      c_gender = gender,
      c_marital_status = marital_status,
      c_pmt_plan = pmt_plan,
      c_veh_age = veh_age,
      c_veh_use = veh_use,
      c_years_claim_free = years_claim_free,
      c_years_licensed = years_licensed,
      starts_with("nb_claims_"),
      starts_with("claim_ind_")
    ) %>% 
    ungroup() %>% 
    drop_na(c_years_claim_free)
}