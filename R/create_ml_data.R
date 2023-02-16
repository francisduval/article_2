create_ml_data <- function(trip_data) {
  trip_data %>% 
    group_by(vin) %>% 
    summarise(
      annual_distance = first(annual_distance),
      commute_distance = first(commute_distance),
      conv_count_3_yrs_minor = first(conv_count_3_yrs_minor),
      gender = first(gender),
      marital_status = first(marital_status),
      pmt_plan = first(pmt_plan),
      veh_age = first(veh_age),
      veh_use = first(veh_use),
      years_claim_free = first(years_claim_free),
      years_licensed = first(years_licensed),
      claim_ind_cov_1_2_3_4_5_6 = first(claim_ind_cov_1_2_3_4_5_6)
    ) %>% 
    ungroup()
}