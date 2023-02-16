compute_local_lofs <- function(data, k_frac) {
  data %>% 
    group_split(vin) %>% 
    map(bake_data_lof) %>% 
    map(~ lof(., minPts = round(k_frac * nrow(.)))) %>% 
    unlist()
}