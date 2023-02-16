compute_local_if <- function(data, k_frac) {
  data %>% 
    group_split(vin) %>% 
    map(bake_data_lof) %>% 
    map(~ compute_if(.x, sample_size = round(k_frac * nrow(.x)))) %>% 
    unlist()
}