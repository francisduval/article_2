compute_local_lofs <- function(data, k) {
  data %>% 
    group_split(vin) %>% 
    map(bake_data_lof) %>% 
    map(lof, minPts = k) %>% 
    unlist()
}