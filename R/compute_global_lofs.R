compute_global_lofs <- function(data, k) {
  data %>% 
    bake_data_lof() %>% 
    lof(minPts = k)
}
