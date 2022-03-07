compute_local_maha <- function(data) {
  data %>% 
    group_split(vin) %>% 
    map(bake_data_lof) %>% 
    map(~ mahalanobis(.x, center = colMeans(.x), cov = cov(.x))) %>% 
    unlist()
}