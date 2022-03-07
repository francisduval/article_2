compute_global_maha <- function(data) {
  data %>% 
    bake_data_lof() %>% 
    mahalanobis(center = colMeans(.), cov = cov(.))
}