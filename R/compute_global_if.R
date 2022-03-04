compute_global_if <- function(data, sample_size) {
  data %>% 
    bake_data_lof() %>% 
    compute_if(sample_size = sample_size)
}
