compute_percentiles <- function(data, vars) {
  p <- seq(0.01, 0.99, by = 0.01)
  p_names <- map_chr(p, ~ paste0("q_", .x * 100))
  p_funs <- map(p, ~ partial(quantile, probs = ., na.rm = T)) %>% set_names(nm = p_names)
  
  data %>% 
    group_by(vin) %>% 
    summarize_at(vars(vars), p_funs) %>% 
    ungroup()
}