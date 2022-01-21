compute_stats <- function(data, group, vars) {
  data %>% 
    group_by({{group}}) %>% 
    summarise_at(
      .vars = all_of(vars),
      .funs = 
        list(
          mean = mean, 
          var = var, 
          skewness = skewness, 
          kurtosis = kurtosis,
          q01 = ~ quantile(., probs = 0.01),
          q05 = ~ quantile(., probs = 0.05),
          q25 = ~ quantile(., probs = 0.25),
          q50 = ~ quantile(., probs = 0.5),
          q75 = ~ quantile(., probs = 0.75),
          q95 = ~ quantile(., probs = 0.95),
          q99 = ~ quantile(., probs = 0.99)
        )
    ) %>% 
    ungroup()
}
