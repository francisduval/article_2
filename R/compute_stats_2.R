compute_stats_2 <- function(data, col) {
  names <- 
    c(
      glue("{col}_mean"),
      glue("{col}_var"),
      glue("{col}_min"),
      glue("{col}_q05"),
      glue("{col}_q25"),
      glue("{col}_median"),
      glue("{col}_q75"),
      glue("{col}_q95"),
      glue("{col}_max"),
      glue("{col}_range"),
      glue("{col}_IQR"),
      glue("{col}_skewness"),
      glue("{col}_kurtosis")
    )
  
  funs <- 
    list(
      mean, 
      var, 
      ~ quantile(., probs = 0),
      ~ quantile(., probs = 0.05),
      ~ quantile(., probs = 0.25),
      ~ quantile(., probs = 0.5),
      ~ quantile(., probs = 0.75),
      ~ quantile(., probs = 0.95),
      ~ quantile(., probs = 1),
      ~ diff(range(.)),
      IQR,
      skewness,
      kurtosis
    ) %>% 
    set_names(names)
  
  data %>% 
    group_by(vin) %>% 
    summarize_at(all_of(col), funs) %>% 
    ungroup()
}
