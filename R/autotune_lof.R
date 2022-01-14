autotune_lof <- function(data, grid_c, grid_k_frac) {
  vin <- unique(data$vin); print(vin)
  nb_rows <- nrow(data)
  grid_k <- round(grid_k_frac * nb_rows)
  grid_k <- grid_k[grid_k >= 3]
  
  nb_c <- length(grid_c)
  nb_k <- length(grid_k)
  
  data_baked <- bake_data_lof(data)
  
  len <- nb_c * nb_k
  T_tib <- 
    tibble(
      c_val = vector(mode = "numeric", length = len),
      k_val = vector(mode = "numeric", length = len),
      T_val = vector(mode = "numeric", length = len)
    )
  
  ii <- 0L
  for(c in grid_c) {
    nb <- floor(c * nb_rows)
    
    for(k in grid_k) {
      ii <- ii + 1L
      
      lof_vec <- lof(data_baked, minPts = k)
      log_lof_vec <- log(lof_vec)
      
      out_ind <- whichpart(log_lof_vec, n = nb)
      in_ind <- whichpart(log_lof_vec[-out_ind], n = nb)
      
      log_lof_vec_out <- log_lof_vec[out_ind]
      log_lof_vec_in <- log_lof_vec[in_ind]
      
      M_ck_out <- mean(log_lof_vec_out)
      M_ck_in <- mean(log_lof_vec_in)
      
      V_ck_out <- var(log_lof_vec_out)
      V_ck_in <- var(log_lof_vec_in)
      
      T_tib[ii, 1] <- c
      T_tib[ii, 2] <- k
      T_tib[ii, 3] <- (M_ck_out - M_ck_in) / sqrt((V_ck_out + V_ck_in) / nb)
    }
  }
  
  k_opt_table <- 
    T_tib %>% 
    group_by(c_val) %>% 
    filter(T_val == max(T_val)) %>% 
    select(c_val, opt_k_val = k_val) %>% 
    ungroup()
  
  k_opt <- 
    k_opt_table %>% 
    pull(opt_k_val) %>% 
    median() %>% 
    round()
  
  res <- 
    list(
      vin = vin, 
      k_opt = k_opt, 
      nb_rows = nb_rows, 
      grid_k_frac = grid_k_frac, 
      grid_k = grid_k,
      k_opt_table = k_opt_table
    )
  
  class(res) <- c("autotune_lof", class(res))
  
  return(res)
}

# Méthode pour printer un objet de classe autotune_lof
print.autotune_lof <- function(obj) {
  cat("=================================================\n")
  cat("VIN:", obj$vin, "\n")
  cat("Nombre de trajets:", obj$nb_rows, "\n")
  cat("Grille de valeurs k utilisées (en % du nombre de lignes):", percent(obj$grid_k_frac), "\n")
  cat("Grille de valeurs k utilisées (en valeur absolue):", obj$grid_k, "\n")
  cat("Valeur de k optimale trouvée:", obj$k_opt, "\n\n")
  cat("Valeur optimale de k en fonction de c:\n")
  print(obj$k_opt_table)
  cat("=================================================")
}