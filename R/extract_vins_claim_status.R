extract_vins_claim_status <- function(data, response, claim = T) {
  if(claim) {
    res <- 
      data %>% 
      group_by(vin) %>% 
      slice(1) %>% 
      filter(!!enquo(response) == "1") %>% 
      pull(vin)
  } else {
    res <- 
      data %>% 
      group_by(vin) %>% 
      slice(1) %>% 
      filter(!!enquo(response) == "0") %>% 
      pull(vin)
  }
  return(res)
}