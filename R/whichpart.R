whichpart <- function(x, n = 30) {
  nx <- length(x)
  p <- nx - n
  xp <- sort(x, partial = p)[p]
  
  res <- which(x > xp)
  
  if(length(res) == 0) {
    res <- which.max(x)
  }
  
  return(res)
}