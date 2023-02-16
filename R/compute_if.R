compute_if <- function(data, sample_size) {
  iso = isolationForest$new(sample_size = sample_size, max_depth = 100)
  iso$fit(data)
  scores <- iso$predict(data)
  return(scores$anomaly_score)
}
