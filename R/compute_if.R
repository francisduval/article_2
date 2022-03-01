compute_if <- function(data, sample_size) {
  iso = isolationForest$new(sample_size = sample_size)
  iso$fit(data)
  scores <- iso$predict(data)
  return(scores$anomaly_score)
}
