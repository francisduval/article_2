compute_scores_iforest <- function(data) {
  iso = isolationForest$new(sample_size = min(256, nrow(data)))
  iso$fit(data)
  scores <- iso$predict(data)
  return(scores$anomaly_score)
}
