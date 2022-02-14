compute_scores_iforest <- function(data) {
  iso = isolationForest$new()
  iso$fit(data)
  scores <- iso$predict(data)
  return(scores$anomaly_score)
}