test_en <- function(fit, new_data) {
  preds <- predict(fit, new_data = new_data, type = "prob")
  preds_hard <- predict(fit, new_data = new_data)
  
  res <- 
    tibble(
      roc_auc = roc_auc_vec(new_data$claim_ind_cov_1_2_3_4_5_6, preds$.pred_0),
      accuracy = accuracy_vec(new_data$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class),
      mn_log_loss = mn_log_loss_vec(new_data$claim_ind_cov_1_2_3_4_5_6, preds$.pred_0),
      f_meas = f_meas_vec(new_data$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class),
      sensitivity = sensitivity_vec(new_data$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class),
      specificity = specificity_vec(new_data$claim_ind_cov_1_2_3_4_5_6, preds_hard$.pred_class)
  )
  
  return(res)
}
