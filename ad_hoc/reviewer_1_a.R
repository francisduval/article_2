library(targets)
library(tidyverse)

test_preds <- tar_read(test_preds)
test_data <- tar_read(class_dist_test_ml)
ml_data_classic <- tar_read(ml_data_classic)

prob_moyenne <- mean(ml_data_classic$claim_ind_cov_1_2_3_4_5_6 == "1")
cout_total <- prob_moyenne * nrow(test_data) * 1000

somme_prob_without <- sum(test_preds$class_dist$.pred_1)
somme_prob_with <- sum(test_preds$global_maha_class_dist$.pred_1)

tib <-
  tibble(
    claim_ind = test_data$claim_ind_cov_1_2_3_4_5_6,
    pred_without = test_preds$class_dist$.pred_1,
    pred_with = test_preds$global_maha_class_dist$.pred_1
  )  %>% 
  mutate(
    prime_without = (pred_without / somme_prob_without) * cout_total,
    prime_with = (pred_with / somme_prob_with) * cout_total
  ) %>% 
  select(
    -pred_without,
    -pred_with
  )

tib %>% 
  group_by(claim_ind) %>% 
  summarise_all(mean)


