library(tidyverse)
library(targets)
library(forcats)
library(tidytext)
theme_set(theme_bw())

ml_data_train <- tar_read(ml_data_train)
ml_data_test <- tar_read(ml_data_test)

ml_data <- bind_rows(ml_data_train, ml_data_test)

ml_data %>% 
  select_if(is.factor) %>%
  select(-claim_ind_cov_1_2_3_4_5_6) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>%
  count() %>%
  ungroup() %>%
  mutate(value = reorder_within(value, -n, name)) %>%
  ggplot(aes(x = value, y = n)) +
  geom_col(fill = "white", col = "black") +
  facet_wrap(~name, scales = "free") +
  scale_x_reordered() +
  xlab(NULL) +
  ylab("Number of vehicles") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
