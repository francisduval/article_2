library(tidyverse)
library(tidymodels)
library(embed)

dat <- 
  tibble(
    x = factor(c("red", "blue", "red", "blue", "white")),
    y = factor(c(1, 0, 1, 1, 0), levels = c(1, 0))
  )


glm(y ~ x + 0, family = binomial, data = dat)


rec <- 
  recipe(y ~ ., data = dat) %>%
  step_lencode_glm(all_nominal_predictors(), outcome = "y")

rec %>% prep %>% juice
