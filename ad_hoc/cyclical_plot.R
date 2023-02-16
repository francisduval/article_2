library(tidyverse)
library(hms)
library(lubridate)
theme_set(theme_bw())

# ===============================================================================================================================

dat_1 <- 
  tibble(
    datetime = as_datetime((86400 * 4):431500),
    time = as.numeric(as_hms(datetime))
  )

dat_2 <- 
  tibble(
    datetime = as_datetime((86400 * 5):517900),
    time = as.numeric(as_hms(datetime))
  )

dat_3 <- 
  tibble(
    datetime = as_datetime((86400 * 6):604300),
    time = as.numeric(as_hms(datetime))
  )

dat_4 <- 
  tibble(
    datetime = as_datetime((86400 * 7):690700),
    time = as.numeric(as_hms(datetime))
  )

dat_5 <- 
  tibble(
    datetime = as_datetime((86400 * 8):777100),
    time = as.numeric(as_hms(datetime))
  )

dat_points_noirs <- 
  tibble(
    datetime = as_datetime((4:9) * 86400),
    time = 0
  )

dat_points_blancs <- 
  tibble(
    datetime = as_datetime((4:9) * 86400),
    time = 86400
  )

# ===============================================================================================================================

ggplot() +
  geom_line(aes(x = datetime, y = time), data = dat_1) +
  geom_line(aes(x = datetime, y = time), data = dat_2) +
  geom_line(aes(x = datetime, y = time), data = dat_3) +
  geom_line(aes(x = datetime, y = time), data = dat_4) +
  geom_line(aes(x = datetime, y = time), data = dat_5) +
  geom_point(aes(x = datetime, y = time), data = dat_points_noirs) +
  geom_point(aes(x = datetime, y = time), data = dat_points_blancs, shape = 1) +
  geom_segment(aes(x = as_datetime(86400 * 4), y = min(dat_1$time), xend = as_datetime(86400 * 4), yend = max(dat_1$time)), linetype = "dotted") +
  geom_segment(aes(x = as_datetime(86400 * 5), y = min(dat_1$time), xend = as_datetime(86400 * 5), yend = max(dat_1$time)), linetype = "dotted") +
  geom_segment(aes(x = as_datetime(86400 * 6), y = min(dat_1$time), xend = as_datetime(86400 * 6), yend = max(dat_1$time)), linetype = "dotted") +
  geom_segment(aes(x = as_datetime(86400 * 7), y = min(dat_1$time), xend = as_datetime(86400 * 7), yend = max(dat_1$time)), linetype = "dotted") +
  geom_segment(aes(x = as_datetime(86400 * 8), y = min(dat_1$time), xend = as_datetime(86400 * 8), yend = max(dat_1$time)), linetype = "dotted") +
  geom_segment(aes(x = as_datetime(86400 * 9), y = min(dat_1$time), xend = as_datetime(86400 * 9), yend = max(dat_1$time)), linetype = "dotted") +
  xlab(NULL) +
  ylab("Encoded value") +
  scale_x_datetime(
    breaks = as_datetime((4:9) * 86400),
    date_labels = "%A \n %H:%M:%S"
  ) +
  theme(text = element_text(size = 20)) 

# ===============================================================================================================================

dat <- 
  tibble(
    datetime = as_datetime((86400 * 4):(86400 * 9)),
    time = as.numeric(as_hms(datetime)),
    time_sin = sin((time * 2 * pi) / 86400),
    time_cos = cos((time * 2 * pi) / 86400)
  ) 

ggplot(dat, aes(x = datetime)) +
  xlim(as_datetime(345600), as_datetime(777600)) +
  geom_function(aes(linetype = "Cosine"), fun = function(x) cos((as.numeric(x) * 2 * pi) / 86400), n = 10000) +
  geom_function(aes(linetype = "Sine"), fun = function(x) sin((as.numeric(x) * 2 * pi) / 86400), n = 10000) +
  xlab(NULL) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  ylab("Encoded value") +
  labs(linetype = NULL) +
  scale_x_datetime(
    breaks = as_datetime((4:9) * 86400),
    date_labels = "%A \n %H:%M:%S"
  ) +
  theme(
    text = element_text(size = 20),
    legend.position = c(0.05, 0.13)
  )
