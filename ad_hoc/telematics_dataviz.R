library(targets)
library(tidyverse)
library(lubridate)

theme_set(theme_bw())
options(scipen = 999)

aug_trip_sample <- tar_read(aug_trip_sample)
aug_trip_sample_baked <- tar_read(aug_trip_sample_baked)


dat <- aug_trip_sample

p1 <- 
  ggplot(dat, aes(x = duration)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 50) +
  ylab(NULL) +
  xlab("Duration (seconds)") +
  xlim(0, 6000)

p2 <- 
  ggplot(dat, aes(x = distance)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 50) +
  ylab(NULL) +
  xlab("Distance (km)") +
  xlim(0, 100)

p3 <- 
  ggplot(dat, aes(x = avg_speed)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 50) +
  ylab(NULL) +
  xlab("Average speed (km/h)") +
  xlim(0, 150)

p4 <- 
  ggplot(dat, aes(x = max_speed)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 50) +
  ylab(NULL) +
  xlab("Maximum speed (km/h)") +
  xlim(0, 200)

p5 <- 
  ggplot(dat, aes(x = time_start)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 48) +
  ylab(NULL) +
  xlab("Time of day")

p6 <- 
  ggplot(dat, aes(x = nb_days_since_monday)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 12 * 7) +
  ylab(NULL) +
  scale_x_continuous(breaks = 1:7) +
  xlab("Time of week (days elapsed since Monday midnight)")

total <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, left = "Number of trips")

ggsave("figures/telematics_histograms.png", plot = total)


# ===============================================================================================================================

x <- 
  aug_trip_sample %>% 
  group_by(vin) %>% 
  summarise_at(c("duration", "distance", "avg_speed", "max_speed"), mean)

plot_1 <- 
  ggplot(x, aes(x = duration)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 50) +
  ylab(NULL) +
  xlab("Duration (seconds)") +
  xlim(0, 3000)

plot_2 <- 
  ggplot(x, aes(x = distance)) +
  geom_histogram(col = "white", alpha = 0.9, bins = 50) +
  ylab(NULL) +
  xlab("Distance (km)") +
  xlim(0, 50)











