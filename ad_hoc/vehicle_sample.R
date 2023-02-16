library(targets)
library(tidyverse)
library(here)
library(glue)
library(Cairo)

theme_set(theme_bw())

# ===============================================================================================================================

aug_trip_sample_train <- tar_read(aug_trip_sample_train)
set.seed(1994)
vins <- sample(unique(aug_trip_sample_train$vin), size = 400)
sample <- filter(aug_trip_sample_train, vin %in% vins)

new_driver <- function(data) {
  driver <- list(vin = data$vin[1], data = data)
  
  class(driver) <- c("driver", class(driver))
  return(driver)
}

drivers_ls <- 
  sample %>% 
  group_split(vin) %>% 
  map(new_driver)

# ===============================================================================================================================

plot.driver <- function(driver) {
  p1 <- 
    ggplot(driver$data, aes(x = duration)) +
    geom_histogram(col = "white", alpha = 0.9, bins = 50) +
    ylab(NULL) +
    xlab("Duration (seconds)") +
    xlim(0, 6000)
  
  p2 <- 
    ggplot(driver$data, aes(x = distance)) +
    geom_histogram(col = "white", alpha = 0.9, bins = 50) +
    ylab(NULL) +
    xlab("Distance (km)") +
    xlim(0, 100)
  
  p3 <- 
    ggplot(driver$data, aes(x = avg_speed)) +
    geom_histogram(col = "white", alpha = 0.9, bins = 50) +
    ylab(NULL) +
    xlab("Average speed (km/h)") +
    xlim(0, 150)
  
  p4 <- 
    ggplot(driver$data, aes(x = max_speed)) +
    geom_histogram(col = "white", alpha = 0.9, bins = 50) +
    ylab(NULL) +
    xlab("Maximum speed (km/h)") +
    xlim(0, 200)
  
  p5 <- 
    ggplot(driver$data, aes(x = time_start)) +
    geom_histogram(col = "white", alpha = 0.9, bins = 48) +
    ylab(NULL) +
    xlab("Time of day") +
    scale_x_time(breaks = seq(0, 86400, by = 14400), limits = c(0, 86400))
  
  p6 <- 
    ggplot(driver$data, aes(x = nb_days_since_monday)) +
    geom_histogram(col = "white", alpha = 0.9, bins = 12 * 7) +
    ylab(NULL) +
    scale_x_continuous(breaks = 1:7) +
    xlab("Time of week (days elapsed since Monday midnight)")
  
  total <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, left = "Number of trips")
}

# ===============================================================================================================================

driver_plots <- map(drivers_ls, plot)
map2(driver_plots, map_chr(drivers_ls, "vin"), ~ ggsave(glue("figures/drivers/{.y}.png"), plot = .x, width = 12))
