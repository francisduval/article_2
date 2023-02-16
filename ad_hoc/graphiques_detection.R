library(tidyverse)
library(ggrepel)
library(mvtnorm)
library(mlbench)
library(dbscan)
library(solitude)
theme_set(theme_bw())

set.seed(1994)
sigma1 <- matrix(c(4, 0, 0, 3), ncol = 2)
sigma2 <- matrix(c(4, 0, 0, 3), ncol = 2)
sigma3 <- matrix(c(0.2, 0, 0, 0.3), ncol = 2)
x1 <- rmvnorm(n = 5000, mean = c(0, 0), sigma = sigma1)
x2 <- rmvnorm(n = 20, mean = c(0, 0), sigma = sigma2)
x3 <- rmvnorm(n = 3, mean = c(0, 0), sigma = sigma3)


dat1 <- 
  as_tibble(x1) %>% 
  rename(x = V1, y = V2) %>% 
  filter(x ^ 2 + y ^ 2 < 1, x ^ 2 + y ^ 2 > 0.6)

dat2 <- 
  as_tibble(x2) %>% 
  rename(x = V1, y = V2) %>% 
  filter(x ^ 2 + y ^ 2 > 1, x ^ 2 + y ^ 2 < 3)

dat3 <- 
  as_tibble(x3) %>% 
  rename(x = V1, y = V2)

dat <- rbind(dat1, dat2, dat3)

centroid <- 
  dat %>% 
  summarise(x = mean(x), y = mean(y))

maha <- mahalanobis(as.matrix(dat), center = colMeans(dat), cov = cov(dat))
lof <- lof(dat, minPts = 10)

compute_if <- function(data, sample_size) {
  iso = isolationForest$new(sample_size = sample_size)
  iso$fit(data)
  scores <- iso$predict(data)
  return(scores$anomaly_score)
}

isfo <- compute_if(dat, sample_size = 30)

maha_norm <- (maha - min(maha)) / (max(maha) - min(maha))
lof_norm <- (lof - min(lof)) / (max(lof) - min(lof))
isfo_norm <- (isfo - min(isfo)) / (max(isfo) - min(isfo))

dat$maha <- maha
dat$lof <- lof
dat$isfo <- isfo

dat$maha_norm <- maha_norm
dat$lof_norm <- lof_norm
dat$isfo_norm <- isfo_norm


trans <- function(x) {
  exp(x) ^ 2.5
}

theme_transparent <- 
  theme(
    rect = element_rect(colour = "black"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
    axis.line = element_line(colour = "black"), # adding a black line for x and y axis
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    title = element_text(colour = "black"),
    panel.border = element_rect(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    text = element_text(size = 14)
  )

p1 <- 
  ggplot(dat) +
  geom_point(aes(x = x, y = y), data = centroid, col = "black", shape = 10, size = 7, stroke = 1) +
  geom_text(aes(x = centroid$x, y = centroid$y, label = "Centroid"), vjust = -1.5, col = "black") +
  geom_point(aes(x = x, y = y), size = 0.1, col = "black") +
  geom_point(aes(x = x, y = y), size = trans(maha_norm), shape = 1, col = "#0855a0") +
  geom_text(aes(x = x, y = y, label = if_else(x ^ 2 + y ^ 2 > 0.99|x ^ 2 + y ^ 2 < 0.605, as.character(round(maha, 2)), "")), hjust = -0.5, vjust = 0, size = 4, col = "#0855a0") +
  xlab(expression(x[1])) +
  ylab(expression(x[2])) +
  labs(subtitle = "Mahalanobis Anomaly Scores") +
  theme_transparent

p2 <- 
  ggplot(dat) +
  geom_point(aes(x = x, y = y), size = 0.1, col = "black") +
  geom_point(aes(x = x, y = y), size = trans(lof_norm), shape = 1, col = "#0855a0") +
  geom_text(aes(x = x, y = y, label = if_else(x ^ 2 + y ^ 2 > 0.99|x ^ 2 + y ^ 2 < 0.605, as.character(round(lof, 2)), "")), hjust = -0.5, vjust = 0, size = 4, col = "#0855a0") +
  xlab(expression(x[1])) +
  ylab(expression(x[2])) +
  labs(subtitle = "Local Outlier Factor Anomaly Scores (k = 10)") +
  theme_transparent

p3 <- 
  ggplot(dat) +
  geom_point(aes(x = x, y = y), size = 0.1, col = "black") +
  geom_point(aes(x = x, y = y), size = trans(isfo_norm), shape = 1, col = "#0855a0") +
  geom_text(aes(x = x, y = y, label = if_else(x ^ 2 + y ^ 2 > 0.99|x ^ 2 + y ^ 2 < 0.605, as.character(round(isfo, 2)), "")), hjust = -0.5, vjust = 0, size = 4, col = "#0855a0") +
  xlab(expression(x[1])) +
  ylab(expression(x[2])) +
  labs(subtitle = "Isolation Forest Anomaly Scores") +
  theme_transparent


ggsave(p1, filename = "figures/maha_ex.png",  bg = "transparent", width = 12)
ggsave(p2, filename = "figures/lof_ex.png",  bg = "transparent", width = 12)
ggsave(p3, filename = "figures/if_ex.png",  bg = "transparent", width = 12)
