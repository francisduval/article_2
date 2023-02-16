library(ggplot2)
library(tidyverse)
library(pracma)
library(plotly)
library(patchwork)
theme_set(theme_bw())

circle <- function(x, y) {x ^ 2 + y ^ 2 - 1}
rhombus <- function(x, y) {abs(x) + abs(y) - 1}
elastic <- function(x, y) {(1 - 0.5) * (x ^ 2 + y ^ 2) + 0.5 * (abs(x) + abs(y)) - 1}


make_shape_data <- function(f) {
  x1 <- seq(-1, 1, 0.01)
  y1 <- seq(-1, 1, 0.01)
  
  xy <- meshgrid(x1, y1)
  x <- xy$X
  y <- xy$Y
  
  fh <- f(x, y)
  mtrx2d <-  expand.grid(x1, y1)
  isoc <- as.data.frame(matrix(fh, nrow = prod(dim(fh))))
  
  
  consol <- cbind.data.frame(mtrx2d, isoc)
  names(consol) <- c("x", "y", "z")
  
  return(consol)
}


circle_dat <- make_shape_data(circle)
rhombus_dat <- make_shape_data(rhombus)
elastic_dat <- make_shape_data(elastic)

p1 <- 
  ggplot(circle_dat, aes(x = x, y = y, z = z)) +
  geom_contour(bins = 2, col = "black") +
  coord_fixed(ratio = 1) +
  xlab(~ paste(beta[1])) +
  ylab(~ paste(beta[2])) +
  labs(subtitle = "Ridge")

p2 <- 
  ggplot(rhombus_dat, aes(x = x, y = y, z = z)) +
  geom_contour(bins = 2, col = "black") +
  coord_fixed(ratio = 1) +
  xlab(~ paste(beta[1])) +
  ylab(~ paste(beta[2])) +
  labs(subtitle = "Lasso")

p3 <- 
  ggplot(elastic_dat, aes(x = x, y = y, z = z)) +
  geom_contour(bins = 2, col = "black") +
  coord_fixed(ratio = 1) +
  xlab(~ paste(beta[1])) +
  ylab(~ paste(beta[2])) +
  labs(subtitle = "Elastic-net")

p1 + p2 + p3
