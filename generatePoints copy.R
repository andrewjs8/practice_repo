#' Random Coordinate Generator
#'
#' @description
#' Generate random x and y coordinates within user-specified bounds
#'
#' @param n Integer giving the number of points to generate
#' @param xmin Numeric lower bound for x
#' @param xmax Numeric upper bound for x
#' @param ymin Numeric lower bound for y
#' @param ymax Numeric upper bound for y
#'
#' @returns A data frame with n rows and two columns named x and y
#'
#' @details
#' Randomly generates x and y coordinates independently within the given bounds.
#' Each row represents one point inside the rectangle

## import the tidyverse library which also contains the ggplot2 library
library(tidyverse)
library(patchwork)
## create a function that returns random x and y coordinates
generatePoints <- function(n, xmin, xmax, ymin, ymax) {
  tibble(
    x = runif(n, min = xmin, max = xmax),
    y = runif(n, min = ymin, max = ymax)
  )
}

## generate 1,000 points within the assigned bounds
points <- generatePoints(
  n = 10000,
  xmin = -4,
  xmax = 4,
  ymin = 0,
  ymax = 0.4
)

## flag points that are on or below the normal curve
flaggedPoints <- points |>
  mutate(
    below = y <= dnorm(x, mean = 0, sd = 1)
  )

## estimate the integral by multiplying the proportion below
## the curve by the rectangle's area
rectangleArea <- (4 - (-4)) * (0.4 - 0)
proportion_below <- mean(flaggedPoints$below)
estimated_area <- proportion_below * rectangleArea

print(estimated_area)

## plot the points and normal curve within the assigned bounds
plot10000 <- ggplot(flaggedPoints, aes(x = x, y = y)) +
  geom_point(aes(color = below), alpha = 0.5) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    color = "black",
    linewidth = 1
  ) +
  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, 0.4)
  ) +
  ## add labels and report the sample size and estimated integral
  labs(
    title = "Monte Carlo Integration of the Normal Curve",
    subtitle = paste(
      "Number of points:", nrow(flaggedPoints),
      "| Estimated integral:", round(estimated_area, 4)
    ),
    x = "x",
    y = "Density",
    color = "On or below curve"
  ) +
  theme_bw()

plot10 + plot100 + plot1000 + plot10000
