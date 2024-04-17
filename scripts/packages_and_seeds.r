## Necessary packages
library(ggplot2)
library(tidyverse)

## Setting a random seed
set.seed(2024)

## =================================================================
## Custom functions
## =================================================================

## Euclidian distance metric on the unit sphere
## @PARAM: x is a n-dimensional numerical vector (must be the same length as y).
## @PARAM: y is an n-dimensional numerical vector (must be the same length as
##         x).
## @RET: The Euclidian distance of x and y when projected onto the unit sphere.
d <- function(x, y) {
  dist <- 0
  ## Compute (Euclidian) norm of x and y
  norm_x <- 0
  norm_y <- 0
  for (i in seq_along(x)) {
    norm_x <- norm_x + x[i]^2
    norm_y <- norm_y + y[i]^2
  }
  norm_x <- sqrt(norm_x)
  norm_y <- sqrt(norm_y)

  for (i in seq_along(x)) {
    ## Compute Euclidian distance of x and y when projected onto unit sphere
    dist <- dist + (x[i] / norm_x - y[i] / norm_y)^2
  }

  return(sqrt(as.numeric(dist)))
}

## Generate ONE YEAR of temperature data
## @PARAM: mean is an optional numerical vector of length 365. If left
##         unspecified, a mean of zero is assumed and the mean is a cosine
##         cosine function with amplitude and frequency adjusted to simulate
##         temperatures experienced in America throughout the year.
## @PARAM: winter_sd is the standard deviation experienced in the first quarter
##         and the last quarter of the year. A numerical value greater than zero
##         is expected.
## @PARAM: summer_sd is the standard deviation experienced in middle two
##         quarters of the year. A numerical value greater than zero
##         is expected.
## @RET: One year's worth of simulated temperature data; i.e., a 365-dimensional
##       vector.
temperature_data <- function(mean = rep(0, 365), winter_sd, summer_sd) {
  if (identical(mean, rep(0, 365))) {
    winter_noise <- rnorm(183, mean = 0, sd = winter_sd)
    summer_noise <- rnorm(182, mean = 0, sd = summer_sd)

    temp_data <- c((-30 * cos(2 * pi * 0:90 / 365) + 50) + winter_noise[1:91],
                   (-30 * cos(2 * pi * 91:181 / 365) + 50) + summer_noise[1:91],
                   (-30 * cos(2 * pi * 182:272 / 365) + 50) +
                     summer_noise[92:182],
                   (-30 * cos(2 * pi * 273:363 / 365) + 50) +
                     winter_noise[92:182],
                   (-30 * cos(2 * pi * 364 / 365) + 50) + winter_noise[183])

    return(temp_data)
  } else {
    winter_noise <- rnorm(183, mean = 0, sd = winter_sd)
    summer_noise <- rnorm(182, mean = 0, sd = summer_sd)

    temp_data <- c(mean[1:91] + winter_noise[1:91],
                   mean[92:182] + summer_noise[1:91],
                   mean[183:273] + summer_noise[92:182],
                   mean[274: 364] + winter_noise[92:182],
                   mean[365] + winter_noise[183])
    return(temp_data)
  }
}