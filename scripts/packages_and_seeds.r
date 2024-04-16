## Necessary packages
library(ggplot2)
library(tidyverse)

## Setting a random seed
set.seed(2024)

## Euclidian distance metric on the unit sphere
## x and y should be numerical vectors of the same length
d <- function(x, y) {
  dist <- 0
  ## Compute norm of x and y
  norm_x <- 0
  norm_y <- 0
  for (i in seq_along(x)) {
    norm_x <- norm_x + x[i]^2
    norm_y <- norm_y + y[i]^2
  }
  norm_x <- sqrt(norm_x)
  norm_y <- sqrt(norm_y)

  for (i in seq_along(x)) {
    dist <- dist + (x[i] / norm_x - y[i] / norm_y)^2
  }

  return(sqrt(as.numeric(dist)))
}

## Generate random temperature data
## Takes a potential vector input of means and two scalar sd inputs
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