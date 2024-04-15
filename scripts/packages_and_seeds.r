## Necessary packages
library(ggplot2)
library(tidyverse)

## Setting a random seed
set.seed(2024)

## Euclidian distance metric
## x and y should be numerical vectors of the same length
d <- function(x, y) {
  return(x - y)^2
}