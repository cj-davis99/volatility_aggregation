source("scripts/packages_and_seeds.r")

## We want to generate our temperature for each weather station
## Temperature will be a noisy form of the following function
base_plt <- ggplot() +
  xlim(0, 365) +
  geom_function(
    fun = function(x) -30 * cos(2 * pi * x / 365) + 50
  ) +
  xlab("Day") +
  ylab("Temperature (Fahrenheit)")

## Adding noise:
noise <- rnorm(365, sd = 7.5)
temp_data <- (-30 * cos(2 * pi * 0:364 / 365) + 50) + noise
noise_plt <- ggplot(
  data = data.frame(
    x = 1:365,
    y = temp_data
  ),
  aes(
    x = 1:365,
    y = temp_data
  )
) +
  xlim(0, 365) +
  geom_point() +
  geom_function(
    fun = function(x) -30 * cos(2 * pi * x / 365) + 50,
    color = "red"
  ) +
  xlab("Day") +
  ylab("Temperature (Fahrenheit)")

## Import weather station location data
ws_a <- read.csv("data/ws_data_A.csv")
ws_b <- read.csv("data/ws_data_B.csv")
ws_c <- read.csv("data/ws_data_C.csv")
ws_d <- read.csv("data/ws_data_D.csv")

ws_coordinates <- rbind(ws_a, ws_b, ws_c, ws_d)

num_weather_stations <- length(ws_coordinates["x"])

## Initialize a distance matrix
dist_matrix <- matrix(0, num_weather_stations, num_weather_stations)

## Compute the distance between every weather station:
