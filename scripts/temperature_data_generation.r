source("scripts/packages_and_seeds.r")

## =================================================================
## In this script:
## We will generate ONE YEAR of temperature for each weather station
## in our region and save the temperature data (paired with the
## weather station location data) to the data folder.
## PREREQUISITE: must have run:
##                region_generation.r
##                weather_station_generation.r
## =================================================================

## =================================================================
## Example Plots
## =================================================================
## The following plots help visualize the data generation process
## !!!
## None of this is necessary for data generation
## !!!
## Temperature will be a noisy form of the following function
base_plt <- ggplot() +
  xlim(0, 365) +
  geom_function(
    fun = function(x) -30 * cos(2 * pi * x / 365) + 50
  ) +
  xlab("Day") +
  ylab("Temperature (Fahrenheit)")

## Generating noisy data (example):
temp_data <- temperature_data(winter_sd = 7.5, summer_sd = 5)

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

## Save plots
## This 'save' assumes that you're working directory is project dirctory
ggsave("no_noise_temperature.pdf", plot = base_plt, path = "output/")
ggsave("noisy_temp_ex.pdf", plot = noise_plt, path = "output/")

## =================================================================
## Data generation
## =================================================================
## Import weather station location data
ws_a <- read.csv("data/ws_loc_A.csv")
num_ws_a <- length(ws_a$x)
ws_b <- read.csv("data/ws_loc_B.csv")
num_ws_b <- length(ws_b$x)
ws_c <- read.csv("data/ws_loc_C.csv")
num_ws_c <- length(ws_c$x)
ws_d <- read.csv("data/ws_loc_D.csv")
num_ws_d <- length(ws_d$x)

ws_coordinates <- rbind(ws_a, ws_b, ws_c, ws_d)

num_weather_stations <- length(ws_coordinates[, "x"])

## Initialize a distance matrix
dist <- c()

## Compute the distance between every weather station
## We need this for creating correlation between weather stations
## !!!
## (THIS MAY TAKE A COUPLE OF MINUTES):
## !!!
for (i in seq_along(ws_coordinates$x)) {
  for (j in seq_along(ws_coordinates$x)) {
    dist <- c(dist, as.numeric(d(ws_coordinates[i, c("x", "y")],
                                 ws_coordinates[j, c("x", "y")])))
  }
}

dist_matrix <- matrix(
  dist,
  nrow = num_weather_stations,
  ncol = num_weather_stations
)

## Now we generate temperature data for each weather station
temp_matrix <- matrix(
  0,
  nrow = num_weather_stations,
  ncol = 365
)

## We will cluster weather stations and give weather stations in the same
## cluster correlated temperatures.
## Centroid of clusters will be chosen at random. Remark that each centroid
## generated below will correspond to an index in ws_coordinates
centroid_a <- sample(1:num_ws_a, 1)
centroid_b <- sample(1:num_ws_b, 1) + num_ws_a
centroid_c <- sample(1:num_ws_c, 1) + num_ws_a +
  num_ws_b
centroid_d <- sample(1:num_ws_d, 1) + num_ws_a +
  num_ws_b + num_ws_c

centroids <- c(centroid_a, centroid_b, centroid_c, centroid_d)

## The standard deviations CAN BE MODIFIED
## Temperature in centroid_a
temp_matrix[centroid_a, ] <- temperature_data(winter_sd = 10, summer_sd = 3)

## Temperature in centroid_b
temp_matrix[centroid_b, ] <- temperature_data(winter_sd = 8, summer_sd = 5)

## Temperature in centroid_c
temp_matrix[centroid_c, ] <- temperature_data(winter_sd = 3, summer_sd = 8)

## Temperature in centroid_d
temp_matrix[centroid_d, ] <- temperature_data(winter_sd = 5, summer_sd = 5)

## Generate temperature data
for (i in 1:num_weather_stations) {
  ## Figure out which cluster weather station i belongs to
  closest_centroid <- centroid_a
  if (dist_matrix[i, closest_centroid] > dist_matrix[i, centroid_b]) {
    closest_centroid <- centroid_b
  }
  if (dist_matrix[i, closest_centroid] > dist_matrix[i, centroid_c]) {
    closest_centroid <- centroid_c
  }
  if (dist_matrix[i, closest_centroid] > dist_matrix[i, centroid_d]) {
    closest_centroid <- centroid_d
  }

  ## Ensure that we don't override centroid temperature data
  ## Note that base sd CAN BE MODIFIED (2.5 and 2 below)
  if (!(i %in% centroids)) {
    temp_matrix[i, ] <- temperature_data(
      mean = temp_matrix[closest_centroid, ],
      winter_sd = 2.5 * dist_matrix[i, closest_centroid],
      summer_sd = 2 * dist_matrix[i, closest_centroid]
    )
  }
}

## =================================================================
## Saving
## =================================================================
final_table_a <- cbind(ws_a, temp_matrix[1:num_ws_a, ])
final_table_b <- cbind(
  ws_b,
  temp_matrix[(1 + num_ws_a):(num_ws_a + num_ws_b), ]
)
final_table_c <- cbind(
  ws_c,
  temp_matrix[(1 + num_ws_a + num_ws_b):
                (num_ws_a + num_ws_b + num_ws_c), ]
)
final_table_d <- cbind(
  ws_d,
  temp_matrix[seq(
    from = 1 + num_ws_a + num_ws_b + num_ws_c,
    to = num_weather_stations
  ),
  ]
)

## Save all data files to csv
## This 'save' assumes that you're working directory is project dirctory
write.csv(final_table_a, "data/ws_data_A.csv", row.names = FALSE)
write.csv(final_table_b, "data/ws_data_B.csv", row.names = FALSE)
write.csv(final_table_c, "data/ws_data_C.csv", row.names = FALSE)
write.csv(final_table_d, "data/ws_data_D.csv", row.names = FALSE)
