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

## Generating noisy data:
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

## Import weather station location data
ws_a <- read.csv("data/ws_loc_A.csv")
ws_b <- read.csv("data/ws_loc_B.csv")
ws_c <- read.csv("data/ws_loc_C.csv")
ws_d <- read.csv("data/ws_loc_D.csv")

ws_coordinates <- rbind(ws_a, ws_b, ws_c, ws_d)

num_weather_stations <- length(ws_coordinates[, "x"])

## Initialize a distance matrix
dist <- c()

## Compute the distance between every weather station
## (this may take a couple minutes):
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

## We will cluster weather stations and give them correlated temperatures
## Choose a random weather station to be the center of the cluster from each
## region:
centroid_a <- sample(1:length(ws_a$x), 1)
centroid_b <- sample(1:length(ws_b$x), 1) + length(ws_a$x)
centroid_c <- sample(1:length(ws_c$x), 1) + length(ws_a$x) +
  length(ws_b$x)
centroid_d <- sample(1:length(ws_d$x), 1) + length(ws_a$x) +
  length(ws_b$x) + length(ws_c$x)

centroids <- c(centroid_a, centroid_b, centroid_c, centroid_d)

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

  if (!(i %in% centroids)) {
    temp_matrix[i, ] <- temperature_data(
      mean = temp_matrix[closest_centroid, ],
      winter_sd = 2.5 * dist_matrix[i, closest_centroid],
      summer_sd = 2 * dist_matrix[i, closest_centroid]
    )
  }
}

## Now we just need to save the temperature data
final_table_a <- cbind(ws_a, temp_matrix[1:length(ws_a$x), ])
final_table_b <- cbind(
  ws_b,
  temp_matrix[(1 + length(ws_a$x)):(length(ws_a$x) + length(ws_b$x)), ]
)
final_table_c <- cbind(
  ws_c,
  temp_matrix[(1 + length(ws_a$x) + length(ws_b$x)):
                (length(ws_a$x) + length(ws_b$x) + length(ws_c$x)), ]
)
final_table_d <- cbind(
  ws_d,
  temp_matrix[seq(
    from = 1 + length(ws_a$x) + length(ws_b$x) + length(ws_c$x),
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
