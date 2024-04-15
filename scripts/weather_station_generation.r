source("scripts/packages_and_seeds.r")

## We want to randomly generate 200 weather station locations
num_weather_stations <- 200

## Load in the state data
state_data <- read.csv("data/state_data.csv") |>
  column_to_rownames(var = "X")

region_radius <- state_data["y_tl", "A"]
region_intersect <- c(state_data["x_br", "A"], state_data["y_br", "A"])

## Generate random (x,y) coordinates for weather stations (ws)
x_ws <- runif(num_weather_stations, max = region_radius)
y_ws <- runif(num_weather_stations, max = region_radius)

## Sort weather station coordinates
x_ws_A <- c()
x_ws_B <- c()
x_ws_C <- c()
x_ws_D <- c()
y_ws_A <- c()
y_ws_B <- c()
y_ws_C <- c()
y_ws_D <- c()

for (i in 1:num_weather_stations){
  if (x_ws[i] < region_intersect[1]) {
    ## Either in region A or C
    if (y_ws[i] < region_intersect[2]) {
      ## In region C
      x_ws_C <- c(x_ws_C, x_ws[i])
      y_ws_C <- c(y_ws_C, y_ws[i])
    } else {
      ## In region A
      x_ws_A <- c(x_ws_A, x_ws[i])
      y_ws_A <- c(y_ws_A, y_ws[i])
    }
  } else {
    ## Either in region B or D
    if (y_ws[i] < region_intersect[2]) {
      ## In region D
      x_ws_D <- c(x_ws_D, x_ws[i])
      y_ws_D <- c(y_ws_D, y_ws[i])
    } else {
      ## In region B
      x_ws_B <- c(x_ws_B, x_ws[i])
      y_ws_B <- c(y_ws_B, y_ws[i])
    }
  }
}

## Plot weather stations in regions
positions <- data.frame(
  x = c(state_data[2:5, "A"], state_data["x_tl", "A"], state_data[2:5, "B"],
        state_data[2:5, "C"], state_data["x_tl", "C"], state_data[2:5, "D"]),
  y = c(state_data[6:9, "A"], state_data["y_tl", "A"], state_data[6:9, "B"],
        state_data[6:9, "C"], state_data["y_tl", "C"], state_data[6:9, "D"])
)

x_left_label <- (state_data["x_tl", "A"] + state_data["x_tr", "A"]) / 2
x_right_label <- (state_data["x_tl", "B"] + state_data["x_tr", "B"]) / 2
y_top_label <- (state_data["y_tl", "A"] + state_data["y_bl", "A"]) / 2
y_bottom_label <- (state_data["y_tl", "C"] + state_data["y_bl", "C"]) / 2

plt <- ggplot(positions, aes(x = x, y = y)) +
  xlab("x (mi)") +
  ylab("y (mi)") +
  geom_path() +
  geom_point( ## Plotting weather stations in A
    color = "red",
    data = data.frame(
      x = x_ws_A,
      y = y_ws_A
    )
  ) +
  geom_point( ## Plotting weather stations in B
    color = "blue",
    data = data.frame(
      x = x_ws_B,
      y = y_ws_B
    )
  ) +
  geom_point( ## Plotting weather stations in C
    color = "green",
    data = data.frame(
      x = x_ws_C,
      y = y_ws_C
    )
  ) +
  geom_point( ## Plotting weather stations in D
    color = "orange",
    data = data.frame(
      x = x_ws_D,
      y = y_ws_D
    )
  ) +
  geom_label(
    label = "A",
    x = x_left_label,
    y = y_top_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "red"
  ) +
  geom_label(
    label = "B",
    x = x_right_label,
    y = y_top_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "white",
    fill = "blue"
  ) +
  geom_label(
    label = "C",
    x = x_left_label,
    y = y_bottom_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "green"
  ) +
  geom_label(
    label = "D",
    x = x_right_label,
    y = y_bottom_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "orange"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plt

## Save plot
## This 'save' assumes that you're working directory is project dirctory
ggsave("weather_station_map.pdf", plot = plt, path = "output/")

## Save the weather station location data
ws_data_A <- tibble(
  x = x_ws_A,
  y = y_ws_A,
)
write.csv(ws_data_A, "data/ws_data_A.csv", row.names = FALSE)
ws_data_B <- tibble(
  x = x_ws_B,
  y = y_ws_B,
)
write.csv(ws_data_B, "data/ws_data_B.csv", row.names = FALSE)
ws_data_C <- tibble(
  x = x_ws_C,
  y = y_ws_C,
)
write.csv(ws_data_C, "data/ws_data_C.csv", row.names = FALSE)
ws_data_D <- tibble(
  x = x_ws_D,
  y = y_ws_D,
)
write.csv(ws_data_D, "data/ws_data_D.csv", row.names = FALSE)
