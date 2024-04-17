source("scripts/packages_and_seeds.r")

## =================================================================
## In this script:
## We will construct a square region and split the region into FOUR
## states (scripts do not generalize to a different sized split).
## PREREQUISITE: NA
## =================================================================

## =================================================================
## Data generation
## =================================================================
## Generating region shares
## This is useful for custom aggregation schemes
shares <- runif(4)
shares <- shares / sum(shares)

## Generating region geometry:
## Region is loosely based on Iowa, Illinois, Nebraska, and Minnesota
## Iowa is approximately 56,273 square miles
## Illinois is approximatley 57,915 square miles
## Nebraska is approximately 77,348 square miles
## Minnesota is approximately 86,943 square miles
## Size of region CAN BE MODIFIED
total_sq_miles <- 56273 + 57915 + 77348 + 86943
region_radius <- sqrt(total_sq_miles)
region_intersect <- runif(2, min = 0, max = region_radius)


## Region A will be the top left of rectangle in the region
x_A <- c(0, region_intersect[1], region_intersect[1], 0)
y_A <- c(region_radius, region_radius, region_intersect[2], region_intersect[2])

## Region B will be the top right rectangle in the region
x_B <- c(region_intersect[1], region_radius, region_radius, region_intersect[1])
y_B <- c(region_radius, region_radius, region_intersect[2], region_intersect[2])

## Region C will be the bottom left rectangle in the region 
x_C <- c(0, region_intersect[1], region_intersect[1], 0)
y_C <- c(region_intersect[2], region_intersect[2], 0, 0)

## Region D will be the bottom right rectangle in the region
x_D <- c(region_intersect[1], region_radius, region_radius, region_intersect[1])
y_D <- c(region_intersect[2], region_intersect[2], 0, 0)

## =================================================================
## Plotting the region
## =================================================================
ids <- factor(c("A", "B", "C", "D"))
x <- c(x_A, x_B, x_C, x_D)
y <- c(y_A, y_B, y_C, y_D)

values <- data.frame(
  id = ids,
  value = shares
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = x,
  y = y
)

datapoly <- merge(values, positions, by = c("id"))

## To label the regions, I need the following coordinates
x_left_label <- region_intersect[1] / 2
y_top_label <- (region_radius + region_intersect[2]) / 2
x_right_label <- (region_intersect[1] + region_radius) / 2
y_bottom_label <- region_intersect[2] / 2

## Plotting
plt <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id)) +
  xlab("x (mi)") +
  ylab("y (mi)") +
  labs(fill = "Share of Production")  +
  geom_label(
    label = "A",
    x = x_left_label,
    y = y_top_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "#ffffff"
  ) +
  geom_label(
    label = "B",
    x = x_right_label,
    y = y_top_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "#ffffff"
  ) +
  geom_label(
    label = "C",
    x = x_left_label,
    y = y_bottom_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "#ffffff"
  ) +
  geom_label(
    label = "D",
    x = x_right_label,
    y = y_bottom_label,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill = "white"
  ) +
  geom_vline(
    xintercept = region_intersect[1],
    colour = "white",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = region_intersect[2],
    colour = "white",
    linewidth = 1
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

plt

## =================================================================
## Saving
## =================================================================
## Save plot
## This 'save' assumes that you're working directory is project dirctory
ggsave("region_map.pdf", plot = plt, path = "output/")

## Save necessary region information to a table
state_data <- data.frame(
  A = c(shares[1], x_A, y_A),
  B = c(shares[2], x_B, y_B),
  C = c(shares[3], x_C, y_C),
  D = c(shares[4], x_D, y_D),
  row.names = c("Share", "x_tl", "x_tr", "x_br", "x_bl",
                "y_tl", "y_tr", "y_br", "y_bl")
)
## This 'write' assumes that you're working directory is project dirctory
write.csv(state_data, "data/state_data.csv")
