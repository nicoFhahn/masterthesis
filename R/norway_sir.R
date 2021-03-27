# load the packages
library(ggplot2)
library(leaflet)
library(leaflet.mapboxgl)
library(patchwork)
source("R/preprocess_norge.R")
color_low <- "#002FA7"
color_high <- "#F50039"
plot_1 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = sir)) +
  ggtitle(
    label = "Standardised incidence ratio",
    subtitle = "Norway"
  ) +
  scale_fill_gradient2(
    "SIR",
    low = color_low,
    high = color_high,
    midpoint = 1
  ) +
  theme_minimal()
newest_numbers_south <- newest_numbers[unlist(lapply(lapply(newest_numbers$geometry, st_bbox), function(x) x[4] <= 64)), ]
plot_2 <- ggplot(data = newest_numbers_south) +
  geom_sf(aes(fill = sir)) +
  scale_fill_gradient2(
    "SIR",
    low = color_low,
    high = color_high,
    midpoint = 1
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )
plot_1 +
  inset_element(
    plot_2,
    left = 0.4,
    bottom = 0.01,
    right = 1,
    top = 0.7
  )
plot_3 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = log10(sir))) +
  ggtitle(
    label = "Logarithmic standardised incidence ratio",
    subtitle = "Norway"
  ) +
  scale_fill_gradient2(
    "SIR",
    low = color_low,
    high = color_high,
    midpoint = 0
  ) +
  theme_minimal()
plot_4 <- ggplot(data = newest_numbers_south) +
  geom_sf(aes(fill = log10(sir))) +
  scale_fill_gradient2(
    "SIR",
    low = color_low,
    high = color_high,
    midpoint = 0
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )
plot_3 +
  inset_element(
    plot_4,
    left = 0.4,
    bottom = 0.01,
    right = 1,
    top = 0.7
  )


rc1 <- colorRampPalette(
  c(
    "#002FA7",
    "#0040E0",
    "#0A50FF",
    "#336DFF",
    "#5C8Aff",
    "#85A7FF",
    "#ADC5FF",
    "#D6E2FF",
    "#FFFFFF"
  ),
  space = "Lab"
)(10)
rc2 <- colorRampPalette(
  c(
    "#FFEBEF",
    "#FFC2D0",
    "#FF99B1",
    "#FF7092",
    "#FF4772",
    "#FF1F53",
    "#F50039"
  ),
  space = "Lab"
)(round(10 * range(newest_numbers$sir)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = newest_numbers$sir
)
leaflet() %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    data = newest_numbers,
    fillColor = ~ pal(sir),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    label = paste(
      "Kommune: ", newest_numbers$kommune_name, "<br>",
      "Number of infections: ", newest_numbers$value, "<br>",
      "Expected number of infections: ", round(newest_numbers$expected_count), "<br>",
      "SIR: ", round(newest_numbers$sir, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = newest_numbers,
    pal = pal,
    values = ~sir,
    title = "SIR"
  )
