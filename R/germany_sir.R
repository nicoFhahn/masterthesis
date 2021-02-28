# load the packages
library(data.table)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.mapboxgl)
library(patchwork)
library(readr)
library(reshape2)
library(sf)
library(SpatialEpi)
library(stringr)
# load the data
source("R/preprocess_germany.R")
color_low <- "#84dcc6"
color_high <- "#ef233c"
plot_1 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = sir)) +
  ggtitle(
    label = "Standardised incidence ratio",
    subtitle = "Germany"
  ) +
  scale_fill_gradient2(
    "SIR",
    low = color_low,
    high = color_high,
    midpoint = 1
  ) +
  theme_minimal()
newest_numbers_south <- newest_numbers[unlist(lapply(lapply(newest_numbers$geometry, st_bbox), function(x) x[4] <= 64)), ]
rc1 <- colorRampPalette(
  c(
    "#86e7b8",
    "#93ff96",
    "#b2ffa8",
    "#d0ffb7",
    "#f2f5de",
    "white"
  ),
  space = "Lab"
)(12)
rc2 <- colorRampPalette(
  c(
    "white",
    "#fae0e4",
    "#f7cad0",
    "#f9bec7",
    "#fbb1bd",
    "#ff99ac",
    "#ff85a1",
    "#ff7096",
    "#ff5c8a",
    "#ff477e",
    "#ff0a54"
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
      "Kommune: ", newest_numbers$Landkreis, "<br>",
      "Number of infections: ", newest_numbers$CumNumberTestedIll, "<br>",
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
