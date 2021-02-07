# load the packages
library(data.table)
library(dplyr)
library(ggplot2)
library(leaflet)
library(patchwork)
library(readr)
library(reshape2)
library(sf)
library(SpatialEpi)
library(stringr)
# load the data
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
norway_municipality_confirmed_long <- melt(
  setDT(norway_municipality_confirmed),
  id.vars = colnames(norway_municipality_confirmed)[1:6],
  variable.name = "date"
)
norway_municipality_confirmed_long$date <- as.Date(as.character(norway_municipality_confirmed_long$date))
newest_numbers <- norway_municipality_confirmed_long[norway_municipality_confirmed_long$date == max(norway_municipality_confirmed_long$date), ]
newest_numbers <- merge(
  newest_numbers,
  norge_sf,
  by = "kommune_no"
)
expected_count <- expected(
  population = newest_numbers$population,
  cases = newest_numbers$value,
  n.strata = 1
)
newest_numbers$expected_count <- expected_count
# calculate the SIR
newest_numbers$sir <- newest_numbers$value / newest_numbers$expected_count
newest_numbers <- st_as_sf(newest_numbers)
color_low <- "#84dcc6"
color_high <- "#ef233c"
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
newest_numbers_south <- newest_numbers[unlist(lapply(lapply(newest_numbers$geometry, st_bbox), function(x) x[4] <= 64)),]
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
)(10)
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
)(50)
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
    fillColor = ~pal(sir),
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
norge_features <- read_csv("wrangled_data/norge_features.csv")