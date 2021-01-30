library(sf)
library(dplyr)
library(readr)
library(SpatialEpi)
library(stringr)
library(leaflet)
library(tmap)
library(ggplot2)
library(patchwork)
time <- Sys.time()
norge <- read_sf("wrangled_data/norge_complete.shp")
Sys.time() - time
newest_numbers <- norge[norge$date == max(norge$date), ]
newest_numbers <- newest_numbers[order(
  newest_numbers$kommn_n,
  newest_numbers$sex,
  newest_numbers$age
  ), ]
d <- newest_numbers %>%
  group_by(kommn_n) %>%
  summarise(
    count = sum(value)
  )
# 4602
expected_count <- expected(
  population = newest_numbers$popultn,
  cases = newest_numbers$value,
  n.strata = 12
)
d$expected_count <- expected_count
d$sir <- d$count / d$expected_count
tm_shape(d) +
  tm_polygons(
    col = "sir",
    palette = "viridis",
    title = "SIR"
  ) +
  tm_layout(
    title = "SIR in Norway"
  )

color_low <- "#84dcc6"
color_high <- "#ef233c"
plot_1 <- ggplot(data = d) +
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
d_south <- d[unlist(lapply(lapply(d$geometry, st_bbox), function(x) x[4] <= 64)),]
plot_2 <- ggplot(data = d_south) +
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
  domain = d$sir
)
d$kmmn_nm <- unique(newest_numbers$kmmn_nm)
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = d,
    fillColor = ~pal(sir),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    label = paste(
      "Kommune: ", d$kmmn_nm, "<br>",
      "Number of infections: ", d$count, "<br>",
      "Expected number of infections: ", round(d$expected_count), "<br>",
      "SIR: ", round(d$sir, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = d,
    pal = pal,
    values = ~sir,
    title = "SIR"
  )
