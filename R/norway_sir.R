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
library(tmap)
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
norge_demo <- read_delim("norge_data/Personer1.csv", ";")
norge_demo$age <- as.numeric(str_extract(norge_demo$age, "[0-9]{1,}"))
norge_demo$kommune_no <- str_extract(norge_demo$region, "[0-9]{4}")
newest_numbers <- merge(
  newest_numbers,
  norge_demo,
  by = "kommune_no"
)
# order them
newest_numbers <- newest_numbers[order(
  newest_numbers$kommune_no,
  newest_numbers$sex,
  newest_numbers$age
), ]
# group them
d <- newest_numbers %>%
  group_by(kommune_no) %>%
  summarise(
    count = sum(value)
  )
# calculate the expected count
expected_count <- expected(
  population = newest_numbers$population,
  cases = newest_numbers$value,
  n.strata = 212
)
d$expected_count <- expected_count
# calculate the SIR
d$sir <- d$count / d$expected_count
color_low <- "#84dcc6"
color_high <- "#ef233c"
d <- st_as_sf(merge(
  d,
  norge_sf,
  by = "kommune_no"
))
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
d$kommune_no <- unique(newest_numbers$kommune_no)
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = d,
    fillColor = ~pal(sir),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    label = paste(
      "Kommune: ", d$kommune_no, "<br>",
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
norge_features <- read_csv("wrangled_data/norge_features.csv")
d$geometry <- NULL
norge_features <- merge(
  norge_features,
  d,
  by = "kommune_no"
)
write_csv(norge_features, "wrangled_data/norge_features.csv")