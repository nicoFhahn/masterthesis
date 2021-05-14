# load the packages
library(ggplot2)
library(readr)
library(sf)
library(SpatialEpi)
# load the data
newest_numbers <- read_csv("eval_data/newest_numbers_germany_may2.csv")
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
newest_numbers <- merge(
  newest_numbers,
  germany_sf,
  by.x = "municipality_id",
  by.y = "Kennziffer"
)
newest_numbers <- st_as_sf(newest_numbers)
color_low <- "#20A4F3"
color_high <- "#FF206E"
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
plot_1
