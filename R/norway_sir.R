# load the packages
library(ggplot2)
library(patchwork)
source("R/preprocess_norge.R")
color_low <- "#20A4F3"
color_high <- "#FF206E"
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
newest_numbers_south <- newest_numbers[
  unlist(
    lapply(
      lapply(
        newest_numbers$geometry,
        st_bbox
      ),
      function(x) x[4] <= 64
    )
  ),
]
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
