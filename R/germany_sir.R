# this is the script used to create the SIR plots for germany
library(ggplot2)
library(readr)
library(sf)
# load the data
source("R/preprocess_germany.R")
# set the colours
color_low <- "#20A4F3"
color_high <- "#FF206E"
# plot
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
