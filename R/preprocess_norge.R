library(data.table)
library(readr)
library(sf)
library(SpatialEpi)
library(units)
#####################################################
# prepare the data
norge_features <- read_csv("wrangled_data/norge_features.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
norge <- merge(
  norge_features,
  norge_sf,
  by = "kommune_no"
)
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
newest_numbers$value <- NULL
newest_numbers$time <- NULL
newest_numbers$fylke_no <- NULL
newest_numbers$fylke_name <- NULL
newest_numbers$population <- NULL
newest_numbers$date <- NULL
newest_numbers$kommune_name <- NULL
newest_numbers <- merge(
  newest_numbers,
  norge[norge$date == max(norge$date), ],
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
st_crs(newest_numbers) <- 4326
# calculate the number of infected people
newest_numbers$inf_rate <- newest_numbers$value / newest_numbers$population
# add id area variables
newest_numbers$idarea_1 <- seq_len(nrow(newest_numbers))
newest_numbers$idarea_2 <- seq_len(nrow(newest_numbers))
# add the expected count
newest_numbers$area <- as.numeric(set_units(st_area(newest_numbers), km^2))
newest_numbers$pop_dens <- newest_numbers$population / newest_numbers$area
newest_numbers$urb_dens <- newest_numbers$residential / newest_numbers$area
newest_numbers$sex <- newest_numbers$population_female / newest_numbers$population_total
rm(list=setdiff(ls(), "newest_numbers"))
