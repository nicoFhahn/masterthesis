library(data.table)
library(readr)
library(dplyr)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
library(regclass)
library(MASS)
#####################################################
# load the newest data
norway_munc_conf <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
# turn it into long format
norway_munc_conf_long <- melt(
  setDT(norway_munc_conf),
  id.vars = colnames(norway_munc_conf)[1:6],
  variable.name = "date"
)
newest_numbers_norway <- read_csv("server/data/norge_features.csv")
norge_sf <- read_sf("server/data/shapes_norge.shp")
# merge it together
newest_numbers_norway <- merge(
  newest_numbers_norway,
  norge_sf,
  by = "kommune_no"
)
# get the newest numbers
norway_munc_conf_long$date <- as.Date(
  as.character(norway_munc_conf_long$date)
)
newest_numbers_norway_2 <- norway_munc_conf_long[
  norway_munc_conf_long$date == max(
    norway_munc_conf_long$date
  ),
]
# merge all together
newest_numbers_norway <- merge(
  newest_numbers_norway,
  newest_numbers_norway_2,
  by = "kommune_no"
)
colnames(newest_numbers_norway)[2:4] <- c(
  "kommune_name",
  "population",
  "date"
)
newest_numbers_norway[, 38:43] <- NULL
expected_count <- expected(
  population = newest_numbers_norway$population,
  cases = newest_numbers_norway$value,
  n.strata = 1
)
# add new variables
newest_numbers_norway$expected_count <- expected_count
# calculate the SIR
newest_numbers_norway$sir <- newest_numbers_norway$value / newest_numbers_norway$expected_count
newest_numbers_norway$sir <- round(newest_numbers_norway$sir, 1)
newest_numbers_norway$expected_count <- round(expected_count, 1)
newest_numbers_norway <- st_as_sf(newest_numbers_norway)
st_crs(newest_numbers_norway) <- 4326
# calculate the proportion of infected people
newest_numbers_norway$inf_rate <- round(newest_numbers_norway$value / newest_numbers_norway$population, 1)
rownames(newest_numbers_norway) <- NULL
colnames(newest_numbers_norway)[37] <- "infections"
norway_munc_conf_long <- norway_munc_conf_long[
  norway_munc_conf_long$kommune_no %in% newest_numbers_norway$kommune_no,
]
norway_long <- norway_munc_conf_long %>%
  group_by(
    date
  ) %>%
  summarise(
    time = norway_munc_conf_long$time[1],
    kommune_no = "0",
    kommune_name = "Norway",
    fylke_no = "0",
    fylke_name = "Norway",
    population = sum(population),
    value = sum(value)
  )
norway_munc_conf_long <- rbind(norway_munc_conf_long, norway_long)
norway_munc_conf_long$value_100k <- 100000 *
  (norway_munc_conf_long$value / norway_munc_conf_long$population)
norway_munc_conf_long <- norway_munc_conf_long[
  order(norway_munc_conf_long$date, norway_munc_conf_long$kommune_no),
]
norway_munc_conf_long$value_daily <- norway_munc_conf_long$value -
  c(rep(0, 357), norway_munc_conf_long$value[1:(nrow(norway_munc_conf_long) - 357)])
norway_munc_conf_long$value_daily_100k <- 100000 *
  (norway_munc_conf_long$value_daily / norway_munc_conf_long$population)
norway_munc_conf_long$value_seven <- norway_munc_conf_long$value -
  c(rep(0, 357 * 7), norway_munc_conf_long$value[1:(nrow(norway_munc_conf_long) - (357 * 7))])
norway_munc_conf_long$incidence_seven <- 100000 *
  (norway_munc_conf_long$value_seven / norway_munc_conf_long$population)
norway_munc_conf_long$value_100k <- round(norway_munc_conf_long$value_100k, 1)
norway_munc_conf_long$value_daily_100k <- round(norway_munc_conf_long$value_daily_100k, 1)
norway_munc_conf_long$incidence_seven <- round(norway_munc_conf_long$incidence_seven, 1)
rm(list = setdiff(ls(), c("newest_numbers_norway", "norway_munc_conf_long")))
