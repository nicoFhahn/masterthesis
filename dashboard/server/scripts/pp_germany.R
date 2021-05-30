library(data.table)
library(dplyr)
library(readr)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
library(covid19germany)
library(regclass)
library(MASS)
# download the newest RKI data
rki <- get_RKI_timeseries()
# group it after municipality
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
germany_confirmed[
  str_detect(germany_confirmed$Landkreis, "Berlin"),
]$Landkreis <- "SK Berlin"
germany_confirmed[
  str_detect(germany_confirmed$Landkreis, "Berlin"),
]$IdLandkreis <- 11000
# create a berlin frame
berlin <- germany_confirmed[germany_confirmed$Landkreis == "SK Berlin", ]
# sum the numbers by date
berlin_grouped <- berlin %>%
  group_by(
    Date
  ) %>%
  summarise(
    Landkreis = "SK Berlin",
    NumberNewTestedIll = sum(NumberNewTestedIll),
    NumberNewDead = sum(NumberNewDead),
    NumberNewRecovered = sum(NumberNewRecovered),
    CumNumberTestedIll = sum(CumNumberTestedIll),
    CumNumberDead = sum(CumNumberDead),
    CumNumberRecovered = sum(CumNumberRecovered),
    IdLandkreis = 11000
  )
berlin_grouped <- berlin_grouped[order(berlin_grouped$Date), ]
# calculate the correct cumulative numbers
for (i in 2:nrow(berlin_grouped)) {
  berlin_grouped[i, ]$CumNumberTestedIll <- berlin_grouped[
    i - 1,
  ]$CumNumberTestedIll +
    berlin_grouped[
      i,
    ]$NumberNewTestedIll
  berlin_grouped[i, ]$CumNumberDead <- berlin_grouped[
    i - 1,
  ]$CumNumberDead +
    berlin_grouped[
      i,
    ]$NumberNewDead
  berlin_grouped[i, ]$CumNumberRecovered <- berlin_grouped[
    i - 1,
  ]$CumNumberRecovered +
    berlin_grouped[
      i,
    ]$NumberNewRecovered
}
# remove the old data from the frame
germany_confirmed <- germany_confirmed[
  germany_confirmed$Landkreis != "SK Berlin",
]
# attach the new data
germany_confirmed <- germany_confirmed[, c(1:5, 9:11, 15)]
germany_confirmed <- rbind(germany_confirmed, berlin_grouped)
germany_confirmed_split <- split(germany_confirmed, germany_confirmed$Date)
# create data frames for the missing dates
germany_confirmed_split <- lapply(
  germany_confirmed_split,
  function(x, ...) {
    missing <- unique(
      germany_confirmed$Landkreis
    )[
      !unique(
        germany_confirmed$Landkreis
      ) %in% unique(x$Landkreis)
    ]
    missing_tib <- tibble(
      Landkreis = missing,
      Date = rep(unique(x$Date), length(missing)),
      NumberNewTestedIll = rep(0, length(missing)),
      NumberNewDead = rep(NA, length(missing)),
      NumberNewRecovered = rep(NA, length(missing)),
      CumNumberTestedIll = rep(NA, length(missing)),
      CumNumberDead = rep(NA, length(missing)),
      CumNumberRecovered = rep(NA, length(missing)),
      Kennziffer = unique(
        germany_confirmed[
          germany_confirmed$Landkreis %in% missing,
        ]$Kennziffer
      )
    )
    tib <- rbind(x, missing_tib)
    tib[order(tib$Landkreis), ]
  }
)
# bind it together
germany_confirmed <- bind_rows(germany_confirmed_split)
# now split it by the municipality
germany_confirmed_split <- split(
  germany_confirmed,
  germany_confirmed$Landkreis
)
# now add the missing numbers
germany_confirmed_split <- lapply(
  germany_confirmed_split,
  function(x) {
    if (is.na(x$CumNumberTestedIll[1])) {
      ind_1 <- min(which(is.na(x$CumNumberTestedIll) %in% FALSE)) - 1
      x[1:ind_1, 3:8] <- 0
    }
    ind_2 <- which(x$CumNumberTestedIll %in% NA)
    x[ind_2, 4:5] <- 0
    x[ind_2, 6:8] <- x[ind_2 - 1, 6:8]
    ind_3 <- which(x$CumNumberTestedIll %in% NA)
    while (length(ind_3) > 0) {
      x[ind_3, 6:8] <- x[ind_3 - 1, 6:8]
      ind_3 <- which(x$CumNumberTestedIll %in% NA)
    }
    x
  }
)
germany_confirmed <- bind_rows(germany_confirmed_split)
germany_munc_long <- germany_confirmed
# load the previously generated file
newest_numbers_germany <- read_csv("server/data/germany_features.csv")
germany_sf <- read_sf("server/data/shapes_germany.shp")
colnames(germany_sf)[1] <- "municipality_id"
# merge it together
newest_numbers_germany <- merge(
  newest_numbers_germany,
  germany_sf,
  by = "municipality_id"
)
# get the newest date with at least 396 observations
date_1 <- rev(
  names(table(germany_confirmed$Date)[table(germany_confirmed$Date) > 400])
)[1]
germany_confirmed <- germany_confirmed[germany_confirmed$Date == date_1, ]
# if there is newer data available create new feature dataset
germany_confirmed$MovingCorrectionDead <- NULL
germany_confirmed$MovingCorrectionRecovered <- NULL
germany_confirmed$MovingCorrectionTestedIll <- NULL
germany_confirmed$CumMovingCorrectionDead <- NULL
germany_confirmed$CumMovingCorrectionRecovered <- NULL
germany_confirmed$CumMovingCorrectionTestedIll <- NULL
# the data for berlin needs to be manually grouped
# change the municipality name and ID to the same for all berlin cases
colnames(germany_confirmed)[9] <- "municipality_id"
newest_numbers_germany <- merge(
  newest_numbers_germany,
  germany_confirmed,
  "municipality_id"
)
newest_numbers_germany$value <- NULL
newest_numbers_germany$Date.x <- NULL
newest_numbers_germany$CumNumberRecovered <- NULL
newest_numbers_germany$CumNumberDead <- NULL
newest_numbers_germany$NumberNewRecovered <- NULL
newest_numbers_germany$NumberNewDead <- NULL
newest_numbers_germany$Landkreis <- NULL
colnames(newest_numbers_germany)[43] <- "date"
expected_count <- expected(
  population = newest_numbers_germany$population,
  cases = newest_numbers_germany$CumNumberTestedIll,
  n.strata = 1
)
# add new variables
newest_numbers_germany$expected_count <- expected_count
# calculate the SIR
newest_numbers_germany$sir <- newest_numbers_germany$CumNumberTestedIll / newest_numbers_germany$expected_count
newest_numbers_germany$sir <- round(newest_numbers_germany$sir, 2)
newest_numbers_germany$expected_count <- round(expected_count, 2)
newest_numbers_germany <- st_as_sf(newest_numbers_germany)
newest_numbers_germany$pop_dens <- round(newest_numbers_germany$pop_dens)
newest_numbers_germany$urb_dens <- round(newest_numbers_germany$urb_dens)
st_crs(newest_numbers_germany) <- 4326
# calculate the proportion of infected people
newest_numbers_germany$inf_rate <- round(newest_numbers_germany$CumNumberTestedIll / newest_numbers_germany$population, 2)
rownames(newest_numbers_germany) <- NULL
colnames(newest_numbers_germany)[44] <- "infections"
germany_long <- germany_munc_long %>%
  group_by(Date) %>%
  summarise(
    Landkreis = "Germany",
    NumberNewTestedIll = sum(NumberNewTestedIll),
    NumberNewDead = sum(NumberNewDead),
    NumberNewRecovered = sum(NumberNewRecovered),
    CumNumberTestedIll = sum(CumNumberTestedIll),
    CumNumberDead = sum(CumNumberDead),
    CumNumberRecovered = sum(CumNumberRecovered),
    IdLandkreis = "0000"
  )
germany_munc_long <- rbind(germany_long, germany_munc_long)
germany_munc_long$population <- 0
germany_munc_long[germany_munc_long$Landkreis == "Germany", ]$population <- sum(newest_numbers_germany$population)
for (i in seq_len(401)) {
  germany_munc_long[
    germany_munc_long$Landkreis == newest_numbers_germany$municipality[i],
  ]$population <- newest_numbers_germany$population[i]
}
colnames(germany_munc_long)[6] <- "value"
germany_munc_long$NumberNewTestedIll <- NULL
germany_munc_long$NumberNewDead <- NULL
germany_munc_long$NumberNewRecovered <- NULL
germany_munc_long$CumNumberDead <- NULL
germany_munc_long$CumNumberRecovered <- NULL
germany_munc_long$value_100k <- 100000 *
  (germany_munc_long$value / germany_munc_long$population)
germany_munc_long <- germany_munc_long[
  order(germany_munc_long$Date, germany_munc_long$Landkreis),
]
germany_munc_long$value_daily <- germany_munc_long$value -
  c(rep(0, 402), germany_munc_long$value[1:(nrow(germany_munc_long) - 402)])
germany_munc_long$value_daily_100k <- 100000 *
  (germany_munc_long$value_daily / germany_munc_long$population)
germany_munc_long$value_seven <- germany_munc_long$value -
  c(rep(0, 402 * 7), germany_munc_long$value[1:(nrow(germany_munc_long) - (402 * 7))])
germany_munc_long$incidence_seven <- 100000 *
  (germany_munc_long$value_seven / germany_munc_long$population)
germany_munc_long$value_100k <- round(germany_munc_long$value_100k, 1)
germany_munc_long$value_daily_100k <- round(germany_munc_long$value_daily_100k, 1)
germany_munc_long$incidence_seven <- round(germany_munc_long$incidence_seven, 1)
colnames_germany_actual <- c(
  "aerodrome", "area", "asyl_benefits", "bakeries", "clinic", "entertainment",
  "expected_count", "sex", "hairdresser", "higher_education", "kindergarten",
  "income_tax", "income_total", "trade_tax", "marketplace", "infections", "nursing_home", "office",
  "place_of_worship", "population", "pop_dens", "inf_rate", "protection_seekers",
  "platform", "residential", "restaurant", "retail", "schools", "shops", "sir",
  "sport", "unemployed_total", "unemployed_foreigners", "urb_dens", "afd", "Union",
  "FDP", "SPD", "Gruene", "die_linke", "welfare_recipients"
)
colnames_germany_nice <- sort(c(
  "Population", "Asylum benefits", "Log trade tax", "Log total income", "Log income tax",
  "Vote for die Union", "Vote for SPD", "Vote for the Greens", "Vote for FDP", "Vote for the Left",
  "Vote for AfD", "Protection seekers", "Welfare recipients", "Total unemployment", "Unemployed foreigners",
  "Marketplaces", "Entertainment venues", "Sports facilities", "Clinics", "Hairdressers", "Shops", "Places of worship",
  "Retail stores", "Nursing homes", "Restaurants", "Aerodromes", "Offices",
  "Public transport platforms", "Kindergartens", "Schools", "Bakeries", "Residential buildings",
  "Higher education", "Population density", "Urban density", "Female to male ratio",
  "Number of infections", "Expected count", "SIR", "Proportion of infected", "Area"
))
rm(list = setdiff(ls(), c(
  "newest_numbers_germany", "germany_munc_long",
  "newest_numbers_norway", "norway_munc_conf_long",
  "colnames_germany_actual", "colnames_germany_nice",
  "colnames_norway_actual", "colnames_norway_nice",
  "ts_europe", "ts_europe_unscaled",
  "colnames_europe_actual", "colnames_europe_nice"
)))
