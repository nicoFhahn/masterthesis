# this script is used for creating the timeseries data
library(covid19germany)
library(covidregionaldata)
library(data.table)
library(dplyr)
library(eurostat)
library(ISOcodes)
library(readr)
library(reshape2)
library(sf)
library(stringr)
library(SpatialEpi)
library(tibble)
# load the timeseries data and specify the col types
ts <- read_csv(
  "wrangled_data/timeseries_covid.csv",
  col_types = list(
    col_character(),
    col_character(),
    col_date(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_number(),
    col_factor(),
    col_factor(),
    col_number(),
    col_factor(),
    col_factor(),
    col_number(),
    col_factor(),
    col_factor(),
    col_factor(),
    col_factor(),
    col_factor(),
    col_factor(),
    col_factor(),
    col_factor(),
    col_number(),
    col_number(),
    col_factor(),
    col_factor()
  )
)
# only use the data for europe
ts_europe <- ts[ts$continent == "Europe", ]
# only use the data with a date available
ts_europe <- ts_europe[!is.na(ts_europe$Date), ]
# check for which dates the number of new cases is NA
contains_na <- which(ts_europe$new_cases %in% NA)
for (i in contains_na) {
  # if it is a day in between other days, add the previos days values
  if (ts_europe$Country[i - 1] == ts_europe$Country[i]) {
    ts_europe$total_cases[i] <- ts_europe$total_cases[i - 1]
    ts_europe$new_cases[i] <- 0
  } else {
    # else it is the first date for the country, so we add 0
    ts_europe$total_cases[i] <- 0
    ts_europe$new_cases[i] <- 0
  }
}
# remove these variables
ts_europe$handwashing_facilities <- NULL
ts_europe$new_tests <- NULL
ts_europe$extreme_poverty <- NULL
# add proportion of people aged 70 or older in serbia, according to
# https://www.stat.gov.rs/en-us/vizuelizacija/interaktivni-grafikoni/mapa/
ts_europe[ts_europe$Country == "Serbia", ]$aged_70_older <- 26.5
# https://tobacconomics.org/files/research/645/237-fact-sheet-nmk-stc-see-2019-v4-1.pdf
# these are the countries without mobility data
table(ts_europe[is.na(ts_europe$retail_and_recreation), ]$Country)
# remove these countries
ts_europe <- ts_europe[!is.na(ts_europe$retail_and_recreation), ]
# no data available for liechtenstein
ts_europe[ts_europe$Country == "Liechtenstein", ]$residential
# remove liechtenstein
ts_europe <- ts_europe[ts_europe$Country != "Liechtenstein", ]
# for each country where park mobility is missing, we assume a steady de-/incline between the missing dates
# first we get the ids for each country, then we impute the values
park_na <- which(is.na(ts_europe[ts_europe$Country == "Estonia", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na[1] - 1] -
  ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na[14] + 1]) / 15
ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na[1] - 1] -
  seq_len(14) * mean_decline
park_na <- which(is.na(ts_europe[ts_europe$Country == "Luxembourg", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na[1] - 1] -
  ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na[23] + 1]) / 24
ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na[1] - 1] -
  seq_len(23) * mean_decline
park_na <- which(is.na(ts_europe[ts_europe$Country == "Malta", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Malta", ]$parks[park_na[1] - 1] -
  ts_europe[ts_europe$Country == "Malta", ]$parks[park_na[4] + 1]) / 5
ts_europe[ts_europe$Country == "Malta", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Malta", ]$parks[park_na[1] - 1] -
  seq_len(4) * mean_decline
park_na <- which(is.na(ts_europe[ts_europe$Country == "Slovenia", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na[1] - 1] -
  ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na[10] + 1]) / 11
ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na[1] - 1] -
  seq_len(10) * mean_decline
# no data for north macedonia
ts_europe[is.na(ts_europe$stringency_index), ]$Country
# remove north macedonia
ts_europe <- ts_europe[ts_europe$Country != "North Macedonia", ]
# now we only have complete caes
ts_europe <- ts_europe[complete.cases(ts_europe), ]
# get the iso codes data
iso_code <- ISO_3166_1[, c("Alpha_2", "Name")]
# set the colnames
colnames(iso_code) <- c("CNTR_CODE", "Country")
# change country name for moldova
iso_code[str_detect(iso_code$Country, "Moldova"), ]$Country <- "Moldova"
# add the iso codes to the timeseries
ts_europe <- merge(
  ts_europe,
  iso_code,
  by = "Country"
)
# now get the case numbers for all countries, these are more accurate
cases <- get_national_data(unique(ts_europe$Country))
cases <- cases[, c("date", "cases_new", "iso_code")]
colnames(cases) <- c("Date", "cases_new", "CNTR_CODE")
# add this data to the timeseries
ts_europe <- merge(
  ts_europe,
  cases,
  by = c("Date", "CNTR_CODE")
)
# remove the old data
ts_europe$new_cases <- ts_europe$cases_new
ts_europe$cases_new <- NULL
# get the shapefiles of the european countries
europe_shapes <- get_eurostat_geospatial(nuts_level = "0")
# check what is missing
missing <- !unique(ts_europe$CNTR_CODE) %in% europe_shapes$CNTR_CODE
unique(ts_europe$Country)[missing]
unique(ts_europe$CNTR_CODE)[missing]
# set the correct iso codes
ts_europe$CNTR_CODE[ts_europe$CNTR_CODE == "GR"] <- "EL"
ts_europe$CNTR_CODE[ts_europe$CNTR_CODE == "GB"] <- "UK"
missing <- !unique(ts_europe$CNTR_CODE) %in% europe_shapes$CNTR_CODE
# check what is still missing
unique(ts_europe$Country)[missing]
unique(ts_europe$CNTR_CODE)[missing]
# load the shapefiles for each of this country and add them to the shape frame
# first belarues
bl <- read_sf("misc_files/blr_admbnda_adm0_UNICEF.shp")
europe_shapes <- rbind(europe_shapes, europe_shapes[1, ])
europe_shapes[38, c(1:2, 5:7)] <- "BY"
europe_shapes[38, 3] <- "Belarus"
europe_shapes[38, ]$geometry <- bl$geometry
# next bosnia herzegovina
bh <- read_sf("misc_files/BIH_adm0.shp")
bh <- st_transform(bh, st_crs(europe_shapes))
europe_shapes <- rbind(europe_shapes, europe_shapes[1, ])
europe_shapes[39, c(1:2, 5:7)] <- "BA"
europe_shapes[39, 3] <- "Bosnia and Herzegovina"
europe_shapes[39, ]$geometry <- bh$geometry
# finally moldova
md <- st_union(read_sf("misc_files/MDA_Admin1.shp"))
md <- st_transform(md, st_crs(europe_shapes))
europe_shapes <- rbind(europe_shapes, europe_shapes[1, ])
europe_shapes[40, c(1:2, 5:7)] <- "MD"
europe_shapes[40, 3] <- "Moldova"
europe_shapes[40, ]$geometry <- md
# load the population data
population <- read_csv("wrangled_data/demo_pjan_1_Data.csv")
# use year 2020
population <- population[population$TIME == 2020, ]
population$GEO[!population$GEO %in% unique(ts_europe$Country)]
unique(ts_europe$Country)[!unique(ts_europe$Country) %in% population$GEO]
# update the name for germany
population$GEO[10] <- "Germany"
population <- population[population$GEO %in% unique(ts_europe$Country), c("GEO", "Value")]
colnames(population)[1] <- "Country"
# add the population data
ts_europe <- merge(
  ts_europe,
  population,
  by = "Country"
)
# add the shapefiles
ts_europe <- merge(
  ts_europe,
  europe_shapes[, 2],
  by = "CNTR_CODE"
)
# add the missing population values  according to worldometers.info
# https://www.worldometers.info/world-population/belarus-population/
ts_europe[ts_europe$CNTR_CODE == "BY", ]$Value <- "9,446,609"
# https://www.worldometers.info/world-population/bosnia-and-herzegovina-population/
ts_europe[ts_europe$CNTR_CODE == "BA", ]$Value <- "3,263,180"
# https://www.worldometers.info/world-population/moldova-population/
ts_europe[ts_europe$CNTR_CODE == "MD", ]$Value <- "4,025,805"
# turn into sf frame
ts_europe <- st_as_sf(ts_europe)
colnames(ts_europe)[42] <- "population"
# turn population into numeric var
ts_europe$population <- as.numeric(str_replace_all(ts_europe$population, ",", ""))
# split the frame by country
ts_europe_split <- split(ts_europe, ts_europe$Country)
# calculate the expected count for each country
ts_europe_split_e <- pbapply::pblapply(
  ts_europe_split,
  function(x) {
    expected <- expected(
      x$population,
      x$new_cases,
      1
    )
    x$expected <- expected
    x
  }
)
# bind it together again
ts_europe <- bind_rows(ts_europe_split_e)
# sort it
ts_europe <- ts_europe[order(ts_europe$Country, ts_europe$Date), ]
# create a tibble for the date ids
date_tibble <- tibble(
  Date = unique(ts_europe$Date),
  id_date_1 = seq_len(length(unique(ts_europe$Date))),
  id_date_2 = seq_len(length(unique(ts_europe$Date)))
)
# create a tibble for the country ids
area_tibble <- tibble(
  CNTR_CODE = unique(ts_europe$CNTR_CODE),
  id_country_1 = seq_len(length(unique(ts_europe$CNTR_CODE))),
  id_country_2 = seq_len(length(unique(ts_europe$CNTR_CODE)))
)
# add the date variables
ts_europe <- merge(
  ts_europe,
  date_tibble,
  by = "Date"
)
# add the country variables
ts_europe <- merge(
  ts_europe,
  area_tibble,
  by = "CNTR_CODE"
)
# sort it
ts_europe <- ts_europe[order(ts_europe$Country, ts_europe$Date), ]
# only keep dates with non-negative number of cases
ts_europe <- ts_europe[ts_europe$new_cases >= 0, ]
# add id for date and area
ts_europe$id_date_area <- seq_len(nrow(ts_europe))
rm(list = setdiff(ls(), "ts_europe"))
# create the timeseries for norway and germany
ts_norway <- ts_europe[ts_europe$Country == "Norway", ][, c(1:6, 19:49)]
ts_germany <- ts_europe[ts_europe$Country == "Germany", ][, c(1:6, 19:49)]
# temporarily remove the geometry column
geom <- ts_norway$geometry
ts_norway$geometry <- NULL
# do some scaling
ts_norway[, c(7:12, 15, 26, 27)] <- scale(ts_norway[, c(7:12, 15, 26, 27)])
# add geometry again and turn into sf
ts_norway$geometry <- geom
ts_norway <- st_as_sf(ts_norway)
# first day of the final timeseries will be 3 weeks before the first case was detected
last_0 <- which(diff(which(ts_norway$new_cases %in% 0)) > 1 %in% TRUE)[1] - 20
ts_norway <- ts_norway[last_0:nrow(ts_norway), ]
# now the exact same for germany
geom <- ts_germany$geometry
ts_germany$geometry <- NULL
ts_germany[, c(7:12, 15, 26, 27)] <- scale(ts_germany[, c(7:12, 15, 26, 27)])
ts_germany$geometry <- geom
ts_germany <- st_as_sf(ts_germany)
last_0 <- which(diff(which(ts_germany$new_cases %in% 0)) > 1 %in% TRUE)[1] - 20
ts_germany <- ts_germany[last_0:nrow(ts_germany), ]
# and for europe
ts_europe$geometry <- NULL
ts_europe[, c(7:24, 27, 38, 39)] <- scale(ts_europe[, c(7:24, 27, 38, 39)])
ts_europe$geometry <- geom
ts_europe <- st_as_sf(ts_europe)
# but here we have to split for each country
ts_europe_split <- split(ts_europe, ts_europe$CNTR_CODE)
ts_europe_split <- lapply(ts_europe_split, function(x) {
  last_0 <- max(which(diff(which(x$new_cases %in% 0)) > 1 %in% TRUE)[1] - 20, 1)
  x <- x[last_0:nrow(x), ]
  x
})
# and bind it together again
ts_europe <- bind_rows(ts_europe_split)
# now we will get the actual numbers for germany and norway
rki <- get_RKI_timeseries()
# group it after municipality
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
germany_grouped <- germany_confirmed %>%
  group_by(Date) %>%
  summarise(
    new_cases = sum(NumberNewTestedIll)
  )
# keep only data for the dates in the timeseries
germany_grouped <- germany_grouped[germany_grouped$Date %in% ts_germany$Date, ]
# and replace the values
ts_germany$new_cases <- germany_grouped$new_cases
# now the same for norway
norway <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
# turn it into long format
norway <- melt(
  setDT(norway),
  id.vars = colnames(norway)[1:6],
  variable.name = "date"
)
norway$date <- as.Date(as.character(norway$date))
norway <- norway[
  order(
    norway$date,
    norway$kommune_no
  ),
]
# calculate the daily cases
daily_cases <- norway[359:nrow(norway), ]$value - norway[1:(nrow(norway) - 358), ]$value
# if they are below 0, we say no new cases
daily_cases[daily_cases < 0] <- 0
norway <- norway[359:nrow(norway), ]
norway$value <- daily_cases
# group by municipality
norway_grouped <- norway %>%
  group_by(date) %>%
  summarise(new_cases = sum(value))
norway_grouped <- norway_grouped[norway_grouped$date %in% ts_norway$Date, ]
ts_norway[ts_norway$Date %in% (seq(min(norway_grouped$date), max(norway_grouped$date), 1) + 1), ]$new_cases <- norway_grouped$new_cases
# remove the geometry columns
ts_germany$geometry <- NULL
ts_norway$geometry <- NULL
# create the csv files
write_csv(ts_germany, "wrangled_data/ts_germany.csv")
write_csv(ts_norway, "wrangled_data/ts_norway.csv")
rm(list = setdiff(ls(), c("ts_germany", "ts_norway", "ts_europe")))
