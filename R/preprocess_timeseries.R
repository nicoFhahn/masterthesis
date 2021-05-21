library(readr)
library(eurostat)
library(tibble)
library(ISOcodes)
library(stringr)
library(data.table)
library(dplyr)
library(sf)
library(SpatialEpi)
library(covidregionaldata)
library(covid19germany)
library(reshape2)
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
ts_europe <- ts[ts$continent == "Europe", ]
ts_europe <- ts_europe[!is.na(ts_europe$Date), ]
# ts_germany$extreme_poverty <- NULL
# ts_germany$handwashing_facilities <- NULL
# ts_germany$new_tests <- NULL
# ts_norway$extreme_poverty <- NULL
# ts_norway$handwashing_facilities <- NULL
# ts_norway$new_tests <- NULL
contains_na <- which(ts_europe$new_cases %in% NA)
for (i in contains_na) {
  if (ts_europe$Country[i - 1] == ts_europe$Country[i]) {
    ts_europe$total_cases[i] <- ts_europe$total_cases[i - 1]
    ts_europe$new_cases[i] <- 0
  } else {
    ts_europe$total_cases[i] <- 0
    ts_europe$new_cases[i] <- 0
  }
}
ts_europe$handwashing_facilities <- NULL
ts_europe$new_tests <- NULL
ts_europe$extreme_poverty <- NULL
# https://www.stat.gov.rs/en-us/vizuelizacija/interaktivni-grafikoni/mapa/
ts_europe[ts_europe$Country == "Serbia", ]$aged_70_older <- 26.5
# https://tobacconomics.org/files/research/645/237-fact-sheet-nmk-stc-see-2019-v4-1.pdf
ts_europe[ts_europe$Country == "North Macedonia", ]$female_smokers <- 39
ts_europe[ts_europe$Country == "North Macedonia", ]$male_smokers <- 57.9
table(ts_europe[is.na(ts_europe$retail_and_recreation), ]$Country)
ts_europe <- ts_europe[!is.na(ts_europe$retail_and_recreation), ]
ts_europe[ts_europe$Country == "Liechtenstein", ]$grocery_and_pharmacy
ts_europe[is.na(ts_europe$grocery_and_pharmacy), ]$grocery_and_pharmacy <- 0.667
ts_europe[ts_europe$Country == "Liechtenstein", ]$residential
ts_europe <- ts_europe[ts_europe$Country != "Liechtenstein", ]
park_na <- which(is.na(ts_europe[ts_europe$Country == "Estonia", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na[1] - 1] - ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na[14] + 1]) / 15
ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Estonia", ]$parks[park_na[1] - 1] - seq_len(14) * mean_decline
park_na <- which(is.na(ts_europe[ts_europe$Country == "Luxembourg", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na[1] - 1] - ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na[23] + 1]) / 24
ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Luxembourg", ]$parks[park_na[1] - 1] - seq_len(23) * mean_decline
park_na <- which(is.na(ts_europe[ts_europe$Country == "Malta", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Malta", ]$parks[park_na[1] - 1] - ts_europe[ts_europe$Country == "Malta", ]$parks[park_na[4] + 1]) / 5
ts_europe[ts_europe$Country == "Malta", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Malta", ]$parks[park_na[1] - 1] - seq_len(4) * mean_decline
park_na <- which(is.na(ts_europe[ts_europe$Country == "Slovenia", ]$parks))
mean_decline <- (ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na[1] - 1] - ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na[10] + 1]) / 11
ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na] <- ts_europe[ts_europe$Country == "Slovenia", ]$parks[park_na[1] - 1] - seq_len(10) * mean_decline
# https://www.covid19healthsystem.org/countries/northmacedonia/livinghit.aspx?Section=1.5%20Testing&Type=Section
ts_europe[ts_europe$Country == "North Macedonia" & ts_europe$Date < "2020-03-17", ]$testing_policy <- "No testing policy"
ts_europe[ts_europe$Country == "North Macedonia" & ts_europe$Date >= "2020-03-17", ]$testing_policy <- "Anyone with symptoms"
# https://www.euro.who.int/en/health-topics/health-emergencies/coronavirus-covid-19/news/news/2020/12/who-engages-medical-students-to-boost-covid-19-contact-tracing-in-north-macedonia
# https://balkaninsight.com/2020/04/16/north-macedonia-leads-region-in-covid-19-tracing-app/
ts_europe[ts_europe$Country == "North Macedonia" & ts_europe$Date < "2020-04-16", ]$contact_tracing <- "No tracing"
ts_europe[ts_europe$Country == "North Macedonia" & ts_europe$Date >= "2020-12-07", ]$contact_tracing <- "Comprehensive tracing"
ts_europe[is.na(ts_europe$contact_tracing), ]$contact_tracing <- "Limited tracing"
ts_europe[is.na(ts_europe$stringency_index), ]$Country
ts_europe <- ts_europe[ts_europe$Country != "North Macedonia", ]
ts_europe <- ts_europe[complete.cases(ts_europe), ]
iso_code <- ISO_3166_1[, c("Alpha_2", "Name")]
colnames(iso_code) <- c("CNTR_CODE", "Country")
iso_code[str_detect(iso_code$Country, "Moldova"), ]$Country <- "Moldova"
ts_europe <- merge(
  ts_europe,
  iso_code,
  by = "Country"
)
cases <- get_national_data(unique(ts_europe$Country))
cases <- cases[, c("date", "cases_new", "iso_code")]
colnames(cases) <- c("Date", "cases_new", "CNTR_CODE")
ts_europe <- merge(
  ts_europe,
  cases,
  by = c("Date", "CNTR_CODE")
)
ts_europe$new_cases <- ts_europe$cases_new
ts_europe$cases_new <- NULL
europe_shapes <- get_eurostat_geospatial(nuts_level = "0")
missing <- !unique(ts_europe$CNTR_CODE) %in% europe_shapes$CNTR_CODE
unique(ts_europe$Country)[missing]
unique(ts_europe$CNTR_CODE)[missing]
ts_europe$CNTR_CODE[ts_europe$CNTR_CODE == "GR"] <- "EL"
ts_europe$CNTR_CODE[ts_europe$CNTR_CODE == "GB"] <- "UK"
missing <- !unique(ts_europe$CNTR_CODE) %in% europe_shapes$CNTR_CODE
unique(ts_europe$Country)[missing]
unique(ts_europe$CNTR_CODE)[missing]
bl <- read_sf("misc_files/blr_admbnda_adm0_UNICEF.shp")
europe_shapes <- rbind(europe_shapes, europe_shapes[1, ])
europe_shapes[38, c(1:2, 5:7)] <- "BY"
europe_shapes[38, 3] <- "Belarus"
europe_shapes[38, ]$geometry <- bl$geometry
bh <- read_sf("misc_files/BIH_adm0.shp")
bh <- st_transform(bh, st_crs(europe_shapes))
europe_shapes <- rbind(europe_shapes, europe_shapes[1, ])
europe_shapes[39, c(1:2, 5:7)] <- "BA"
europe_shapes[39, 3] <- "Bosnia and Herzegovina"
europe_shapes[39, ]$geometry <- bh$geometry
md <- st_union(read_sf("misc_files/MDA_Admin1.shp"))
md <- st_transform(md, st_crs(europe_shapes))
europe_shapes <- rbind(europe_shapes, europe_shapes[1, ])
europe_shapes[40, c(1:2, 5:7)] <- "MD"
europe_shapes[40, 3] <- "Moldova"
europe_shapes[40, ]$geometry <- md
ts_europe <- ts_europe[ts_europe$CNTR_CODE %in% europe_shapes$CNTR_CODE, ]
population <- read_csv("wrangled_data/demo_pjan_1_Data.csv")
population <- population[population$TIME == 2020, ]
population$GEO[!population$GEO %in% unique(ts_europe$Country)]
unique(ts_europe$Country)[!unique(ts_europe$Country) %in% population$GEO]
population$GEO[10] <- "Germany"
population <- population[population$GEO %in% unique(ts_europe$Country), c("GEO", "Value")]
colnames(population)[1] <- "Country"
ts_europe <- merge(
  ts_europe,
  population,
  by = "Country"
)
ts_europe <- merge(
  ts_europe,
  europe_shapes[, 2],
  by = "CNTR_CODE"
)
# https://www.worldometers.info/world-population/belarus-population/
ts_europe[ts_europe$CNTR_CODE == "BY", ]$Value <- "9,446,609"
# https://www.worldometers.info/world-population/bosnia-and-herzegovina-population/
ts_europe[ts_europe$CNTR_CODE == "BA", ]$Value <- "3,263,180"
# https://www.worldometers.info/world-population/moldova-population/
ts_europe[ts_europe$CNTR_CODE == "MD", ]$Value <- "4,025,805"
ts_europe <- st_as_sf(ts_europe)
colnames(ts_europe)[42] <- "population"
ts_europe$population <- as.numeric(str_replace_all(ts_europe$population, ",", ""))
# rki <- get_RKI_timeseries()
# germany_confirmed <- group_RKI_timeseries(rki, Bundesland)
# germany_confirmed <- germany_confirmed %>% group_by(Date) %>% summarise(new_cases = sum(NumberNewTestedIll))
# germany_confirmed <- germany_confirmed[germany_confirmed$Date %in% ts_europe[ts_europe$Country == "Germany", ]$Date, ]
# norway_municipality_confirmed <- read_csv(
#   "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
# )
# # turn it into long format
# norway_municipality_confirmed_long <- melt(
#   setDT(norway_municipality_confirmed),
#   id.vars = colnames(norway_municipality_confirmed)[1:6],
#   variable.name = "date"
# )
# norway_confirmed <- norway_municipality_confirmed_long %>% group_by(date) %>% summarise(new_cases = sum(value))
# norway_confirmed$date <- as.Date(as.character(norway_confirmed$date))
# norway_confirmed <- norway_confirmed[norway_confirmed$date %in% ts_europe[ts_europe$Country == "Norway", ]$Date, ]
# ts_europe[ts_europe$Country == "Norway", ][ts_europe[ts_europe$Country == "Norway", ]$Date %in% norway_confirmed$date, ]
ts_europe_split <- split(ts_europe, ts_europe$Country)
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
ts_europe <- bind_rows(ts_europe_split_e)
ts_europe <- ts_europe[order(ts_europe$Country, ts_europe$Date), ]
# ts_europe <- ts_europe[ts_europe$new_cases >= 0, ]
date_tibble <- tibble(
  Date = unique(ts_europe$Date),
  id_date_1 = seq_len(length(unique(ts_europe$Date))),
  id_date_2 = seq_len(length(unique(ts_europe$Date)))
)
area_tibble <- tibble(
  CNTR_CODE = unique(ts_europe$CNTR_CODE),
  id_country_1 = seq_len(length(unique(ts_europe$CNTR_CODE))),
  id_country_2 = seq_len(length(unique(ts_europe$CNTR_CODE)))
)
ts_europe <- merge(
  ts_europe,
  date_tibble,
  by = "Date"
)
ts_europe <- merge(
  ts_europe,
  area_tibble,
  by = "CNTR_CODE"
)
ts_europe <- ts_europe[order(ts_europe$Country, ts_europe$Date), ]
ts_europe <- ts_europe[ts_europe$new_cases >= 0, ]
ts_europe$id_date_area <- seq_len(nrow(ts_europe))
rm(list = setdiff(ls(), "ts_europe"))
ts_norway <- ts_europe[ts_europe$Country == "Norway", ][, c(1:6, 19:49)]
ts_germany <- ts_europe[ts_europe$Country == "Germany", ][, c(1:6, 19:49)]
geom <- ts_norway$geometry
ts_norway$geometry <- NULL
ts_norway[, c(7:12, 15, 26, 27)] <- scale(ts_norway[, c(7:12, 15, 26, 27)])
ts_norway$geometry <- geom
ts_norway <- st_as_sf(ts_norway)
last_0 <- which(diff(which(ts_norway$new_cases %in% 0)) > 1 %in% TRUE)[1] - 20
ts_norway <- ts_norway[last_0:nrow(ts_norway), ]
geom <- ts_germany$geometry
ts_germany$geometry <- NULL
ts_germany[, c(7:12, 15, 26, 27)] <- scale(ts_germany[, c(7:12, 15, 26, 27)])
ts_germany$geometry <- geom
ts_germany <- st_as_sf(ts_germany)
last_0 <- which(diff(which(ts_germany$new_cases %in% 0)) > 1 %in% TRUE)[1] - 20
ts_germany <- ts_germany[last_0:nrow(ts_germany), ]
ts_europe$geometry <- NULL
ts_europe[, c(7:24, 27, 38, 39)] <- scale(ts_europe[, c(7:24, 27, 38, 39)])
ts_europe$geometry <- geom
ts_europe <- st_as_sf(ts_europe)
ts_europe_split <- split(ts_europe, ts_europe$CNTR_CODE)
ts_europe_split <- lapply(ts_europe_split, function(x) {
  last_0 <- max(which(diff(which(x$new_cases %in% 0)) > 1 %in% TRUE)[1] - 20, 1)
  x <- x[last_0:nrow(x), ]
  x
})
ts_europe <- bind_rows(ts_europe_split)
rki <- get_RKI_timeseries()
# group it after municipality
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
germany_grouped <- germany_confirmed %>%
  group_by(Date) %>%
  summarise(
    new_cases = sum(NumberNewTestedIll)
  )
germany_grouped <- germany_grouped[germany_grouped$Date %in% ts_germany$Date, ]
ts_germany$new_cases <- germany_grouped$new_cases
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
  order(norway$date,
        norway$kommune_no),
]
daily_cases <- norway[359:nrow(norway), ]$value - norway[1:(nrow(norway) - 358), ]$value
daily_cases[daily_cases < 0] <- 0
norway <-  norway[359:nrow(norway), ]
norway$value <- daily_cases
norway_grouped <- norway %>%
  group_by(date) %>%
  summarise(new_cases = sum(value))
norway_grouped <- norway_grouped[norway_grouped$date %in% ts_norway$Date, ]
ts_norway[ts_norway$Date %in% (seq(min(norway_grouped$date), max(norway_grouped$date), 1) + 1), ]$new_cases <- norway_grouped$new_cases
ts_germany$geometry <- NULL
ts_norway$geometry <- NULL
write_csv(ts_germany, "wrangled_data/ts_germany.csv")
write_csv(ts_norway, "wrangled_data/ts_norway.csv")
rm(list = setdiff(ls(), c("ts_germany", "ts_norway", "ts_europe")))
