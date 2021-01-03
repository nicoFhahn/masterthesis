library(readr)
library(covid19germany)
library(reshape)
library(data.table)
library(dplyr)
library(zoo)
internatonal_mobility <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/mobility_international.csv"
)
india_confirmed <- read_csv(
  "https://raw.githubusercontent.com/kalyaniuniversity/COVID-19-Datasets/master/India%20Statewise%20Confirmed%20Cases/COVID19_INDIA_STATEWISE_TIME_SERIES_CONFIRMED.csv"
)
india_confirmed_long <- melt(
  setDT(india_confirmed),
  id.vars = colnames(india_confirmed)[1:7],
  variable.name = "date"
)
india_confirmed_long <- india_confirmed_long[!is.na(india_confirmed_long$LATITUDE)]
slovakia_confirmed <- read_delim(
  "https://raw.githubusercontent.com/radoondas/covid-19-slovakia/master/data/covid-19-slovensko.csv",
  delim = ";",
  col_names = c("date", "city", "infected", "gender", "note_1", "note_2", "healthy", "died", "region", "age", "district"),
  col_types = list(col_character(), col_character(), col_double(), col_factor(), col_character(), col_character(), col_double(), col_double(), col_character(), col_double(), col_character()),
)
slovakia_confirmed_long <- setDT(slovakia_confirmed)
malta_confirmed <- read_csv(
  "https://raw.githubusercontent.com/Lobeslab-Ltd/covid-19-MT/master/malta_time_series.csv"
)
malta_confirmed_long <- setDT(malta_confirmed)
norway_district_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district_wide.csv"
)
norway_district_confirmed_long <- melt(
  setDT(norway_district_confirmed),
  id.vars = colnames(norway_district_confirmed)[1:10],
  variable.name = "date"
)
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
norway_municipality_confirmed_long <- melt(
  setDT(norway_municipality_confirmed),
  id.vars = colnames(norway_municipality_confirmed)[1:6],
  variable.name = "date"
)
norway_mobility <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/20_mobility/google/mobility.csv"
)
norway_mobility <- setDT(norway_mobility)
norway_mobility_ungrouped <- dcast(
  norway_mobility,
  fylke_no + fylke_name + date ~ category,
  value.var = "mob_change"
)
norway_mobility_ungrouped$fylke_no <- as.character(norway_mobility_ungrouped$fylke_no)
norway_mobility_ungrouped[norway_mobility_ungrouped$fylke_no == "3"]$fylke_no <- "03"
norway_district_confirmed_long$date <- as.Date(as.character(norway_district_confirmed_long$date))
norway_municipality_confirmed_long$date <- as.Date(as.character(norway_municipality_confirmed_long$date))
norway_district_confirmed_mobility <- merge(
  norway_district_confirmed_long,
  norway_mobility_ungrouped,
  by = c("fylke_no", "fylke_name", "date"),
  all = FALSE
)
norway_municipality_confirmed_mobility <- merge(
  norway_municipality_confirmed_long,
  norway_mobility_ungrouped,
  by = c("fylke_no", "fylke_name", "date"),
  all = FALSE
)
italy_confirmed_region <- read_csv(
  "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/Daily%20Covis19%20Italian%20Data%20Cumulative",
  col_types = list(col_date(), col_character(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()),
  col_names = c("date", "region", "hospitalised", "in_icu", "home_isolation", "healed", "dead", "tests", "people_tested"),
  skip = 1
)
italy_confirmed_region_long <- setDT(italy_confirmed_region)
italy_confirmed_province <- read_csv(
  "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/Daily_Covis19_Italian_Data_Province_Cumulative.csv"
)
italy_confirmed_province_long <- setDT(italy_confirmed_province)
brazil_confirmed <- read_csv(
  "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv"
)
brazil_confirmed_long <- setDT(brazil_confirmed)
brazil_city_infos <- read_csv(
  "https://raw.githubusercontent.com/wcota/covid19br/master/cities_info.csv"
)
brazil_city_infos <- setDT(brazil_city_infos)
brazil_confirmed_city_infos <- merge(
  brazil_confirmed_long,
  brazil_city_infos,
  by.x = c("ibgeID", "city", "state", "cod_RegiaoDeSaude", "name_RegiaoDeSaude"),
  by.y = c("ibge", "city", "state", "cod_RegiaoDeSaude", "name_RegiaoDeSaude"),
  all = TRUE
)
brazil_city_gps <- read_csv(
  "https://raw.githubusercontent.com/wcota/covid19br/master/gps_cities.csv"
)
brazil_confirmed_city_gps <- merge(
  brazil_confirmed_city_infos,
  setDT(brazil_city_gps),
  by = "ibgeID"
)
england_confirmed <- read_csv(
  "https://raw.githubusercontent.com/odileeds/covid-19-uk-datasets/master/data/england-cases.csv"
)
england_confirmed_long <- setDT(england_confirmed)
scotland_confirmed <- read_csv(
  "https://raw.githubusercontent.com/odileeds/covid-19-uk-datasets/master/data/scotland-cases.csv"
)
scotland_confirmed_long <- setDT(scotland_confirmed)
switzerland_confirmed <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv"
)
switzerland_confirmed <- switzerland_confirmed[, 1:28]
switzerland_confirmed_long <- melt(
  setDT(switzerland_confirmed),
  id.vars = c("Date"),
  variable.name = "canton"
)
switzerland_confirmed_long[switzerland_confirmed_long$Date == min(switzerland_confirmed_long$Date) & is.na(switzerland_confirmed_long$value)]$value <- 0
switzerland_confirmed_long$value <- na.locf(switzerland_confirmed_long$value)
switzerland_demographics <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/demographics.csv"
)
switzerland_confirmed_demographics <- merge(
  switzerland_confirmed_long,
  setDT(switzerland_demographics),
  by.x = "canton",
  by.y = "Canton"
)
switzerland_age <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/demographics_switzerland_bag.csv"
)
switzerland_age$canton <- NULL
switzerland_age$X1 <- NULL
switzerland_age[is.na(switzerland_age$Kanton), ]$Kanton <- "CH"
switzerland_age <- switzerland_age[switzerland_age$Kanton != "FL", ]
switzerland_age <- setDT(switzerland_age)
switzerland_age_wide <- dcast(
  switzerland_age,
  Kanton ~ age_group + sex,
  value.var = "Population"
)
switzerland_confirmed_demographics_age <- merge(
  switzerland_confirmed_demographics,
  switzerland_age_wide,
  by.x = "canton",
  by.y = "Kanton"
)
switzerland_mobility <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/mobility_switzerland.csv"
)
switzerland_mobility$index <- NULL
switzerland_mobility$country <- NULL
switzerland_mobility <- setDT(switzerland_mobility)
switzerland_confirmed_demo_age_mob <- merge(
  switzerland_confirmed_demographics_age,
  switzerland_mobility,
  by.x = c("canton", "Date"),
  by.y = c("canton", "date")
)
us_confirmed <- read_csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
)
us_maske_use <- read_csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv"
)
us_confirmed_mask_use <- merge(
  setDT(us_confirmed),
  setDT(us_maske_use),
  by.x = "fips",
  by.y = "COUNTYFP"
)
rki <- get_RKI_timeseries()
# germany_confirmed <- group_RKI_timeseries(rki, Landkreis, Gender, Age)
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
germany_population <- ew_kreise
germany_confirmed_population <- merge(
  setDT(germany_confirmed),
  setDT(germany_population),
  by.x = c("Landkreis", "IdLandkreis"),
  by.y = c("NameLandkreis", "IdLandkreis")
)

canada_confirmed <- read_csv(
  "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv"
)
canada_confirmed_detailed <- read_csv(
  "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv"
)
canada_confirmed_detailed$date_report <- as.Date(canada_confirmed_detailed$date_report, "%d-%m-%Y")
canada_confirmed_detailed_long <- canada_confirmed_detailed %>%
  group_by(date_report, province, travel_history_country, travel_yn, locally_acquired, age, sex) %>%
  summarise(
    cases = length(province)
  )