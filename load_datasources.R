library(readr)
library(covid19germany)
internatonal_mobility <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/mobility_international.csv"
)
india_confirmed <- read_csv(
  "https://raw.githubusercontent.com/kalyaniuniversity/COVID-19-Datasets/master/India%20Statewise%20Confirmed%20Cases/COVID19_INDIA_STATEWISE_TIME_SERIES_CONFIRMED.csv"
)
slovakia_confirmed <- read_delim(
  "https://raw.githubusercontent.com/radoondas/covid-19-slovakia/master/data/covid-19-slovensko.csv",
  delim = ";",
  col_names = FALSE
)
malta_confirmed <- read_csv(
  "https://raw.githubusercontent.com/Lobeslab-Ltd/covid-19-MT/master/malta_time_series.csv"
)
norway_district_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district_wide.csv"
)
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
norway_mobility <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/20_mobility/google/mobility.csv"
)
italy_confirmed_region <- read_csv(
  "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/Daily%20Covis19%20Italian%20Data%20Cumulative",
  col_types = list(col_date(), col_character(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double()),
  col_names = c("date", "region", "hospitalised", "in_icu", "home_isolation", "healed", "dead", "tests", "people_tested"),
  skip = 1
)
italy_confirmed_province <- read_csv(
  "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/Daily_Covis19_Italian_Data_Province_Cumulative.csv"
)
brazil_confirmed <- read_csv(
  "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv"
)
brazil_city_infos <- read_csv(
  "https://raw.githubusercontent.com/wcota/covid19br/master/cities_info.csv"
)
brazil_city_gps <- read_csv(
  "https://raw.githubusercontent.com/wcota/covid19br/master/gps_cities.csv"
)
canada_confirmed <- read_csv(
  "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv"
)
canada_confirmed_detailed <- read_csv(
  "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv"
)
england_confirmed <- read_csv(
  "https://raw.githubusercontent.com/odileeds/covid-19-uk-datasets/master/data/england-cases.csv"
)
scotland_confirmed <- read_csv(
  "https://raw.githubusercontent.com/odileeds/covid-19-uk-datasets/master/data/scotland-cases.csv"
)
switzerland_confirmed <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv"
)
switzerland_demographics <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/demographics.csv"
)
switzerland_age <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/demographics_switzerland_bag.csv"
)
switzerland_mobility <- read_csv(
  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/mobility_switzerland.csv"
)
us_confirmed <- read_csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
)
us_maske_use <- read_csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv"
)
rki <- get_RKI_timeseries()
# germany_confirmed <- group_RKI_timeseries(rki, Landkreis, Gender, Age)
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
germany_population <- ew_kreise
