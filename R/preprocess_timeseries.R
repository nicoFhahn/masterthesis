library(readr)
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
ts_germany <- ts_europe[ts_europe$Country == "Germany", ]
ts_norway <- ts_europe[ts_europe$Country == "Norway", ]
ts_germany$extreme_poverty <- NULL
ts_germany$handwashing_facilities <- NULL
ts_germany$new_tests <- NULL
ts_norway$extreme_poverty <- NULL
ts_norway$handwashing_facilities <- NULL
ts_norway$new_tests <- NULL
ts_germany <- ts_germany[complete.cases(ts_germany), ]
ts_norway <- ts_norway[complete.cases(ts_norway), ]
rm(list = setdiff(ls(), c("ts_germany", "ts_norway", "ts_europe")))
