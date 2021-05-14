library(readr)
library(tibble)
library(dplyr)
infections <- read_csv(
  "timeseries/owid-covid-data.csv"
)
covariates <- list.files(
  "timeseries/"
)
covariates <- covariates[covariates != "owid-covid-data.csv"]
covariates <- paste("timeseries/", covariates, sep = "")
covariate_tibbles <- lapply(covariates, read_csv)
covariate_tibbles <- lapply(
  covariate_tibbles,
  function(x) {
    x$Code <- NULL
    x
  }
)
min_date <- as.Date("2020-01-01")
max_date <- as.Date("2021-05-14")
date_range <- seq(min_date, max_date, 1)
countries <- unique(do.call(c, lapply(covariate_tibbles, function(x) unique(x$Entity))))
covariate_tibbles_full <- pbapply::pblapply(
  covariate_tibbles,
  function(x, ...) {
    missing_countries <- countries[!countries %in% x$Entity]
    missing_frame <- x[rep(1, length(missing_countries) * length(unique(x$Day))), ]
    missing_frame$Entity <- unlist(lapply(missing_countries, rep, length(unique(x$Day))))
    missing_frame$Day <- rep(unique(x$Day), length(missing_countries))
    missing_frame[, 3:ncol(missing_frame)] <- NA
    x <- rbind(missing_frame, x)
    country_split <- split(x, x$Entity)
    country_split <- lapply(
      country_split, function(y, ...) {
        missing_dates <- date_range[!date_range %in% y$Day]
        missing_frame <- y[rep(1, length(missing_dates)), ]
        missing_frame$Entity <- y$Entity[1]
        missing_frame$Day <-missing_dates
        missing_frame[, 3:ncol(missing_frame)] <- NA
        y <- rbind(missing_frame, y)
        y
      }
    )
    x <- bind_rows(country_split)
    x
  }
)
covariates_tibble <- bind_cols(covariate_tibbles_full)
covariates_tibble <- covariates_tibble[
  , c(
    1:8, 11, 14, 17, 20, 23, 26, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57,
    61, 64
  )
]
colnames(covariates_tibble)[1:2] <- c("Country", "Date")
infections <- infections[, c(
  2:6, 46:59
)]
colnames(infections)[2:3] <- c("Country", "Date")
missing_countries <- countries[!countries %in% infections$Country]
missing_frame <- infections[rep(1, length(missing_countries) * length(unique(infections$Date))), ]
missing_frame$Country <- unlist(lapply(missing_countries, rep, length(unique(infections$Date))))
missing_frame$Date <- rep(unique(infections$Date), length(missing_countries))
missing_frame$continent <- NA
missing_frame[, 4:ncol(missing_frame)] <- NA
infections <- rbind(missing_frame, infections)
country_split <- split(infections, infections$Country)
country_split <- lapply(
  country_split, function(y, ...) {
    missing_dates <- date_range[!date_range %in% y$Date]
    missing_frame <- y[rep(1, length(missing_dates)), ]
    missing_frame$Country <- y$Country[1]
    missing_frame$continent <- y$continent[1]
    missing_frame$population_density <- y$population_density[1]
    missing_frame$median_age <- y$median_age[1]
    missing_frame$aged_65_older <- y$aged_65_older[1]
    missing_frame$aged_70_older <- y$aged_70_older[1]
    missing_frame$gdp_per_capita <- y$gdp_per_capita[1]
    missing_frame$extreme_poverty <- y$extreme_poverty[1]
    missing_frame$cardiovasc_death_rate <- y$cardiovasc_death_rate[1]
    missing_frame$diabetes_prevalence <- y$diabetes_prevalence[1]
    missing_frame$female_smokers <- y$female_smokers[1]
    missing_frame$male_smokers <- y$male_smokers[1]
    missing_frame$handwashing_facilities <- y$handwashing_facilities[1]
    missing_frame$hospital_beds_per_thousand <- y$hospital_beds_per_thousand[1]
    missing_frame$life_expectancy <- y$life_expectancy[1]
    missing_frame$human_development_index <- y$human_development_index[1]
    missing_frame$Date <- missing_dates
    missing_frame[, 4:5] <- 0
    y <- rbind(missing_frame, y)
    y
  }
)
infections <- bind_rows(country_split)
infections <- infections[order(infections$Country, infections$Date), ]
covariates_tibble <- covariates_tibble[order(covariates_tibble$Country, covariates_tibble$Date), ]
infections <- infections[
  paste(infections$Country, infections$Date) %in%
    paste(covariates_tibble$Country, covariates_tibble$Date),
]
full_tibble <- cbind(infections, covariates_tibble)
write_csv(full_tibble, "wrangled_data/timeseries_covid.csv")
