# this is the script used to create the timeseries csv
library(dplyr)
library(here)
library(readr)
library(RSelenium)
library(stringr)
# load infection / vaccination data
infections <- read_csv(
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
)
# these websites provide interesting data
websites <- c(
  "https://ourworldindata.org/grapher/changes-visitors-covid",
  "https://ourworldindata.org/grapher/covid-19-testing-policy",
  "https://ourworldindata.org/grapher/covid-contact-tracing",
  "https://ourworldindata.org/grapher/covid-stringency-index",
  "https://ourworldindata.org/grapher/covid-vaccination-policy",
  "https://ourworldindata.org/grapher/face-covering-policies-covid",
  "https://ourworldindata.org/grapher/full-list-covid-19-tests-per-day",
  "https://ourworldindata.org/grapher/income-support-covid",
  "https://ourworldindata.org/grapher/internal-movement-covid",
  "https://ourworldindata.org/grapher/international-travel-covid",
  "https://ourworldindata.org/grapher/public-campaigns-covid",
  "https://ourworldindata.org/grapher/public-events-covid",
  "https://ourworldindata.org/grapher/public-gathering-rules-covid",
  "https://ourworldindata.org/grapher/public-transport-covid",
  "https://ourworldindata.org/grapher/school-closures-covid",
  "https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid",
  "https://ourworldindata.org/grapher/share-people-vaccinated-covid",
  "https://ourworldindata.org/grapher/stay-at-home-covid",
  "https://ourworldindata.org/grapher/workplace-closures-covid"
)
# set the download location
download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
# use chromedriver
driver <- rsDriver(
  browser = "chrome",
  chromever = "89.0.4389.23"
)
# initialise server and browser
server <- driver$server
browser <- driver$client
# now go through each website and click where you need to click in order to get
# to the downloading part
for (i in websites) {
  filename <- paste(rev(str_split(i, "/")[[1]])[1], ".csv", sep = "")
  browser$navigate(i)
  Sys.sleep(20)
  tab <- browser$findElements("download-tab-button", using = "class")
  tab[[1]]$clickElement()
  Sys.sleep(20)
  buttons <- browser$findElements("btn-primary", using = "class")
  buttons[[1]]$clickElement()
  Sys.sleep(5)
  # move the files
  file.rename(file.path(download_location, filename), here("timeseries", filename))
}
# stop everything
browser$close()
server$stop()
# get the filenames
covariates <- list.files(
  "timeseries/"
)
covariates <- paste("timeseries/", covariates, sep = "")
# load all the files
covariate_tibbles <- lapply(covariates, read_csv)
# remove the code col for each dataset
covariate_tibbles <- lapply(
  covariate_tibbles,
  function(x) {
    x$Code <- NULL
    x
  }
)
# get the daterange of the infections data
min_date <- as.Date("2020-01-01")
max_date <- max(infections$date)
date_range <- seq(min_date, max_date, 1)
# get all the countries
countries <- unique(infections$location)
# add the missing data for each dataset
covariate_tibbles_full <- lapply(
  covariate_tibbles,
  function(x, ...) {
    # check what countries are missing
    missing_countries <- countries[!countries %in% x$Entity]
    # create the missing frame and add the values for country and date
    missing_frame <- x[rep(1, length(missing_countries) * length(unique(x$Day))), ]
    missing_frame$Entity <- unlist(lapply(missing_countries, rep, length(unique(x$Day))))
    missing_frame$Day <- rep(unique(x$Day), length(missing_countries))
    # set the rest to NA
    missing_frame[, 3:ncol(missing_frame)] <- NA
    x <- rbind(missing_frame, x)
    # now split the data by the country
    country_split <- split(x, x$Entity)
    country_split <- lapply(
      country_split, function(y, ...) {
        # check which dates are missing
        missing_dates <- date_range[!date_range %in% y$Day]
        # create the missing frame
        missing_frame <- y[rep(1, length(missing_dates)), ]
        # add the country and date
        missing_frame$Entity <- y$Entity[1]
        missing_frame$Day <- missing_dates
        # if the data is before the first vaccination, add 0 as number of
        # vaccinated people
        if (any(missing_frame$Day < min(y$Day))) {
          if (any(str_detect(colnames(y), "vaccinated"))) {
            missing_frame[
              missing_frame$Day < min(y$Day), 3:ncol(missing_frame)
            ] <- 0
            # if it is not about vaccinations use the value of the respective
            # variables
          } else {
            missing_frame[
              missing_frame$Day < min(y$Day), 3:ncol(missing_frame)
            ] <- y[y$Day == min(y$Day), 3:ncol(missing_frame)]
          }
        }
        # if new data is missing, use the last available data
        if (any(missing_frame$Day > max(y$Day))) {
          missing_frame[
            missing_frame$Day > max(y$Day), 3:ncol(missing_frame)
          ] <- y[nrow(y), 3:ncol(missing_frame)]
        }
        # bind it all together and sort it
        y <- rbind(missing_frame, y)
        y[order(y$Day), ]
      }
    )
    # bind it all together
    x <- bind_rows(country_split)
    x <- x[x$Entity %in% countries, ]
    x[x$Day %in% date_range, ]
  }
)
# bind it all together
covariates_tibble <- bind_cols(covariate_tibbles_full)
# remove useless variables
covariates_tibble <- covariates_tibble[
  , c(
    1:8, 11, 14, 17, 20, 23, 26, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57,
    61, 64
  )
]
# nicer colnames
colnames(covariates_tibble)[1:2] <- c("Country", "Date")
# remove more useless variables
infections <- infections[, c(
  2:6, 46:59
)]
# nicer colnames
colnames(infections)[2:3] <- c("Country", "Date")
# again add data for missing dates
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
# sort it
infections <- infections[order(infections$Country, infections$Date), ]
# sort this as well
covariates_tibble <- covariates_tibble[
  order(covariates_tibble$Country, covariates_tibble$Date),
]
# bind it together
full_tibble <- cbind(infections, covariates_tibble)
# remove useless data
full_tibble[, 20] <- NULL
full_tibble[, 20] <- NULL
# use nicer names for the factors
full_tibble$testing_policy[
  full_tibble$testing_policy == 0 & !is.na(full_tibble$testing_policy)
] <- "No testing policy"
full_tibble$testing_policy[
  full_tibble$testing_policy == 1 & !is.na(full_tibble$testing_policy)
] <- "Symptoms & key groups"
full_tibble$testing_policy[
  full_tibble$testing_policy == 2 & !is.na(full_tibble$testing_policy)
] <- "Anyone with symptoms"
full_tibble$testing_policy[
  full_tibble$testing_policy == 3 & !is.na(full_tibble$testing_policy)
] <- "Open public testing"
full_tibble$testing_policy <- as.factor(full_tibble$testing_policy)
full_tibble$contact_tracing[
  full_tibble$contact_tracing == 0 & !is.na(full_tibble$contact_tracing)
] <- "No tracing"
full_tibble$contact_tracing[
  full_tibble$contact_tracing == 1 & !is.na(full_tibble$contact_tracing)
] <- "Limited tracing"
full_tibble$contact_tracing[
  full_tibble$contact_tracing == 2 & !is.na(full_tibble$contact_tracing)
] <- "Comprehensive tracing"
full_tibble$contact_tracing <- as.factor(full_tibble$contact_tracing)
full_tibble$vaccination_policy[
  full_tibble$vaccination_policy == 0 & !is.na(full_tibble$vaccination_policy)
] <- "None"
full_tibble$vaccination_policy[
  full_tibble$vaccination_policy == 1 & !is.na(full_tibble$vaccination_policy)
] <- "One group"
full_tibble$vaccination_policy[
  full_tibble$vaccination_policy == 2 & !is.na(full_tibble$vaccination_policy)
] <- "Two groups"
full_tibble$vaccination_policy[
  full_tibble$vaccination_policy == 3 & !is.na(full_tibble$vaccination_policy)
] <- "All vulnerable groups"
full_tibble$vaccination_policy[
  full_tibble$vaccination_policy == 4 & !is.na(full_tibble$vaccination_policy)
] <- "Vulnerable + some others"
full_tibble$vaccination_policy[
  full_tibble$vaccination_policy == 5 & !is.na(full_tibble$vaccination_policy)
] <- "Universal"
full_tibble$vaccination_policy <- as.factor(full_tibble$vaccination_policy)
full_tibble$facial_coverings[
  full_tibble$facial_coverings == 0 & !is.na(full_tibble$facial_coverings)
] <- "No policy"
full_tibble$facial_coverings[
  full_tibble$facial_coverings == 1 & !is.na(full_tibble$facial_coverings)
] <- "Recommended"
full_tibble$facial_coverings[
  full_tibble$facial_coverings == 2 & !is.na(full_tibble$facial_coverings)
] <- "Required in some public spaces"
full_tibble$facial_coverings[
  full_tibble$facial_coverings == 3 & !is.na(full_tibble$facial_coverings)
] <- "Required in all public spaces"
full_tibble$facial_coverings[
  full_tibble$facial_coverings == 4 & !is.na(full_tibble$facial_coverings)
] <- "Required outside at all times"
full_tibble$facial_coverings <- as.factor(full_tibble$facial_coverings)
full_tibble$income_support[
  full_tibble$income_support == 0 & !is.na(full_tibble$income_support)
] <- "No income support"
full_tibble$income_support[
  full_tibble$income_support == 1 & !is.na(full_tibble$income_support)
] <- "<50% of lost salary"
full_tibble$income_support[
  full_tibble$income_support == 2 & !is.na(full_tibble$income_support)
] <- ">50% of lost salary"
full_tibble$income_support <- as.factor(full_tibble$income_support)
full_tibble$restrictions_internal_movements[
  full_tibble$restrictions_internal_movements == 0 &
    !is.na(full_tibble$restrictions_internal_movements)
] <- "No measures"
full_tibble$restrictions_internal_movements[
  full_tibble$restrictions_internal_movements == 1 &
    !is.na(full_tibble$restrictions_internal_movements)
] <- "Recommend movement restriction"
full_tibble$restrictions_internal_movements[
  full_tibble$restrictions_internal_movements == 2 &
    !is.na(full_tibble$restrictions_internal_movements)
] <- "Restrict movement"
full_tibble$restrictions_internal_movements <- as.factor(
  full_tibble$restrictions_internal_movements
)
full_tibble$international_travel_controls[
  full_tibble$international_travel_controls == 0 &
    !is.na(full_tibble$international_travel_controls)
] <- "No measures"
full_tibble$international_travel_controls[
  full_tibble$international_travel_controls == 1 &
    !is.na(full_tibble$international_travel_controls)
] <- "Screening"
full_tibble$international_travel_controls[
  full_tibble$international_travel_controls == 2 &
    !is.na(full_tibble$international_travel_controls)
] <- "Quarantine from high-risk regions"
full_tibble$international_travel_controls[
  full_tibble$international_travel_controls == 3 &
    !is.na(full_tibble$international_travel_controls)
] <- "Ban on high-risk regions"
full_tibble$international_travel_controls[
  full_tibble$international_travel_controls == 4 &
    !is.na(full_tibble$international_travel_controls)
] <- "Total border closure"
full_tibble$international_travel_controls <- as.factor(
  full_tibble$international_travel_controls
)
full_tibble$public_information_campaigns[
  full_tibble$public_information_campaigns == 0 &
    !is.na(full_tibble$public_information_campaigns)
] <- "None"
full_tibble$public_information_campaigns[
  full_tibble$public_information_campaigns == 1 &
    !is.na(full_tibble$public_information_campaigns)
] <- "Public officials urging caution"
full_tibble$public_information_campaigns[
  full_tibble$public_information_campaigns == 2 &
    !is.na(full_tibble$public_information_campaigns)
] <- "Coordinated information campaign"
full_tibble$public_information_campaigns <- as.factor(
  full_tibble$public_information_campaigns
)
full_tibble$cancel_public_events[
  full_tibble$cancel_public_events == 0 &
    !is.na(full_tibble$cancel_public_events)
] <- "No measures"
full_tibble$cancel_public_events[
  full_tibble$cancel_public_events == 1 &
    !is.na(full_tibble$cancel_public_events)
] <- "Recommended cancellations"
full_tibble$cancel_public_events[
  full_tibble$cancel_public_events == 2 &
    !is.na(full_tibble$cancel_public_events)
] <- "Required cancellations"
full_tibble$cancel_public_events <- as.factor(full_tibble$cancel_public_events)
full_tibble$restriction_gatherings[
  full_tibble$restriction_gatherings == 0 &
    !is.na(full_tibble$restriction_gatherings)
] <- "No restrictions"
full_tibble$restriction_gatherings[
  full_tibble$restriction_gatherings == 1 &
    !is.na(full_tibble$restriction_gatherings)
] <- ">1000 people"
full_tibble$restriction_gatherings[
  full_tibble$restriction_gatherings == 2 &
    !is.na(full_tibble$restriction_gatherings)
] <- "100-1000 people"
full_tibble$restriction_gatherings[
  full_tibble$restriction_gatherings == 3 &
    !is.na(full_tibble$restriction_gatherings)
] <- "10-100 people"
full_tibble$restriction_gatherings[
  full_tibble$restriction_gatherings == 4 &
    !is.na(full_tibble$restriction_gatherings)
] <- "<10 people"
full_tibble$restriction_gatherings <- as.factor(
  full_tibble$restriction_gatherings
)
full_tibble$close_public_transport[
  full_tibble$close_public_transport == 0 &
    !is.na(full_tibble$close_public_transport)
] <- "No measures"
full_tibble$close_public_transport[
  full_tibble$close_public_transport == 1 &
    !is.na(full_tibble$close_public_transport)
] <- "Recommended closing"
full_tibble$close_public_transport[
  full_tibble$close_public_transport == 2 &
    !is.na(full_tibble$close_public_transport)
] <- "Required closing"
full_tibble$close_public_transport <- as.factor(
  full_tibble$close_public_transport
)
full_tibble$school_closures[
  full_tibble$school_closures == 0 & !is.na(full_tibble$school_closures)
] <- "No measures"
full_tibble$school_closures[
  full_tibble$school_closures == 1 & !is.na(full_tibble$school_closures)
] <- "Recommended"
full_tibble$school_closures[
  full_tibble$school_closures == 2 & !is.na(full_tibble$school_closures)
] <- "Required (some levels)"
full_tibble$school_closures[
  full_tibble$school_closures == 3 & !is.na(full_tibble$school_closures)
] <- "Required (all levels)"
full_tibble$school_closures <- as.factor(full_tibble$school_closures)
full_tibble$stay_home_requirements[
  full_tibble$stay_home_requirements == 0 &
    !is.na(full_tibble$stay_home_requirements)
] <- "No measures"
full_tibble$stay_home_requirements[
  full_tibble$stay_home_requirements == 1 &
    !is.na(full_tibble$stay_home_requirements)
] <- "Recommended"
full_tibble$stay_home_requirements[
  full_tibble$stay_home_requirements == 2 &
    !is.na(full_tibble$stay_home_requirements)
] <- "Required (except essentials)"
full_tibble$stay_home_requirements[
  full_tibble$stay_home_requirements == 3 &
    !is.na(full_tibble$stay_home_requirements)
] <- "Required (few exceptions)"
full_tibble$stay_home_requirements <- as.factor(
  full_tibble$stay_home_requirements
)
full_tibble$workplace_closures[
  full_tibble$workplace_closures == 0 & !is.na(full_tibble$workplace_closures)
] <- "No measures"
full_tibble$workplace_closures[
  full_tibble$workplace_closures == 1 & !is.na(full_tibble$workplace_closures)
] <- "Recommended"
full_tibble$workplace_closures[
  full_tibble$workplace_closures == 2 & !is.na(full_tibble$workplace_closures)
] <- "Required for some"
full_tibble$workplace_closures[
  full_tibble$workplace_closures == 3 & !is.na(full_tibble$workplace_closures)
] <- "Required for all but key workers"
full_tibble$workplace_closures <- as.factor(full_tibble$workplace_closures)
# save it
write_csv(full_tibble, "wrangled_data/timeseries_covid.csv")
rm(list = ls())
