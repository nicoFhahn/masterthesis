library(data.table)
library(dplyr)
library(readr)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
library(covid19germany)
# download the newest RKI data
rki <- get_RKI_timeseries()
# group it after municipality
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
# load the previously generated file
germany_features <- read_csv("wrangled_data/germany_features.csv")
# get the newest date with at least 396 observations
date_1 <- rev(
  names(table(germany_confirmed$Date)[table(germany_confirmed$Date) > 395])
)[1]
# get the newest date from the feature file with at least 396 observations
date_2 <- rev(
  names(table(germany_features$Date)[table(germany_features$Date) > 395])
)[1]
# if there is newer data available create new feature dataset
if (date_1 != date_2) {
  # get the population of the municipalities
  germany_population <- ew_kreise
  # the data for berlin needs to be manually grouped
  # change the municipality name and ID to the same for all berlin cases
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
  # order the frame by the date
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
  germany_confirmed <- rbind(germany_confirmed, berlin_grouped)
  colnames(germany_confirmed)[9] <- "Kennziffer"
  # split the data by date
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
  germany_confirmed <- do.call(rbind, germany_confirmed_split)
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
  germany_confirmed <- do.call(rbind, germany_confirmed_split)
  # merge the case numbers with the population data
  germany_confirmed_population <- merge(
    setDT(germany_confirmed),
    setDT(germany_population),
    by.x = c("Kennziffer"),
    by.y = c("IdLandkreis")
  )
  # load the shape file for germany
  germany_shape <- read_sf("shapefiles/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")
  # transform it
  germany_shape <- st_transform(germany_shape, 4326)
  # now all the demographic data gets loaded
  germany_politics <- read_delim(
    "germany_data/europawahl_2019.csv",
    delim = ";"
  )[1:538, ]
  germany_unemployed <- read_delim(
    "germany_data/arbeitslose_2019.csv",
    delim = ";"
  )
  germany_protect <- read_delim(
    "germany_data/schutzsuchende_2018.csv",
    delim = ";"
  )
  germany_social <- read_delim(
    "germany_data/sozialhilfe_2019.csv",
    delim = ";"
  )
  germany_company_tax <- read_delim(
    "germany_data/gewerbesteuer_2015.csv",
    delim = ";"
  )
  germany_income_tax <- read_delim(
    "germany_data/einkommen_lohn_steuer_2016.csv",
    delim = ";"
  )
  germany_asyl <- read_delim(
    "germany_data/asylbewerberleistungen_2019.csv",
    delim = ";"
  )
  # replace comma by decimal point
  germany_politics$Wahlbeteiligung <- str_replace_all(
    germany_politics$Wahlbeteiligung,
    "\\,",
    "."
  )
  # turn the data into numeric
  germany_politics$Wahlbeteiligung <- as.numeric(
    germany_politics$Wahlbeteiligung
  )
  # get the newest company tax data
  germany_company_tax <- germany_company_tax[germany_company_tax$X1 == 2015, ]
  # set the correct ids for hamburg and berlin
  germany_politics[
    str_detect(germany_politics$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_politics[
    str_detect(germany_politics$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  germany_unemployed[
    str_detect(germany_unemployed$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_unemployed[
    str_detect(germany_unemployed$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  germany_protect[
    str_detect(germany_protect$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_protect[
    str_detect(germany_protect$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  germany_social[
    str_detect(germany_social$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_social[
    str_detect(germany_social$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  germany_company_tax <- germany_company_tax[
    !is.na(germany_company_tax$Kreis),
  ]
  germany_company_tax[
    str_detect(germany_company_tax$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_company_tax[
    str_detect(germany_company_tax$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  germany_income_tax[
    str_detect(germany_income_tax$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_income_tax[
    str_detect(germany_income_tax$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  germany_asyl[
    str_detect(germany_asyl$Stadt, "Hamburg"),
  ]$Kreis[1] <- "2000"
  germany_asyl[
    str_detect(germany_asyl$Stadt, "Berlin"),
  ]$Kreis[1] <- "11000"
  # remove data with unknown ids
  germany_politics <- germany_politics[
    germany_politics$Kreis %in% germany_shape$Kennziffer,
  ]
  germany_unemployed <- germany_unemployed[
    germany_unemployed$Kreis %in% germany_shape$Kennziffer,
  ]
  germany_protect <- germany_protect[
    germany_protect$Kreis %in% germany_shape$Kennziffer,
  ]
  germany_social <- germany_social[
    germany_social$Kreis %in% germany_shape$Kennziffer,
  ]
  germany_income_tax <- germany_income_tax[
    germany_income_tax$Kreis %in% germany_shape$Kennziffer,
  ]
  germany_asyl <- germany_asyl[
    germany_asyl$Kreis %in% germany_shape$Kennziffer,
  ]
  germany_company_tax <- germany_company_tax[
    germany_company_tax$Kreis %in% germany_shape$Kennziffer,
  ]
  # now all this data gets merged together
  germany <- merge(
    germany_asyl,
    germany_company_tax,
    by = "Kreis"
  )
  germany <- merge(
    germany,
    germany_income_tax,
    by = "Kreis"
  )
  germany <- merge(
    germany,
    germany_politics,
    by = "Kreis"
  )
  germany <- merge(
    germany,
    germany_protect,
    by = "Kreis"
  )
  germany <- merge(
    germany,
    germany_social,
    by = "Kreis"
  )
  germany <- merge(
    germany,
    germany_unemployed,
    by = "Kreis"
  )
  # remove unnecessary columns
  germany <- germany[, !str_detect(colnames(germany), "X1")]
  colnames(germany)[2] <- "Stadt"
  germany <- germany[, c(1:3, 9, 11, 12, 16:22, 25, 27, 33, 34)]
  # merge this data with the covid data
  germany <- merge(
    germany,
    germany_confirmed_population,
    by.x = "Kreis",
    by.y = "Kennziffer"
  )
  # load all the osm data
  load("osmdata/germany_hospital.Rda")
  load("osmdata/germany_place_of_worship.Rda")
  load("osmdata/germany_retail.Rda")
  load("osmdata/germany_nursing_home.Rda")
  load("osmdata/germany_restaurant.Rda")
  load("osmdata/germany_aerodrome.Rda")
  load("osmdata/germany_office.Rda")
  load("osmdata/germany_shops.Rda")
  load("osmdata/germany_platform.Rda")
  load("osmdata/germany_university.Rda")
  load("osmdata/germany_college.Rda")
  load("osmdata/germany_kindergarten.Rda")
  load("osmdata/germany_schools.Rda")
  load("osmdata/germany_bakery.Rda")
  load("osmdata/germany_residential1.Rda")
  load("osmdata/germany_residential2.Rda")
  # create the residential list
  germany_residential <- c(germany_residential1, germany_residential2)
  load("osmdata/germany_hairdresser.Rda")
  load("osmdata/germany_clinic.Rda")
  load("osmdata/germany_sport.Rda")
  load("osmdata/germany_entertainment.Rda")
  load("osmdata/germany_marketplace.Rda")
  # now spatial matching for each
  germany_shape$marketplace <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_marketplace[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$entertainment <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_entertainment[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$sport <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_sport[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$clinic <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_clinic[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$hairdresser <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_hairdresser[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$shops <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_shops[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$place_of_worship <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_place_of_worship[[as.character(
                germany_shape$Kennziffer[x]
              )]]
            )
          )
        )
      }
    )
  )
  germany_shape$retail <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_retail[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$nursing_home <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_nursing_home[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$restaurant <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_restaurant[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$aerodrome <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_aerodrome[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$office <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_office[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$platform <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_platform[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$university <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_university[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$college <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_college[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$kindergarten <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_kindergarten[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$schools <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_schools[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$bakeries <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_bakeries[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  germany_shape$residential <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              germany_shape[x, ],
              germany_residential[[as.character(germany_shape$Kennziffer[x])]]
            )
          )
        )
      }
    )
  )
  # remove needless files from the enviroment
  rm(
    list = setdiff(
      ls(),
      c(
        "germany_shape", "germany_confirmed_population",
        "germany", "germany_confirmed"
      )
    )
  )
  # once again merging data
  germany_complete <- merge(
    germany_shape,
    germany_confirmed_population,
    by = "Kennziffer",
    all = FALSE
  )
  # # safe the shapes
  write_sf(
    st_as_sf(germany_complete)[!duplicated(germany_complete$Kennziffer), ][, 1],
    "wrangled_data/shapes_germany.shp"
  )
  # # remove more needless variables
  germany_complete[, 2:23] <- NULL
  germany_complete$geometry <- NULL
  germany_complete$Kennziffer <- as.character(germany_complete$Kennziffer)
  # order by data and municipality
  germany <- germany[order(germany$Date, germany$Kreis), ]
  # # merge together again
  no_geometry <- merge(
    germany,
    germany_complete[!duplicated(germany_complete$Kennziffer), 1:21],
    by.x = "Kreis",
    by.y = "Kennziffer"
  )
  # remove needless variables
  no_geometry$Stadt <- NULL
  no_geometry[, c(19:21, 23:27, 29, 31)] <- NULL
  colnames(no_geometry)[6] <- "stimmen"
  # # save the data
  # calculate the SIR
  write_csv(no_geometry, "wrangled_data/germany_features_temporal.csv")
  rm(list = ls())
  # prepare the data
  germany_features <- read_csv("wrangled_data/germany_features_temporal.csv")
}
# turn variables into numeric
germany_features$Gewerbesteuer <- as.numeric(
  trimws(germany_features$Gewerbesteuer)
)
germany_features$schutzsuchende <- as.numeric(
  trimws(germany_features$schutzsuchende)
)
# calculate the percentage of the vote
germany_features[, 7:12] <- germany_features[, 7:12] / germany_features$stimmen
# read the shapefile
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
# merge it together
germany <- merge(
  germany_features,
  germany_sf,
  by.x = "Kreis",
  by.y = "Kennziffer"
)
# turn the date into an actual date
germany$Date <- as.Date(germany$Date)
max_date <- max(germany$Date)
germany <- germany[
  germany$Date %in% c(
    max_date - 28, max_date - 21, max_date - 14, max_date - 7, max_date
  ),
]
numbers <- germany[order(germany$Date, germany$Kreis), ] #
numbers <- st_as_sf(numbers)
st_crs(numbers) <- 4326
# add the area, densities and sex proportion
numbers$area <- as.numeric(set_units(st_area(numbers), km^2))
numbers$pop_dens <- numbers$PopulationTotal / numbers$area
numbers$urb_dens <- numbers$residential / numbers$area
numbers$sex <- numbers$PopulationFemale /
  numbers$PopulationTotal
# keep only relevant variables
numbers$higher_education <- numbers$college +
  numbers$university
# remove variable
numbers$PopulationFemale <- NULL
numbers$Landkreis.y <- NULL
# impute missing values
cols_imputed <- lapply(
  c(1:39),
  function(x, ...) {
    vals <- numbers[, x]
    vals$geometry <- NULL
    # use the median to impute
    vals[is.na(vals)] <- median(vals[, 1], na.rm = TRUE)
    vals
  }
)
# bind it together again
numbers_imputed <- Reduce(cbind, cols_imputed)
# add the rest of the data again
numbers_imputed <- cbind(numbers_imputed, numbers[, 41:45])
# turn it into a spatial frame
numbers <- st_as_sf(numbers_imputed)
# remove needless variables
numbers$college <- NULL
numbers$university <- NULL
numbers$stimmen <- NULL
# rename some variables
colnames(numbers)[c(1:5, 12:16, 18, 19)] <- c(
  "municipality_id",
  "asyl_benefits",
  "trade_tax",
  "income_total",
  "income_tax",
  "protection_seekers",
  "welfare_recipients",
  "unemployed_total",
  "unemployed_foreigners",
  "municipality",
  "value",
  "population"
)
# save the geometry before removing it
geom <- numbers$geometry
numbers$geometry <- NULL
# scale all the variables
numbers[, c(2:15, 20:35, 38:41)] <- scale(
  numbers[, c(2:15, 20:35, 38:41)]
)
# add the geometry again
numbers$geometry <- geom
numbers <- st_as_sf(numbers)
rm(list = setdiff(ls(), c("numbers")))

numbers_28 <- numbers[numbers$Date == max(numbers$Date) - 28, ]
numbers_21 <- numbers[numbers$Date == max(numbers$Date) - 21, ]
numbers_14 <- numbers[numbers$Date == max(numbers$Date) - 14, ]
numbers_7 <- numbers[numbers$Date == max(numbers$Date) - 7, ]
numbers_0 <- numbers[numbers$Date == max(numbers$Date), ]
rm(
  list = setdiff(
    ls(),
    c("numbers_0", "numbers_7", "numbers_14", "numbers_21", "numbers_28")
  )
)

# get the newest numbers
newest_numbers <- germany[
  germany$Date == rev(
    names(table(germany$Date)[table(germany$Date) > 395])
  )[1],
]
# calculate the expected count for the newest numbers
expected_count <- expected(
  population = newest_numbers$PopulationTotal,
  cases = newest_numbers$CumNumberTestedIll,
  n.strata = 1
)
# add it as a variable
newest_numbers$expected_count <- expected_count
# calculate the SIR
newest_numbers$sir <- newest_numbers$CumNumberTestedIll /
  newest_numbers$expected_count
# turn it into a spatial frame
newest_numbers <- st_as_sf(newest_numbers)
st_crs(newest_numbers) <- 4326
# calculate the proportion of infected people
newest_numbers$inf_rate <- newest_numbers$CumNumberTestedIll /
  newest_numbers$PopulationTotal
# add id area variables
newest_numbers$idarea_1 <- seq_len(nrow(newest_numbers))
newest_numbers$idarea_2 <- seq_len(nrow(newest_numbers))
# add the area
newest_numbers$area <- as.numeric(set_units(st_area(newest_numbers), km^2))
# add population and urban density
newest_numbers$pop_dens <- newest_numbers$PopulationTotal / newest_numbers$area
newest_numbers$urb_dens <- newest_numbers$residential / newest_numbers$area
# add proportion of females
newest_numbers$sex <- newest_numbers$PopulationFemale /
  newest_numbers$PopulationTotal
# add number of higher education buildings
