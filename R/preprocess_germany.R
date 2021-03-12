library(data.table)
library(dplyr)
library(readr)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
library(covid19germany)
#####################################################
rki <- get_RKI_timeseries()
# germany_confirmed <- group_RKI_timeseries(rki, Landkreis, Gender, Age)
germany_confirmed <- group_RKI_timeseries(rki, Landkreis)
germany_features <- read_csv("wrangled_data/germany_features.csv")
date_1 <- rev(names(table(germany_confirmed$Date)[table(germany_confirmed$Date) > 395]))[1]
date_2 <- rev(names(table(germany_features$Date)[table(germany_features$Date) > 395]))[1]
if (date_1 != date_2) {
  germany_population <- ew_kreise
  germany_confirmed[str_detect(germany_confirmed$Landkreis, "Berlin"), ]$Landkreis <- "SK Berlin"
  germany_confirmed[str_detect(germany_confirmed$Landkreis, "Berlin"), ]$IdLandkreis <- 11000
  berlin <- germany_confirmed[germany_confirmed$Landkreis == "SK Berlin", ]
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
  for (i in 2:nrow(berlin_grouped)) {
    berlin_grouped[i, ]$CumNumberTestedIll <- berlin_grouped[i - 1, ]$CumNumberTestedIll + berlin_grouped[i, ]$NumberNewTestedIll
    berlin_grouped[i, ]$CumNumberDead <- berlin_grouped[i - 1, ]$CumNumberDead + berlin_grouped[i, ]$NumberNewDead
    berlin_grouped[i, ]$CumNumberRecovered <- berlin_grouped[i - 1, ]$CumNumberRecovered + berlin_grouped[i, ]$NumberNewRecovered
  }
  germany_confirmed <- germany_confirmed[germany_confirmed$Landkreis != "SK Berlin", ]
  germany_confirmed <- rbind(germany_confirmed, berlin_grouped)
  colnames(germany_confirmed)[9] <- "Kennziffer"
  germany_confirmed_population <- merge(
    setDT(germany_confirmed),
    setDT(germany_population),
    by.x = c("Kennziffer"),
    by.y = c("IdLandkreis")
  )
  germany_shape <- read_sf("shapefiles/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")
  germany_shape <- st_transform(germany_shape, 4326)
  germany_politics <- read_delim("germany_data/europawahl_2019.csv", delim = ";")[1:538, ]
  germany_politics$Wahlbeteiligung <- str_replace_all(germany_politics$Wahlbeteiligung, "\\,", ".")
  germany_politics$Wahlbeteiligung <- as.numeric(germany_politics$Wahlbeteiligung)
  germany_politics[str_detect(germany_politics$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_politics[str_detect(germany_politics$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_politics <- germany_politics[germany_politics$Kreis %in% germany_shape$Kennziffer, ]
  germany_unemployed <- read_delim("germany_data/arbeitslose_2019.csv", delim = ";") # [1:538, ]
  germany_unemployed[str_detect(germany_unemployed$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_unemployed[str_detect(germany_unemployed$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_unemployed <- germany_unemployed[germany_unemployed$Kreis %in% germany_shape$Kennziffer, ]
  germany_protect <- read_delim("germany_data/schutzsuchende_2018.csv", delim = ";") # [1:538, ]
  germany_protect[str_detect(germany_protect$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_protect[str_detect(germany_protect$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_protect <- germany_protect[germany_protect$Kreis %in% germany_shape$Kennziffer, ]
  germany_social <- read_delim("germany_data/sozialhilfe_2019.csv", delim = ";") # [1:538, ]
  germany_social[str_detect(germany_social$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_social[str_detect(germany_social$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_social <- germany_social[germany_social$Kreis %in% germany_shape$Kennziffer, ]
  germany_company_tax <- read_delim("germany_data/gewerbesteuer_2015.csv", delim = ";")
  germany_company_tax <- germany_company_tax[!is.na(germany_company_tax$Kreis), ]
  germany_company_tax <- germany_company_tax[germany_company_tax$X1 == 2015, ]
  germany_company_tax[str_detect(germany_company_tax$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_company_tax[str_detect(germany_company_tax$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_company_tax <- germany_company_tax[germany_company_tax$Kreis %in% germany_shape$Kennziffer, ]
  germany_income_tax <- read_delim("germany_data/einkommen_lohn_steuer_2016.csv", delim = ";") # [1:538, ]
  germany_income_tax[str_detect(germany_income_tax$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_income_tax[str_detect(germany_income_tax$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_income_tax <- germany_income_tax[germany_income_tax$Kreis %in% germany_shape$Kennziffer, ]
  germany_asyl <- read_delim("germany_data/asylbewerberleistungen_2019.csv", delim = ";") # [1:538, ]
  germany_asyl[str_detect(germany_asyl$Stadt, "Hamburg"), ]$Kreis[1] <- "2000"
  germany_asyl[str_detect(germany_asyl$Stadt, "Berlin"), ]$Kreis[1] <- "11000"
  germany_asyl <- germany_asyl[germany_asyl$Kreis %in% germany_shape$Kennziffer, ]
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
  germany <- germany[, !str_detect(colnames(germany), "X1")]
  colnames(germany)[2] <- "Stadt"
  germany <- germany[, c(1:5, 9, 11, 12, 15:23, 25, 27:31, 33:39)]
  germany <- merge(
    germany,
    germany_confirmed_population,
    by.x = "Kreis",
    by.y = "Kennziffer"
  )
  load("osmdata/germany_hospital.Rda")
  load("osmdata/germany_place_of_worship.Rda")
  load("osmdata/germany_retail.Rda")
  load("osmdata/germany_nursing_home.Rda")
  load("osmdata/germany_restaurant.Rda")
  load("osmdata/germany_terminal.Rda")
  load("osmdata/germany_aerodrome.Rda")
  load("osmdata/germany_office.Rda")
  load("osmdata/germany_shops.Rda")
  load("osmdata/germany_platform.Rda")
  load("osmdata/germany_university.Rda")
  load("osmdata/germany_college.Rda")
  load("osmdata/germany_kindergarten.Rda")
  load("osmdata/germany_schools.Rda")
  load("osmdata/germany_bakeries.Rda")
  load("osmdata/germany_residential1.Rda")
  load("osmdata/germany_residential2.Rda")
  germany_residential <- c(germany_residential1, germany_residential2)
  load("osmdata/germany_hairdresser.Rda")
  load("osmdata/germany_clinic.Rda")
  load("osmdata/germany_sport.Rda")
  load("osmdata/germany_entertainment.Rda")
  load("osmdata/germany_marketplace.Rda")

  germany_shape$marketplace <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(germany_marketplace[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_marketplace[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_entertainment[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_entertainment[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_sport[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_sport[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_clinic[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_clinic[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_hairdresser[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_hairdresser[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_shops[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_shops[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_place_of_worship[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_place_of_worship[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_retail[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_retail[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_nursing_home[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_nursing_home[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_restaurant[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_restaurant[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )

  germany_shape$terminal <- unlist(
    lapply(
      seq_len(
        nrow(germany_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(germany_terminal[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_terminal[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_aerodrome[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_aerodrome[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_office[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_office[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_platform[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_platform[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_university[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_university[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_college[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_college[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_kindergarten[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_kindergarten[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_schools[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_schools[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_bakeries[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_bakeries[[x]]
              )
            )
          ),
          0
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
        ifelse(
          nrow(germany_residential[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                germany_shape[x, ], germany_residential[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )

  rm(list = setdiff(ls(), c("germany_shape", "germany_confirmed_population", "germany", "germany_confirmed")))

  germany_complete <- merge(
    germany_shape,
    germany_confirmed_population,
    by = "Kennziffer",
    all = FALSE
  )
  write_sf(st_as_sf(germany_complete)[!duplicated(germany_complete$Kennziffer), ][, 1], "wrangled_data/shapes_germany.shp")
  germany_complete[, 2:23] <- NULL
  germany_complete$geometry <- NULL
  germany_complete$Kennziffer <- as.character(germany_complete$Kennziffer)
  germany <- germany[order(germany$Date, germany$Kreis), ]
  germany_complete <- germany_complete[order(germany_complete$Date, germany_complete$Kennziffer), ]
  no_geometry <- cbind(germany, germany_complete)
  no_geometry$Stadt <- NULL
  no_geometry[, c(3, 4, 19:22, 25:29, 32:34, 36:40, 44, 66:80)] <- NULL
  colnames(no_geometry)[7] <- "stimmen"

  # calculate the SIR
  write_csv(no_geometry, "wrangled_data/germany_features.csv")
  rm(list = ls())
  # prepare the data
  germany_features <- read_csv("wrangled_data/germany_features.csv")
}
germany_features$Gewerbesteuer <- as.numeric(trimws(germany_features$Gewerbesteuer))
germany_features$schutzsuchende <- as.numeric(trimws(germany_features$schutzsuchende))
germany_features[, 8:14] <- germany_features[, 8:14] / germany_features$stimmen
germany_features[, c(2:5, 14:18, 26:44)] <- 1000 * germany_features[, c(2:5, 14:18, 26:44)] / germany_features$PopulationTotal
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
germany <- merge(
  germany_features,
  germany_sf,
  by.x = "Kreis",
  by.y = "Kennziffer"
)
germany$Date <- as.Date(germany$Date)
newest_numbers <- germany[germany$Date == rev(names(table(germany$Date)[table(germany$Date) > 395]))[1], ]
expected_count <- expected(
  population = newest_numbers$PopulationTotal,
  cases = newest_numbers$CumNumberTestedIll,
  n.strata = 1
)
newest_numbers$expected_count <- expected_count
# calculate the SIR
newest_numbers$sir <- newest_numbers$CumNumberTestedIll / newest_numbers$expected_count
newest_numbers <- st_as_sf(newest_numbers)
st_crs(newest_numbers) <- 4326
# calculate the number of infected people
newest_numbers$inf_rate <- newest_numbers$CumNumberTestedIll / newest_numbers$PopulationTotal
# add id area variables
newest_numbers$idarea_1 <- seq_len(nrow(newest_numbers))
newest_numbers$idarea_2 <- seq_len(nrow(newest_numbers))
# add the expected count
newest_numbers$area <- as.numeric(set_units(st_area(newest_numbers), km^2))
newest_numbers$pop_dens <- newest_numbers$PopulationTotal / newest_numbers$area
newest_numbers$urb_dens <- newest_numbers$residential / newest_numbers$area
newest_numbers$sex <- newest_numbers$PopulationFemale / newest_numbers$PopulationTotal
cols_imputed <- lapply(
  c(1:47, 49:55),
  function(x, ...) {
    vals <- newest_numbers[, x]
    vals$geometry <- NULL
    vals[is.na(vals)] <- median(vals[, 1], na.rm = TRUE)
    vals
  }
)
newest_numbers_imputed <- Reduce(cbind, cols_imputed)
newest_numbers_imputed$geometry <- newest_numbers$geometry
newest_numbers <- st_as_sf(newest_numbers_imputed)

# newest_numbers_21 <- norway_municipality_confirmed_long[norway_municipality_confirmed_long$date == (max(norway_municipality_confirmed_long$date) - 21), ]
# newest_numbers_21$value <- NULL
# newest_numbers_21$time <- NULL
# newest_numbers_21$fylke_no <- NULL
# newest_numbers_21$fylke_name <- NULL
# newest_numbers_21$population <- NULL
# newest_numbers_21$date <- NULL
# newest_numbers_21$kommune_name <- NULL
# newest_numbers_21 <- merge(
#   newest_numbers_21,
#   germany[germany$date == (max(germany$date) - 21), ],
#   by = "kommune_no"
# )
# expected_count <- expected(
#   population = newest_numbers_21$population,
#   cases = newest_numbers_21$value,
#   n.strata = 1
# )
# newest_numbers_21$expected_count <- expected_count
# # calculate the SIR
# newest_numbers_21$sir <- newest_numbers_21$value / newest_numbers_21$expected_count
# newest_numbers_21 <- st_as_sf(newest_numbers_21)
# st_crs(newest_numbers_21) <- 4326
# # calculate the number of infected people
# newest_numbers_21$inf_rate <- newest_numbers_21$value / newest_numbers_21$population
# # add id area variables
# newest_numbers_21$idarea_1 <- seq_len(nrow(newest_numbers_21))
# newest_numbers_21$idarea_2 <- seq_len(nrow(newest_numbers_21))
# # add the expected count
# newest_numbers_21$area <- as.numeric(set_units(st_area(newest_numbers_21), km^2))
# newest_numbers_21$pop_dens <- newest_numbers_21$population / newest_numbers_21$area
# newest_numbers_21$urb_dens <- newest_numbers_21$residential / newest_numbers_21$area
# newest_numbers_21$sex <- newest_numbers_21$population_female / newest_numbers_21$population_total
newest_numbers$higher_education <- newest_numbers$college + newest_numbers$university
rm(list = setdiff(ls(), c("newest_numbers")))
