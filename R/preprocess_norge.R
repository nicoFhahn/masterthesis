library(data.table)
library(readr)
library(dplyr)
library(reshape2)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
#####################################################
# prepare the data
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
norway_municipality_confirmed_long <- melt(
  setDT(norway_municipality_confirmed),
  id.vars = colnames(norway_municipality_confirmed)[1:6],
  variable.name = "date"
)
norge_shape <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
norge_features <- read_csv("wrangled_data/norge_features.csv")
date_1 <- max(as.Date(as.character(norway_municipality_confirmed_long$date)))
date_2 <- max(norge_features$date)
if (date_1 != date_2) {
  kommune_4602 <- norge_shape[norge_shape$kommunenum == 4602, ][1, ]
  kommune_4602$geometry <- st_union(norge_shape[norge_shape$kommunenum == 4602, ])
  norge_shape <- norge_shape[norge_shape$kommunenum != 4602, ]
  norge_shape <- rbind(norge_shape, kommune_4602)
  norge_demo <- read_delim("norge_data/norge_age.csv", ";")
  norge_demo$age <- as.numeric(str_extract(norge_demo$age, "[0-9]{1,}"))
  norge_demo$total_age <- norge_demo$age * norge_demo$`Persons 2020`
  norge_demo_region <- split(norge_demo, norge_demo$region)
  norge_demo_region_sex <- lapply(norge_demo_region, function(x) split(x, x$sex))
  norge_demo <- lapply(
    norge_demo_region_sex,
    function(x) {
      frames <- lapply(
        x,
        function(y) {
          mean_age <- sum(y$total_age) / sum(y$`Persons 2020`)
          median_age <- median(rep(y$age, y$`Persons 2020`))
          as_tibble(
            data.frame(
              region = y$region[1],
              sex = y$sex[1],
              population = sum(y$`Persons 2020`),
              mean_age_sex = mean_age,
              median_age_sex = median_age
            )
          )
        }
      )
      frame <- do.call(rbind, frames)
      frame_both <- do.call(rbind, x)
      frame$median_age <- median(rep(frame_both$age, frame_both$`Persons 2020`))
      frame$mean_age <- sum(frame_both$total_age) / sum(frame_both$`Persons 2020`)
      frame_final <- as_tibble(
        data.frame(
          region = frame$region[1],
          population_total = sum(frame$population),
          population_male = frame$population[1],
          population_female = frame$population[2],
          mean_age_male = frame$mean_age_sex[1],
          mean_age_female = frame$mean_age_sex[2],
          median_age_male = frame$median_age_sex[1],
          median_age_female = frame$median_age_sex[2],
          mean_age = frame$mean_age[1],
          median_age = frame$median_age[1]
        )
      )
      frame_final
    }
  )
  norge_demo <- do.call(rbind, norge_demo)
  norge_demo$kommune_no <- str_extract(norge_demo$region, "[0-9]{4}")
  norge_no_shape <- merge(
    norway_municipality_confirmed_long,
    norge_demo,
    by = "kommune_no"
  )
  norge_unemploy <- read_delim("norge_data/norge_unemployment.csv", delim = ";")
  colnames(norge_unemploy)[3] <- "unemployed_perc"
  norge_unemploy_s <- split(norge_unemploy, norge_unemploy$region)
  norge_unemploy_s <- lapply(
    norge_unemploy_s,
    function(x) {
      data.table(
        region = x$region[1],
        unemp_tot = x$unemployed_perc[1],
        unemp_immg = x$unemployed_perc[2]
      )
    }
  )
  norge_unemploy <- do.call(rbind, norge_unemploy_s)
  norge_unemploy$kommune_no <- str_extract(
    norge_unemploy$region,
    "[0-9]{4}"
  )
  norge_no_shape <- merge(
    norge_no_shape,
    norge_unemploy,
    by = "kommune_no"
  )
  norge_workers <- read_delim("norge_data/norge_workers.csv", delim = ";")
  norge_workers_s <- split(norge_workers, norge_workers$region)
  norge_workers_s <- lapply(
    norge_workers_s,
    function(x) {
      data.table(
        region = x$region[1],
        workers_ft_res = x$`Employees by place of residence 2019`[8],
        workers_pt_res = x$`Employees by place of residence 2019`[7],
        mining_ft_res = x$`Employees by place of residence 2019`[10],
        mining_pt_res = x$`Employees by place of residence 2019`[9],
        construction_ft_res = x$`Employees by place of residence 2019`[12],
        construction_pt_res = x$`Employees by place of residence 2019`[11],
        workers_ft_work = x$`Employees by place of work 2019`[8],
        workers_pt_work = x$`Employees by place of work 2019`[7],
        mining_ft_work = x$`Employees by place of work 2019`[10],
        mining_pt_work = x$`Employees by place of work 2019`[9],
        construction_ft_work = x$`Employees by place of work 2019`[12],
        construction_pt_work = x$`Employees by place of work 2019`[11],
        workers_ft_com = x$`Employees by place of work 2019`[8] -
          x$`Employees by place of residence 2019`[8],
        workers_pt_com = x$`Employees by place of work 2019`[7] -
          x$`Employees by place of residence 2019`[7],
        mining_ft_com = x$`Employees by place of work 2019`[10] -
          x$`Employees by place of residence 2019`[10],
        mining_pt_com = x$`Employees by place of work 2019`[9] -
          x$`Employees by place of residence 2019`[9],
        construction_ft_com = x$`Employees by place of work 2019`[12] -
          x$`Employees by place of residence 2019`[12],
        construction_pt_com = x$`Employees by place of work 2019`[11] -
          x$`Employees by place of residence 2019`[11]
      )
    }
  )
  norge_workers <- do.call(rbind, norge_workers_s)
  norge_workers$kommune_no <- str_extract(
    norge_workers$region,
    "[0-9]{4}"
  )
  norge_no_shape <- merge(
    norge_no_shape,
    norge_workers,
    by = "kommune_no"
  )
  norge_immigration <- read_delim("norge_data/norge_immigration.csv", delim = ";")
  norge_immigration_s <- split(norge_immigration, norge_immigration$region)
  norge_immigration_s <- lapply(
    norge_immigration_s,
    function(x) {
      data.table(
        region = x$region[1],
        immigrants_total = x$`Per cent of population 2020`[1],
        immigrants_norge = x$`Per cent of population 2020`[2],
        immigrants_pure = x$`Per cent of population 2020`[1] -
          x$`Per cent of population 2020`[2]
      )
    }
  )
  norge_immigration <- do.call(rbind, norge_immigration_s)
  norge_immigration$kommune_no <- str_extract(
    norge_immigration$region,
    "[0-9]{4}"
  )
  norge_no_shape <- merge(
    norge_no_shape,
    norge_immigration,
    by = "kommune_no"
  )
  norge_no_shape$region.y <- NULL
  norge_no_shape$region.x <- NULL
  norge_no_shape$region.x <- NULL
  colnames(norge_no_shape)[38] <- "region"
  load("osmdata/norge_hospital.Rda")
  load("osmdata/norge_place_of_worship.Rda")
  load("osmdata/norge_retail.Rda")
  load("osmdata/norge_nursing_home.Rda")
  load("osmdata/norge_restaurant.Rda")
  load("osmdata/norge_terminal.Rda")
  load("osmdata/norge_aerodrome.Rda")
  load("osmdata/norge_office.Rda")
  load("osmdata/norge_shops.Rda")
  load("osmdata/norge_platform.Rda")
  load("osmdata/norge_university.Rda")
  load("osmdata/norge_college.Rda")
  load("osmdata/norge_kindergarten.Rda")
  load("osmdata/norge_schools.Rda")
  load("osmdata/norge_bakeries.Rda")
  load("osmdata/norge_gas.Rda")
  load("osmdata/norge_banks.Rda")
  load("osmdata/norge_atms.Rda")
  load("osmdata/norge_residential.Rda")
  load("osmdata/norge_hairdresser.Rda")
  load("osmdata/norge_toilets.Rda")
  load("osmdata/norge_clinic.Rda")
  load("osmdata/norge_sport.Rda")
  load("osmdata/norge_entertainment.Rda")
  load("osmdata/norge_marketplace.Rda")
  
  
  norge_shape$marketplace <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_marketplace[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_marketplace[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$entertainment <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_entertainment[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_entertainment[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$sport <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_sport[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_sport[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$clinic <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_clinic[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_clinic[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$toilet <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_toilets[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_toilets[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$hairdresser <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_hairdresser[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_hairdresser[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$shops <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_shops[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_shops[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$place_of_worship <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_place_of_worship[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_place_of_worship[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$retail <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_retail[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_retail[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$nursing_home <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_nursing_home[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_nursing_home[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$restaurant <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_restaurant[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_restaurant[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$terminal <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_terminal[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_terminal[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$aerodrome <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_aerodrome[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_aerodrome[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$office <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_office[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_office[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$platform <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_platform[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_platform[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$university <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_university[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_university[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$college <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_college[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_college[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$kindergarten <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_kindergarten[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_kindergarten[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$schools <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_schools[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_schools[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$bakeries <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_bakeries[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_bakeries[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$gas <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_gas[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_gas[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  norge_shape$banks <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_banks[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_banks[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  
  norge_shape$atm <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_atms[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_atms[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  
  norge_shape$residential <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        ifelse(
          nrow(norge_residential[[x]]) > 0,
          length(
            unlist(
              st_intersects(
                norge_shape[x, ], norge_residential[[x]]
              )
            )
          ),
          0
        )
      }
    )
  )
  
  
  setDT(norge_shape)
  colnames(norge_shape)[1] <- "kommune_no"
  norge_complete <- merge(
    norge_no_shape,
    norge_shape,
    by = "kommune_no",
    all = FALSE
  )
  norge_complete$objtype <- NULL
  norge_complete$lokalid <- NULL
  norge_complete$oppdaterin <- NULL
  norge_complete$datauttaks <- NULL
  norge_complete$versjonid <- NULL
  norge_complete$opphav <- NULL
  norge_complete$samiskforv <- NULL
  norge_complete$datafangst <- NULL
  norge_complete$navnerom <- NULL
  norge_complete$navn <- NULL
  no_geometry <- norge_complete
  no_geometry$geometry <- NULL
  no_geometry$higher_education <- no_geometry$college + no_geometry$university
  # calculate the SIR
  write_csv(no_geometry, "wrangled_data/norge_features.csv")
  write_sf(st_as_sf(norge_complete)[!duplicated(norge_complete$kommune_no), ][, 1], "wrangled_data/shapes_norge.shp") 
  norge_features <- read_csv("wrangled_data/norge_features.csv")
}
norge_features[, c(18:37, 39:64, 66)] <- 1000 * norge_features[, c(18:37, 39:64, 66)] / norge_features$population
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
norge <- merge(
  norge_features,
  norge_sf,
  by = "kommune_no"
)
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
norway_municipality_confirmed_long <- melt(
  setDT(norway_municipality_confirmed),
  id.vars = colnames(norway_municipality_confirmed)[1:6],
  variable.name = "date"
)
norway_municipality_confirmed_long$date <- as.Date(as.character(norway_municipality_confirmed_long$date))
newest_numbers <- norway_municipality_confirmed_long[norway_municipality_confirmed_long$date == max(norway_municipality_confirmed_long$date), ]
newest_numbers$value <- NULL
newest_numbers$time <- NULL
newest_numbers$fylke_no <- NULL
newest_numbers$fylke_name <- NULL
newest_numbers$population <- NULL
newest_numbers$date <- NULL
newest_numbers$kommune_name <- NULL
newest_numbers <- merge(
  newest_numbers,
  norge[norge$date == max(norge$date), ],
  by = "kommune_no"
)
expected_count <- expected(
  population = newest_numbers$population,
  cases = newest_numbers$value,
  n.strata = 1
)
newest_numbers$expected_count <- expected_count
# calculate the SIR
newest_numbers$sir <- newest_numbers$value / newest_numbers$expected_count
newest_numbers <- st_as_sf(newest_numbers)
st_crs(newest_numbers) <- 4326
# calculate the number of infected people
newest_numbers$inf_rate <- newest_numbers$value / newest_numbers$population
# add id area variables
newest_numbers$idarea_1 <- seq_len(nrow(newest_numbers))
newest_numbers$idarea_2 <- seq_len(nrow(newest_numbers))
# add the expected count
newest_numbers$area <- as.numeric(set_units(st_area(newest_numbers), km^2))
newest_numbers$pop_dens <- newest_numbers$population / newest_numbers$area
newest_numbers$urb_dens <- newest_numbers$residential / newest_numbers$area
newest_numbers$sex <- newest_numbers$population_female / newest_numbers$population_total

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
#   norge[norge$date == (max(norge$date) - 21), ],
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
newest_numbers <- newest_numbers[, c(1, 3, 6:9, 17:37, 39:54, 56:60, 62:76)]
rm(list = setdiff(ls(), c("newest_numbers")))
