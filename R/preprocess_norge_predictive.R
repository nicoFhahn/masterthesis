library(data.table)
library(readr)
library(dplyr)
library(reshape2)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
#####################################################
# load the newest data
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
# turn it into long format
norway_municipality_confirmed_long <- melt(
  setDT(norway_municipality_confirmed),
  id.vars = colnames(norway_municipality_confirmed)[1:6],
  variable.name = "date"
)
# read the shapefile
norge_shape <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
# read the last safed data
norge_features <- read_csv("wrangled_data/norge_features.csv")
# check if there is newer data available
date_1 <- max(as.Date(as.character(norway_municipality_confirmed_long$date)))
date_2 <- max(as.Date(norge_features$date))
if (date_1 != date_2) {
  # this kommune exists twice, therefore we merge the geometry
  kommune_4602 <- norge_shape[norge_shape$kommunenum == 4602, ][1, ]
  kommune_4602$geometry <- st_union(
    norge_shape[norge_shape$kommunenum == 4602, ]
  )
  # remove the two data points
  norge_shape <- norge_shape[norge_shape$kommunenum != 4602, ]
  # add the new data
  norge_shape <- rbind(norge_shape, kommune_4602)
  # now we load the age data
  norge_demo <- read_delim("norge_data/norge_age.csv", ";")
  # extract the numbers
  norge_demo$age <- as.numeric(str_extract(norge_demo$age, "[0-9]{1,}"))
  # multiply the age by the number of persons
  norge_demo$total_age <- norge_demo$age * norge_demo$`Persons 2020`
  # split the data by region
  norge_demo_region <- split(norge_demo, norge_demo$region)
  # split the regions by gender
  norge_demo_region_sex <- lapply(
    norge_demo_region, function(x) split(x, x$sex)
  )
  # now calculate for each region the gender-specific mean and median age
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
      frame$mean_age <- sum(
        frame_both$total_age
      ) / sum(frame_both$`Persons 2020`)
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
  # bind it all together again
  norge_demo <- do.call(rbind, norge_demo)
  # extract the kommune numbers
  norge_demo$kommune_no <- str_extract(norge_demo$region, "[0-9]{4}")
  # merge it
  norge_no_shape <- merge(
    norway_municipality_confirmed_long,
    norge_demo,
    by = "kommune_no"
  )
  # load the unemployment data
  norge_unemploy <- read_delim("norge_data/norge_unemployment.csv", delim = ";")
  colnames(norge_unemploy)[3] <- "unemployed_perc"
  # split it by region
  norge_unemploy_s <- split(norge_unemploy, norge_unemploy$region)
  # get the unemployment in each region for all and for foreigners
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
  # bind it together
  norge_unemploy <- do.call(rbind, norge_unemploy_s)
  # get the kommune number
  norge_unemploy$kommune_no <- str_extract(
    norge_unemploy$region,
    "[0-9]{4}"
  )
  # merge it
  norge_no_shape <- merge(
    norge_no_shape,
    norge_unemploy,
    by = "kommune_no"
  )
  # load the workers data
  norge_workers <- read_delim("norge_data/norge_workers.csv", delim = ";")
  # split it by region
  norge_workers_s <- split(norge_workers, norge_workers$region)
  # get the values of interest
  norge_workers_s <- lapply(
    norge_workers_s,
    function(x) {
      data.table(
        region = x$region[1],
        workers_ft_res = x$`Employees by place of residence 2019`[8],
        workers_pt_res = x$`Employees by place of residence 2019`[7],
        construction_ft_res = x$`Employees by place of residence 2019`[12],
        construction_pt_res = x$`Employees by place of residence 2019`[11],
        workers_ft_work = x$`Employees by place of work 2019`[8],
        workers_pt_work = x$`Employees by place of work 2019`[7],
        construction_ft_work = x$`Employees by place of work 2019`[12],
        construction_pt_work = x$`Employees by place of work 2019`[11],
        workers_ft = x$`Employees by place of work 2019`[8] -
          x$`Employees by place of residence 2019`[8],
        workers_pt = x$`Employees by place of work 2019`[7] -
          x$`Employees by place of residence 2019`[7],
        construction_ft = x$`Employees by place of work 2019`[12] -
          x$`Employees by place of residence 2019`[12],
        construction_pt = x$`Employees by place of work 2019`[11] -
          x$`Employees by place of residence 2019`[11]
      )
    }
  )
  # bind it together, extract the kommune number and merge
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
  # now the same for immigration data
  norge_immigration <- read_delim(
    "norge_data/norge_immigration.csv",
    delim = ";"
  )
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
  # remove useless variables
  norge_no_shape$region.y <- NULL
  norge_no_shape$region.x <- NULL
  norge_no_shape$region.x <- NULL
  colnames(norge_no_shape)[32] <- "region"
  # load the osm data
  load("osmdata/norge_hospital.Rda")
  load("osmdata/norge_place_of_worship.Rda")
  load("osmdata/norge_retail.Rda")
  load("osmdata/norge_nursing_home.Rda")
  load("osmdata/norge_restaurant.Rda")
  load("osmdata/norge_aerodrome.Rda")
  load("osmdata/norge_office.Rda")
  load("osmdata/norge_shops.Rda")
  load("osmdata/norge_platform.Rda")
  load("osmdata/norge_university.Rda")
  load("osmdata/norge_college.Rda")
  load("osmdata/norge_kindergarten.Rda")
  load("osmdata/norge_schools.Rda")
  load("osmdata/norge_bakery.Rda")
  load("osmdata/norge_residential.Rda")
  load("osmdata/norge_hairdresser.Rda")
  load("osmdata/norge_clinic.Rda")
  load("osmdata/norge_sport.Rda")
  load("osmdata/norge_entertainment.Rda")
  load("osmdata/norge_marketplace.Rda")
  # perform all spatial matching
  norge_shape$marketplace <- unlist(
    lapply(
      seq_len(
        nrow(norge_shape)
      ),
      function(x, ...) {
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_marketplace[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_entertainment[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_sport[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_clinic[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_hairdresser[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_shops[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_place_of_worship[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_retail[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_nursing_home[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_restaurant[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_aerodrome[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_office[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_platform[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_university[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_college[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_kindergarten[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_schools[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_bakeries[[norge_shape$kommunenum[x]]]
            )
          )
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
        length(
          unlist(
            st_intersects(
              norge_shape[x, ],
              norge_residential[[norge_shape$kommunenum[x]]]
            )
          )
        )
      }
    )
  )
  setDT(norge_shape)
  colnames(norge_shape)[1] <- "kommune_no"
  # merge it all together
  norge_complete <- merge(
    norge_no_shape,
    norge_shape,
    by = "kommune_no",
    all = FALSE
  )
  # remove needless variables
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
  # add the higher education variable
  no_geometry$higher_education <- no_geometry$college + no_geometry$university
  # safe the data
  write_csv(no_geometry, "wrangled_data/norge_features.csv")
  write_sf(
    st_as_sf(norge_complete)[!duplicated(norge_complete$kommune_no), ][, 1],
    "wrangled_data/shapes_norge.shp"
  )
  # load the data
  norge_features <- read_csv("wrangled_data/norge_features.csv")
}
# norge_features[, c(20:31, 36:53, 55)] <- 1000 * norge_features[, c(20:31, 36:53, 55)] / norge_features$population
# scale the data
norge_features[, c(20:31, 36:53, 55)] <- scale(
  norge_features[, c(20:31, 36:53, 55)]
)
# norge_features[, c(18, 19, 33, 34, 35)] <- norge_features[, c(18, 19, 33, 34, 35)] / 100
norge_features[, c(18, 19, 33, 34, 35)] <- scale(
  norge_features[, c(18, 19, 33, 34, 35)]
)
# load the shapefiles
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
# merge it together
norge <- merge(
  norge_features,
  norge_sf,
  by = "kommune_no"
)
# get the newest numbers
norway_municipality_confirmed_long$date <- as.Date(
  as.character(norway_municipality_confirmed_long$date)
)
max_date <- max(norway_municipality_confirmed_long$date)
date_diff <- as.numeric(max_date - as.Date("2021-03-24"))
date_seq <- seq(0, date_diff, 7)
numbers <- norway_municipality_confirmed_long[
  norway_municipality_confirmed_long$date %in% c(
    as.Date("2021-03-24") + date_seq[date_seq <= 28]
  ),
]
# remove needless variables
numbers$value <- NULL
numbers$time <- NULL
numbers$fylke_no <- NULL
numbers$fylke_name <- NULL
numbers$population <- NULL
numbers$kommune_name <- NULL
# merge all together
norge <- norge[
  norge$date %in% c(
    as.Date("2021-03-24") + date_seq[date_seq <= 28]
  ),
]
norge <- norge[order(norge$date, norge$kommune_no), ]#
numbers <- numbers[order(numbers$date, numbers$kommune_no), ]
numbers <- numbers[numbers$kommune_no %in% norge$kommune_no, ]
norge$date <- NULL
numbers <- cbind(
  numbers, norge
)
numbers <- st_as_sf(numbers)
st_crs(numbers) <- 4326
# add the area, densities and sex proportion
numbers$area <- as.numeric(set_units(st_area(numbers), km^2))
numbers$pop_dens <- numbers$population / numbers$area
numbers$urb_dens <- numbers$residential / numbers$area
numbers$sex <- numbers$population_female /
  numbers$population_total
# keep only relevant variables
numbers <- numbers[
  , c(1, 2, 5, 8:9, 18:20, 25:28, 34, 37:49, 52:61)
]
# impute
cols_imputed <- lapply(
  c(6:31, 33:36),
  function(x, ...) {
    vals <- numbers[, x]
    vals$geometry <- NULL
    vals <- unlist(vals)
    vals[is.na(vals)] <- median(vals, na.rm = TRUE)
    as.data.frame(vals)
  }
)
# bind everything together again
numbers_imputed <- Reduce(cbind, cols_imputed)
colnames(numbers_imputed) <- colnames(numbers)[c(6:31, 33:36)]
numbers_imputed <- cbind(numbers[, 1:5], numbers_imputed)
# turn it into a spatial frame
numbers <- st_as_sf(numbers_imputed)
rownames(numbers) <- NULL
# scale the remaining variables
numbers$pop_dens <- as.numeric(scale(numbers$pop_dens))
numbers$urb_dens <- as.numeric(scale(numbers$urb_dens))
numbers$sex <- as.numeric(scale(numbers$sex))
numbers$median_age <- as.numeric(scale(numbers$median_age))
numbers_28 <- numbers[numbers$date == max(numbers$date) - 28, ]
numbers_21 <- numbers[numbers$date == max(numbers$date) - 21, ]
numbers_14 <- numbers[numbers$date == max(numbers$date) - 14, ]
numbers_7 <- numbers[numbers$date == max(numbers$date) - 7, ]
numbers_0 <- numbers[numbers$date == max(numbers$date), ]
numbers_0$geometry <- NULL
rm(
  list = setdiff(
    ls(),
    c("numbers_0", "numbers_7", "numbers_14", "numbers_21", "numbers_28")
  )
)
