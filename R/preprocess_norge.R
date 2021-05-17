library(data.table)
library(readr)
library(dplyr)
library(reshape2)
library(sf)
library(SpatialEpi)
library(stringr)
library(units)
library(regclass)
library(MASS)
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
norge_shape <- read_sf("norge_data/shapefile_norway.shp")
cols <- unlist(read_csv("norge_data/colnames_shapefile.csv"))
colnames(norge_shape) <- c(cols[cols != "geometry"], "geometry")
# read the last safed data
norge_features <- read_csv("wrangled_data/norge_features.csv")
# check if there is newer data available
date_1 <- max(as.Date(as.character(norway_municipality_confirmed_long$date)))
date_2 <- max(as.Date(norge_features$date))
if (date_1 != date_2) {
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
  # load the data
  norge_features <- read_csv("wrangled_data/norge_features.csv")
}
# scale the data
norge_features[, c(17:31, 33:53, 55)] <- scale(
  norge_features[, c(17:31, 33:53, 55)]
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
# newest_numbers <- norway_municipality_confirmed_long[
#   norway_municipality_confirmed_long$date == max(
#     norway_municipality_confirmed_long$date
#   ),
# ]
newest_numbers <- norway_municipality_confirmed_long[
  norway_municipality_confirmed_long$date == as.Date(
    "2021-05-02"
  ),
]
# remove needless variables
newest_numbers$value <- NULL
newest_numbers$time <- NULL
newest_numbers$fylke_no <- NULL
newest_numbers$fylke_name <- NULL
newest_numbers$population <- NULL
newest_numbers$date <- NULL
newest_numbers$kommune_name <- NULL
# merge all together
newest_numbers <- merge(
  newest_numbers,
  norge[norge$date == max(norge$date), ],
  by = "kommune_no"
)
newest_numbers$date <- as.Date(
  "2021-05-02"
)
# calculate the expected count
expected_count <- expected(
  population = newest_numbers$population,
  cases = newest_numbers$value,
  n.strata = 1
)
# add new variables
newest_numbers$expected_count <- expected_count
# calculate the SIR
newest_numbers$sir <- newest_numbers$value / newest_numbers$expected_count
newest_numbers <- st_as_sf(newest_numbers)
st_crs(newest_numbers) <- 4326
# calculate the proportion of infected people
newest_numbers$inf_rate <- newest_numbers$value / newest_numbers$population
# add id area variables
newest_numbers$idarea_1 <- seq_len(nrow(newest_numbers))
newest_numbers$idarea_2 <- seq_len(nrow(newest_numbers))
# add the area, densities and sex proportion
newest_numbers$area <- as.numeric(set_units(st_area(newest_numbers), km^2))
newest_numbers$pop_dens <- newest_numbers$population / newest_numbers$area
newest_numbers$urb_dens <- newest_numbers$residential / newest_numbers$area
newest_numbers$sex <- newest_numbers$population_female /
  newest_numbers$population_total
# keep only relevant variables
newest_numbers <- newest_numbers[
  , c(1, 3, 6:8, 17:19, 24:27, 32:33, 36:48, 51:65)
]
# impute
cols_imputed <- lapply(
  c(6:34, 36:42),
  function(x, ...) {
    vals <- newest_numbers[, x]
    vals$geometry <- NULL
    vals <- unlist(vals)
    vals[is.na(vals)] <- median(vals, na.rm = TRUE)
    as.data.frame(vals)
  }
)
# bind everything together again
newest_numbers_imputed <- Reduce(cbind, cols_imputed)
colnames(newest_numbers_imputed) <- colnames(newest_numbers)[c(6:34, 36:42)]
newest_numbers_imputed <- cbind(newest_numbers[, 1:5], newest_numbers_imputed)
# turn it into a spatial frame
newest_numbers <- st_as_sf(newest_numbers_imputed)
newest_numbers$region <- NULL
rownames(newest_numbers) <- NULL
# scale the remaining variables
newest_numbers$pop_dens <- as.numeric(scale(newest_numbers$pop_dens))
newest_numbers$urb_dens <- as.numeric(scale(newest_numbers$urb_dens))
newest_numbers$sex <- as.numeric(scale(newest_numbers$sex))
newest_numbers$median_age <- as.numeric(scale(newest_numbers$median_age))
vaccines <- read_delim("norge_data/vaccination_norway_2nd_may.csv", delim = ";")
colnames(vaccines) <- str_remove(colnames(vaccines), "Covid-19, ")
colnames(vaccines)[1] <- "date"
missing <- colnames(vaccines)[
  !colnames(vaccines) %in% unique(norge$kommune_name)
]
sort(missing)
# change colnames to correct names
vaccines[, colnames(vaccines) == missing[3]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[2]] <- "Tana"
vaccines[, colnames(vaccines) == missing[4]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[5]] <- "Kåfjord"
vaccines[, colnames(vaccines) == missing[7]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[6]] <- "Kautokeino"
vaccines[, colnames(vaccines) == missing[10]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[11]] <- "Karasjok"
vaccines[, colnames(vaccines) == missing[14]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[15]] <- "Porsanger"
vaccines[, colnames(vaccines) == missing[17]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[18]] <- "Snåsa"
vaccines[, colnames(vaccines) == missing[21]] <- NULL
colnames(vaccines)[colnames(vaccines) == missing[22]] <- "Nesseby"
colnames(vaccines)[colnames(vaccines) == missing[12]] <- "Os"
colnames(vaccines)[colnames(vaccines) == missing[13]] <- "Oslo"
colnames(vaccines)[colnames(vaccines) == missing[16]] <- "Røyrvik"
# load the data for heroy
heroy_more <- read_delim(
  "norge_data/vaccination_heroy_more_2nd_may.csv",
  delim = ";"
)
heroy_nordland <- read_delim(
  "norge_data/vaccination_heroy_nordland_2nd_may.csv",
  delim = ";"
)
vaccines$`Herøy (Møre og Romsdal)` <- heroy_more$`Covid-19, Herøy`
vaccines$`Herøy (Nordland)` <- heroy_nordland$`Covid-19, Herøy`
vaccines$Herøy <- NULL
vaccines$`Ikke oppgitt` <- NULL
vaccines$Svalbard <- NULL
vaccines$`Tysfjord *` <- NULL
# change the date variable
vaccines$date <- as.Date(
  str_replace_all(vaccines$date, "\\.", "-"),
  format = "%d-%m-%Y"
)
# use cumulative sums
vaccines[, 2:357] <- cumsum(vaccines[, 2:357])
# get long format
vaccine_shots <- melt(
  setDT(vaccines),
  id.vars = "date",
  variable.name = "kommune_name"
)
# merge it
newest_numbers <- merge(
  newest_numbers,
  vaccine_shots,
  by = c("date", "kommune_name")
)
colnames(newest_numbers)[41] <- "vaccine_shots"
colnames(newest_numbers)[5] <- "value"
newest_numbers$vaccine_shots <- newest_numbers$vaccine_shots /
  newest_numbers$population
newest_numbers$vaccine_shots <- scale(newest_numbers$vaccine_shots)[, 1]
b <- newest_numbers
b$geometry <- NULL
b <- b[, c(5:29, 31, 38:41)]
sign <- TRUE
while (sign) {
  mod <- glm.nb(
    value ~ .,
    data = b
  )
  if (!any(VIF(mod) > 5)) {
    sign <- FALSE
  } else {
    b[, names(VIF(mod))[VIF(mod) == max(VIF(mod))]] <- NULL
  }
}
newest_numbers[, c(5:29, 31, 38:41)] <- NULL
newest_numbers <- st_as_sf(cbind(b, newest_numbers))

rm(list = setdiff(ls(), c("newest_numbers", "backup")))
