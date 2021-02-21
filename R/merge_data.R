##### GERMANY RESIDENTIAL NEW

library(readr)
library(data.table)
library(dplyr)
library(reshape2)
library(sf)
library(stringr)
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
norway_mobility[norway_mobility$fylke_name == "Troms og Finnmark fylke", ]$fylke_name <- "Troms og Finnmark"
norway_mobility <- setDT(norway_mobility)
norway_mobility_ungrouped <- dcast(
  norway_mobility,
  fylke_no + fylke_name + date ~ category,
  value.var = "mob_change"
)
norway_mobility_ungrouped$fylke_no <- as.character(norway_mobility_ungrouped$fylke_no)
norway_mobility_ungrouped[norway_mobility_ungrouped$fylke_no == "3", ]$fylke_no <- "03"
norway_municipality_confirmed_long$date <- as.Date(as.character(norway_municipality_confirmed_long$date))
norway_municipality_confirmed_mobility <- merge(
  norway_municipality_confirmed_long,
  norway_mobility_ungrouped,
  by = c("fylke_no", "fylke_name", "date"),
  all = FALSE
)
norge_shape <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
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
  norway_municipality_confirmed_mobility,
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
colnames(norge_no_shape)[44] <- "region"
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
load("osmdata/norge_bakery.Rda")
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
colnames(norge_complete)[9:14] <- c(
  "groc_pha",
  "parks",
  "resident",
  "ret_recr",
  "transit",
  "workplace"
)
no_geometry <- norge_complete
no_geometry$geometry <- NULL
no_geometry$higher_educ <- no_geometry$college + no_geometry$university
# calculate the SIR
write_csv(no_geometry, "wrangled_data/norge_features.csv")
write_sf(st_as_sf(norge_complete)[!duplicated(norge_complete$kommune_no), ][, 1], "wrangled_data/shapes_norge.shp")
###############################################
library(covid19germany)
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
germany_shape <- read_sf("shapefiles/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")
germany_shape <- st_transform(germany_shape, 4326)
germany_politics <- read_delim("germany_data/europawahl_2019.csv", delim = ";")[1:538, ]
germany_politics[str_detect(germany_politics$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_politics[str_detect(germany_politics$Stadt, "Berlin"),]$Kreis[1] <- "11000"
germany_politics <- germany_politics[germany_politics$Kreis %in% germany_shape$Kennziffer, ]
germany_unemployed <- read_delim("germany_data/arbeitslose_2019.csv", delim = ";")#[1:538, ]
germany_unemployed[str_detect(germany_unemployed$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_unemployed[str_detect(germany_unemployed$Stadt, "Berlin"),]$Kreis[1] <- "11000"
germany_unemployed <- germany_unemployed[germany_unemployed$Kreis %in% germany_shape$Kennziffer, ]
germany_protect <- read_delim("germany_data/schutzsuchende_2018.csv", delim = ";")#[1:538, ]
germany_protect[str_detect(germany_protect$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_protect[str_detect(germany_protect$Stadt, "Berlin"),]$Kreis[1] <- "11000"
germany_protect <- germany_protect[germany_protect$Kreis %in% germany_shape$Kennziffer, ]
germany_social <- read_delim("germany_data/sozialhilfe_2019.csv", delim = ";")#[1:538, ]
germany_social[str_detect(germany_social$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_social[str_detect(germany_social$Stadt, "Berlin"),]$Kreis[1] <- "11000"
germany_social <- germany_social[germany_social$Kreis %in% germany_shape$Kennziffer, ]
germany_company_tax <- read_delim("germany_data/gewerbesteuer_2015.csv", delim = ";")
germany_company_tax <- germany_company_tax[!is.na(germany_company_tax$Kreis), ]
germany_company_tax <- germany_company_tax[germany_company_tax$X1 == 2015, ]
germany_company_tax[str_detect(germany_company_tax$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_company_tax[str_detect(germany_company_tax$Stadt, "Berlin"),]$Kreis[1] <- "11000"
germany_company_tax <- germany_company_tax[germany_company_tax$Kreis %in% germany_shape$Kennziffer, ]
germany_income_tax <- read_delim("germany_data/einkommen_lohn_steuer_2016.csv", delim = ";")#[1:538, ]
germany_income_tax[str_detect(germany_income_tax$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_income_tax[str_detect(germany_income_tax$Stadt, "Berlin"),]$Kreis[1] <- "11000"
germany_income_tax <- germany_income_tax[germany_income_tax$Kreis %in% germany_shape$Kennziffer, ]
germany_asyl <- read_delim("germany_data/asylbewerberleistungen_2019.csv", delim = ";")#[1:538, ]
germany_asyl[str_detect(germany_asyl$Stadt, "Hamburg"),]$Kreis[1] <- "2000"
germany_asyl[str_detect(germany_asyl$Stadt, "Berlin"),]$Kreis[1] <- "11000"
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
germany <- germany[, c(1:5, 9, 11, 12, 15, 17:23, 25, 27:31, 33:39)]

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
load("osmdata/germany_bakery.Rda")
load("osmdata/germany_gas.Rda")
load("osmdata/germany_banks.Rda")
load("osmdata/germany_atms.Rda")
load("osmdata/germany_residential.Rda")
load("osmdata/germany_hairdresser.Rda")
load("osmdata/germany_toilets.Rda")
load("osmdata/germany_clinic.Rda")
load("osmdata/germany_sport.Rda")
load("osmdata/germany_entertainment.Rda")
load("osmdata/germany_marketplace.Rda")

germany_shape$supermarket <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...) {
      ifelse(
        nrow(germany_supermarket[[x]]) > 0,
        length(
          unlist(
            st_intersects(
              germany_shape[x, ], germany_supermarket[[x]]
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

germany_shape$gas <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...) {
      ifelse(
        nrow(germany_gas[[x]]) > 0,
        length(
          unlist(
            st_intersects(
              germany_shape[x, ], germany_gas[[x]]
            )
          )
        ),
        0
      )
    }
  )
)

germany_shape$banks <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...) {
      ifelse(
        nrow(germany_banks[[x]]) > 0,
        length(
          unlist(
            st_intersects(
              germany_shape[x, ], germany_banks[[x]]
            )
          )
        ),
        0
      )
    }
  )
)


germany_shape$atm <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...) {
      ifelse(
        nrow(germany_atms[[x]]) > 0,
        length(
          unlist(
            st_intersects(
              germany_shape[x, ], germany_atms[[x]]
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


library(stringr)
germany_confirmed[str_detect(germany_confirmed$Landkreis, "Berlin"), ]$Landkreis <- "SK Berlin"
germany_confirmed[str_detect(germany_confirmed$Landkreis, "Berlin"), ]$IdLandkreis <- 11000
berlin <- germany_confirmed[germany_confirmed$Landkreis == "SK Berlin", ]
berlin_grouped <- berlin[,
  .(
    Landkreis = "SK Berlin",
    NumberNewTestedIll = sum(NumberNewTestedIll),
    NumberNewDead = sum(NumberNewDead),
    NumberNewRecovered = sum(NumberNewRecovered),
    CumNumberTestedIll = sum(CumNumberTestedIll),
    CumNumberDead = sum(CumNumberDead),
    CumNumberRecovered = sum(CumNumberRecovered),
    IdLandkreis = 11000
  ),
  by = list(Date)
]
berlin_grouped <- berlin_grouped[order(berlin_grouped$Date)]
for (i in 2:nrow(berlin_grouped)) {
  berlin_grouped[i, ]$CumNumberTestedIll <- berlin_grouped[i - 1, ]$CumNumberTestedIll + berlin_grouped[i, ]$NumberNewTestedIll
  berlin_grouped[i, ]$CumNumberDead <- berlin_grouped[i - 1, ]$CumNumberDead + berlin_grouped[i, ]$NumberNewDead
  berlin_grouped[i, ]$CumNumberRecovered <- berlin_grouped[i - 1, ]$CumNumberRecovered + berlin_grouped[i, ]$NumberNewRecovered
}
berlin_grouped
germany_confirmed <- germany_confirmed[germany_confirmed$Landkreis != "SK Berlin", ]
germany_confirmed <- rbind(germany_confirmed, berlin_grouped)
colnames(germany_confirmed)[9] <- "Kennziffer"
germany_complete <- merge(
  germany_shape,
  germany_confirmed,
  by = "Kennziffer",
  all = FALSE
)
write_sf(st_as_sf(germany_complete), "wrangled_data/germany_complete.shp")

no_geometry <- germany_complete
no_geometry$geometry <- NULL
# calculate the SIR
write_csv(no_geometry, "wrangled_data/germany_features.csv")
write_sf(st_as_sf(germany_complete)[!duplicated(germany_complete$Kennziffer), ][, 1], "wrangled_data/shapes_germany.shp")
