library(readr)
library(data.table)
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
norge_demo <- read_delim("norge_data/Personer1.csv", ";")
norge_demo$kommune_no <- str_extract(norge_demo$region, "[0-9]{4}")
norge_no_shape <- merge(
  norway_municipality_confirmed_mobility,
  norge_demo,
  by = "kommune_no"
)
load("osmdata/norge_hospital.Rda")
load("osmdata/norge_place_of_worship.Rda")
load("osmdata/norge_retail.Rda")
load("osmdata/norge_nursing_home.Rda")
load("osmdata/norge_restaurant.Rda")
load("osmdata/norge_terminal.Rda")
load("osmdata/norge_aerodrome.Rda")
load("osmdata/norge_office.Rda")
load("osmdata/norge_supermarket.Rda")
load("osmdata/norge_platform.Rda")
load("osmdata/norge_university.Rda")
load("osmdata/norge_college.Rda")
load("osmdata/norge_kindergarten.Rda")
load("osmdata/norge_schools.Rda")

norge_shape$supermarket <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
      ),
    function(x, ...)
      ifelse(
        nrow(norge_supermarket[[x]]) > 0,
        length(
          unlist(
            st_intersects(
              norge_shape[x, ], norge_supermarket[[x]]
            )
          )
        ),
        0
      )
  )
)

norge_shape$place_of_worship <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$retail <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$nursing_home <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$restaurant <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$terminal <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$aerodrome <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$office <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$platform <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$university <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$college <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$kindergarten <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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
  )
)

norge_shape$schools <- unlist(
  lapply(
    seq_len(
      nrow(norge_shape)
    ),
    function(x, ...)
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

write_sf(st_as_sf(norge_complete), "wrangled_data/norge_complete.shp")
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
load("osmdata/germany_hospital.Rda")
load("osmdata/germany_place_of_worship.Rda")
load("osmdata/germany_retail.Rda")
load("osmdata/germany_nursing_home.Rda")
load("osmdata/germany_restaurant.Rda")
load("osmdata/germany_terminal.Rda")
load("osmdata/germany_aerodrome.Rda")
load("osmdata/germany_office.Rda")
load("osmdata/germany_supermarket.Rda")
load("osmdata/germany_platform.Rda")
load("osmdata/germany_university.Rda")
load("osmdata/germany_college.Rda")
load("osmdata/germany_kindergarten.Rda")
load("osmdata/germany_schools.Rda")

germany_shape$supermarket <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$place_of_worship <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$retail <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$nursing_home <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$restaurant <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$terminal <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$aerodrome <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$office <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$platform <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$university <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$college <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$kindergarten <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
  )
)

germany_shape$schools <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...)
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
       IdLandkreis = 11000),
       by=list(Date)]
berlin_grouped <- berlin_grouped[order(berlin_grouped$Date)]
for (i in 2:nrow(berlin_grouped)) {
  berlin_grouped[i, ]$CumNumberTestedIll <- berlin_grouped[i - 1, ]$CumNumberTestedIll + berlin_grouped[i, ]$NumberNewTestedIll
  berlin_grouped[i, ]$CumNumberDead <- berlin_grouped[i - 1, ]$CumNumberDead + berlin_grouped[i, ]$NumberNewDead
  berlin_grouped[i, ]$CumNumberRecovered <- berlin_grouped[i - 1, ]$CumNumberRecovered + berlin_grouped[i, ]$NumberNewRecovered
}
berlin_grouped
germany_confirmed <- germany_confirmed[germany_confirmed$Landkreis != "SK Berlin",]
germany_confirmed <- rbind(germany_confirmed, berlin_grouped)
colnames(germany_confirmed)[9] <- "Kennziffer"
germany_complete <- merge(
  germany_shape,
  germany_confirmed,
  by = "Kennziffer",
  all = FALSE
)
write_sf(st_as_sf(germany_complete), "wrangled_data/germany_complete.shp")
