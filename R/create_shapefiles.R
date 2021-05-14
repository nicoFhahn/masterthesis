library(sf)
norge_shape <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
kommune_4602 <- norge_shape[norge_shape$kommunenum == 4602, ][1, ]
kommune_4602$geometry <- st_union(
  norge_shape[norge_shape$kommunenum == 4602, ]
)
# remove the two data points
norge_shape <- norge_shape[norge_shape$kommunenum != 4602, ]
# add the new data
norge_shape <- rbind(norge_shape, kommune_4602)
# load the osm data
load("osmdata/norge_hospital.Rda")
load("osmdata/norge_place_of_worship.Rda")
load("osmdata/norge_retail.Rda")
load("osmdata/norge_nursing_home.Rda")
load("osmdata/norge_restaurant.Rda")
load("osmdata/norge_aerodrome.Rda")
norge_aerodrome[unlist(lapply(norge_aerodrome, nrow)) > 1] <- lapply(
  norge_aerodrome[unlist(lapply(norge_aerodrome, nrow)) > 1],
  function(x) {
    coordinates <- st_coordinates(x)
    distances <- distHaversine(coordinates)
    if (any(distances <= 1000)) {
      dups_exist <- TRUE
      i <- 1
      while(dups_exist) {
        if(length(which(distances <= 1000)) > 0) {
          x <- x[-(which(distances <= 1000) + i), ]
        }
        coordinates <- st_coordinates(x[(i + 1):nrow(x), ])
        if (nrow(coordinates) > 1 & !is.na(coordinates[1, ])[1]) {
          distances <- distHaversine(coordinates)
          i <- i + 1
        } else {
          dups_exist <- FALSE
        }
      }
    }
    x
  }
)
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
colnames(norge_shape)[1] <- "kommune_no"
cols <- tibble(
  colnames = colnames(norge_shape)
)
write_csv(cols, "norge_data/colnames_shapefile.csv")
write_sf(norge_shape, "norge_data/shapefile_norway.shp")
# load the shape file for germany
germany_shape <- read_sf("shapefiles/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")
# transform it
germany_shape <- st_transform(germany_shape, 4326)
# load all the osm data
load("osmdata/germany_hospital.Rda")
load("osmdata/germany_place_of_worship.Rda")
load("osmdata/germany_retail.Rda")
load("osmdata/germany_nursing_home.Rda")
load("osmdata/germany_restaurant.Rda")
load("osmdata/germany_aerodrome.Rda")
germany_aerodrome[unlist(lapply(germany_aerodrome, nrow)) > 1] <- lapply(
  germany_aerodrome[unlist(lapply(germany_aerodrome, nrow)) > 1],
  function(x) {
    coordinates <- st_coordinates(x)
    distances <- distHaversine(coordinates)
    if (any(distances <= 1000)) {
      dups_exist <- TRUE
      i <- 1
      while(dups_exist) {
        if(length(which(distances <= 1000)) > 0) {
          x <- x[-(which(distances <= 1000) + i), ]
        }
        coordinates <- st_coordinates(x[(i + 1):nrow(x), ])
        if (nrow(coordinates) > 1 & !is.na(coordinates[1, ])[1]) {
          distances <- distHaversine(coordinates)
          i <- i + 1
        } else {
          dups_exist <- FALSE
        }
      }
    }
    x
  }
)
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
cols <- tibble(
  colnames = colnames(germany_shape)
)
write_csv(cols, "germany_data/colnames_shapefile.csv")
write_sf(germany_shape, "germany_data/shapefile_germany.shp")