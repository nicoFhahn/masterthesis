# this is the script used to create the shapefiles that are used during
# preprocessing
library(geosphere)
library(sf)
library(tibble)
# load the shapefile for norway
norge_shape <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
# this kommune is duplicated
kommune_4602 <- norge_shape[norge_shape$kommunenum == 4602, ][1, ]
kommune_4602$geometry <- st_union(
  norge_shape[norge_shape$kommunenum == 4602, ]
)
# remove the two data points
norge_shape <- norge_shape[norge_shape$kommunenum != 4602, ]
# add the new data
norge_shape <- rbind(norge_shape, kommune_4602)
# load the osm data amd remove duplicates
load("osmdata/norge_hospital.Rda")
norge_hospital <- st_as_sf(dplyr::bind_rows(norge_hospital))
norge_hospital <- norge_hospital[
  !duplicated(norge_hospital$osm_id, incomparables = NA),
]
norge_hospital <- norge_hospital[
  !rownames(norge_hospital) %in% rownames(norge_hospital[
    is.na(norge_hospital$osm_id),
  ][
    duplicated(st_coordinates(norge_hospital[
      is.na(norge_hospital$osm_id),
    ])),
  ]),
]
norge_hospital$type <- "hospital"
load("osmdata/norge_place_of_worship.Rda")
norge_place_of_worship <- st_as_sf(dplyr::bind_rows(norge_place_of_worship))
norge_place_of_worship <- norge_place_of_worship[
  !duplicated(norge_place_of_worship$osm_id, incomparables = NA),
]
norge_place_of_worship <- norge_place_of_worship[
  !rownames(norge_place_of_worship) %in% rownames(norge_place_of_worship[
    is.na(norge_place_of_worship$osm_id),
  ][
    duplicated(st_coordinates(norge_place_of_worship[
      is.na(norge_place_of_worship$osm_id),
    ])),
  ]),
]
norge_place_of_worship$type <- "place of worship"
load("osmdata/norge_retail.Rda")
norge_retail <- st_as_sf(dplyr::bind_rows(norge_retail))
norge_retail <- norge_retail[
  !duplicated(norge_retail$osm_id, incomparables = NA),
]
norge_retail <- norge_retail[
  !rownames(norge_retail) %in% rownames(norge_retail[
    is.na(norge_retail$osm_id),
  ][
    duplicated(st_coordinates(norge_retail[
      is.na(norge_retail$osm_id),
    ])),
  ]),
]
norge_retail$type <- "retail"
load("osmdata/norge_nursing_home.Rda")
norge_nursing_home <- st_as_sf(do.call(rbind, norge_nursing_home))
norge_nursing_home <- norge_nursing_home[
  !duplicated(norge_nursing_home$osm_id, incomparables = NA),
]
norge_nursing_home <- norge_nursing_home[
  !rownames(norge_nursing_home) %in% rownames(norge_nursing_home[
    is.na(norge_nursing_home$osm_id),
  ][
    duplicated(st_coordinates(norge_nursing_home[
      is.na(norge_nursing_home$osm_id),
    ])),
  ]),
]
norge_nursing_home$type <- "nursing home"
load("osmdata/norge_restaurant.Rda")
norge_restaurant <- st_as_sf(dplyr::bind_rows(norge_restaurant))
norge_restaurant <- norge_restaurant[
  !duplicated(norge_restaurant$osm_id, incomparables = NA),
]
norge_restaurant <- norge_restaurant[
  !rownames(norge_restaurant) %in% rownames(norge_restaurant[
    is.na(norge_restaurant$osm_id),
  ][
    duplicated(st_coordinates(norge_restaurant[
      is.na(norge_restaurant$osm_id),
    ])),
  ]),
]
norge_restaurant$type <- "restaurant"
load("osmdata/norge_aerodrome.Rda")
norge_aerodrome[unlist(lapply(norge_aerodrome, nrow)) > 1] <- lapply(
  norge_aerodrome[unlist(lapply(norge_aerodrome, nrow)) > 1],
  function(x) {
    coordinates <- st_coordinates(x)
    distances <- distHaversine(coordinates)
    if (any(distances <= 1000)) {
      dups_exist <- TRUE
      i <- 1
      while (dups_exist) {
        if (length(which(distances <= 1000)) > 0) {
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
norge_aerodrome <- st_as_sf(do.call(rbind, norge_aerodrome))
norge_aerodrome <- norge_aerodrome[
  !duplicated(norge_aerodrome$osm_id, incomparables = NA),
]
norge_aerodrome <- norge_aerodrome[
  !rownames(norge_aerodrome) %in% rownames(norge_aerodrome[
    is.na(norge_aerodrome$osm_id),
  ][
    duplicated(st_coordinates(norge_aerodrome[
      is.na(norge_aerodrome$osm_id),
    ])),
  ]),
]
norge_aerodrome$type <- "aerodrome"
load("osmdata/norge_office.Rda")
norge_office <- st_as_sf(dplyr::bind_rows(norge_office))
norge_office <- norge_office[
  !duplicated(norge_office$osm_id, incomparables = NA),
]
norge_office <- norge_office[
  !rownames(norge_office) %in% rownames(norge_office[
    is.na(norge_office$osm_id),
  ][
    duplicated(st_coordinates(norge_office[
      is.na(norge_office$osm_id),
    ])),
  ]),
]
norge_office$type <- "office"
load("osmdata/norge_shops.Rda")
norge_shops <- st_as_sf(dplyr::bind_rows(norge_shops))
norge_shops <- norge_shops[
  !duplicated(norge_shops$osm_id, incomparables = NA),
]
norge_shops <- norge_shops[
  !rownames(norge_shops) %in% rownames(norge_shops[
    is.na(norge_shops$osm_id),
  ][
    duplicated(st_coordinates(norge_shops[
      is.na(norge_shops$osm_id),
    ])),
  ]),
]
norge_shops$type <- "shop"
load("osmdata/norge_platform.Rda")
norge_platform <- st_as_sf(dplyr::bind_rows(norge_platform))
norge_platform <- norge_platform[
  !duplicated(norge_platform$osm_id, incomparables = NA),
]
norge_platform <- norge_platform[
  !rownames(norge_platform) %in% rownames(norge_platform[
    is.na(norge_platform$osm_id),
  ][
    duplicated(st_coordinates(norge_platform[
      is.na(norge_platform$osm_id),
    ])),
  ]),
]
norge_platform$type <- "platform"
load("osmdata/norge_university.Rda")
norge_university <- st_as_sf(do.call(rbind, norge_university))
norge_university <- norge_university[
  !duplicated(norge_university$osm_id, incomparables = NA),
]
norge_university <- norge_university[
  !rownames(norge_university) %in% rownames(norge_university[
    is.na(norge_university$osm_id),
  ][
    duplicated(st_coordinates(norge_university[
      is.na(norge_university$osm_id),
    ])),
  ]),
]
norge_university$type <- "higher education"
load("osmdata/norge_college.Rda")
norge_college <- st_as_sf(do.call(rbind, norge_college))
norge_college <- norge_college[
  !duplicated(norge_college$osm_id, incomparables = NA),
]
norge_college <- norge_college[
  !rownames(norge_college) %in% rownames(norge_college[
    is.na(norge_college$osm_id),
  ][
    duplicated(st_coordinates(norge_college[
      is.na(norge_college$osm_id),
    ])),
  ]),
]
norge_college$type <- "higher education"
load("osmdata/norge_kindergarten.Rda")
norge_kindergarten <- st_as_sf(dplyr::bind_rows(norge_kindergarten))
norge_kindergarten <- norge_kindergarten[
  !duplicated(norge_kindergarten$osm_id, incomparables = NA),
]
norge_kindergarten <- norge_kindergarten[
  !rownames(norge_kindergarten) %in% rownames(norge_kindergarten[
    is.na(norge_kindergarten$osm_id),
  ][
    duplicated(st_coordinates(norge_kindergarten[
      is.na(norge_kindergarten$osm_id),
    ])),
  ]),
]
norge_kindergarten$type <- "kindergarten"
load("osmdata/norge_schools.Rda")
norge_schools <- st_as_sf(dplyr::bind_rows(norge_schools))
norge_schools <- norge_schools[
  !duplicated(norge_schools$osm_id, incomparables = NA),
]
norge_schools <- norge_schools[
  !rownames(norge_schools) %in% rownames(norge_schools[
    is.na(norge_schools$osm_id),
  ][
    duplicated(st_coordinates(norge_schools[
      is.na(norge_schools$osm_id),
    ])),
  ]),
]
norge_schools$type <- "school"
load("osmdata/norge_bakery.Rda")
norge_bakeries <- st_as_sf(do.call(rbind, norge_bakeries))
norge_bakeries <- norge_bakeries[
  !duplicated(norge_bakeries$osm_id, incomparables = NA),
]
norge_bakeries <- norge_bakeries[
  !rownames(norge_bakeries) %in% rownames(norge_bakeries[
    is.na(norge_bakeries$osm_id),
  ][
    duplicated(st_coordinates(norge_bakeries[
      is.na(norge_bakeries$osm_id),
    ])),
  ]),
]
norge_bakeries$type <- "bakery"
load("osmdata/norge_hairdresser.Rda")
norge_hairdresser <- st_as_sf(dplyr::bind_rows(norge_hairdresser))
norge_hairdresser <- norge_hairdresser[
  !duplicated(norge_hairdresser$osm_id, incomparables = NA),
]
norge_hairdresser <- norge_hairdresser[
  !rownames(norge_hairdresser) %in% rownames(norge_hairdresser[
    is.na(norge_hairdresser$osm_id),
  ][
    duplicated(st_coordinates(norge_hairdresser[
      is.na(norge_hairdresser$osm_id),
    ])),
  ]),
]
norge_hairdresser$type <- "hairdresser"
load("osmdata/norge_clinic.Rda")
norge_clinic <- st_as_sf(dplyr::bind_rows(norge_clinic))
norge_clinic <- norge_clinic[
  !duplicated(norge_clinic$osm_id, incomparables = NA),
]
norge_clinic <- norge_clinic[
  !rownames(norge_clinic) %in% rownames(norge_clinic[
    is.na(norge_clinic$osm_id),
  ][
    duplicated(st_coordinates(norge_clinic[
      is.na(norge_clinic$osm_id),
    ])),
  ]),
]
norge_clinic$type <- "clinic"
load("osmdata/norge_sport.Rda")
norge_sport <- st_as_sf(dplyr::bind_rows(norge_sport))
norge_sport <- norge_sport[
  !duplicated(norge_sport$osm_id, incomparables = NA),
]
norge_sport <- norge_sport[
  !rownames(norge_sport) %in% rownames(norge_sport[
    is.na(norge_sport$osm_id),
  ][
    duplicated(st_coordinates(norge_sport[
      is.na(norge_sport$osm_id),
    ])),
  ]),
]
norge_sport$type <- "sport"
load("osmdata/norge_entertainment.Rda")
norge_entertainment <- st_as_sf(dplyr::bind_rows(norge_entertainment))
norge_entertainment <- norge_entertainment[
  !duplicated(norge_entertainment$osm_id, incomparables = NA),
]
norge_entertainment <- norge_entertainment[
  !rownames(norge_entertainment) %in% rownames(norge_entertainment[
    is.na(norge_entertainment$osm_id),
  ][
    duplicated(st_coordinates(norge_entertainment[
      is.na(norge_entertainment$osm_id),
    ])),
  ]),
]
norge_entertainment$type <- "entertainment"
load("osmdata/norge_marketplace.Rda")
norge_marketplace <- st_as_sf(do.call(rbind, norge_marketplace))
norge_marketplace <- norge_marketplace[
  !duplicated(norge_marketplace$osm_id, incomparables = NA),
]
norge_marketplace <- norge_marketplace[
  !rownames(norge_marketplace) %in% rownames(norge_marketplace[
    is.na(norge_marketplace$osm_id),
  ][
    duplicated(st_coordinates(norge_marketplace[
      is.na(norge_marketplace$osm_id),
    ])),
  ]),
]
norge_marketplace$type <- "marketplace"
load("osmdata/norge_residential.Rda")
norge_residential <- st_as_sf(dplyr::bind_rows(norge_residential))
norge_residential <- norge_residential[
  !duplicated(norge_residential$osm_id, incomparables = NA),
]
norge_residential <- norge_residential[
  !rownames(norge_residential) %in% rownames(norge_residential[
    is.na(norge_residential$osm_id),
  ][
    duplicated(st_coordinates(norge_residential[
      is.na(norge_residential$osm_id),
    ])),
  ]),
]
norge_residential$type <- "residential"
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
            norge_marketplace
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
            norge_entertainment
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
            norge_sport
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
            norge_clinic
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
            norge_hairdresser
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
            norge_shops
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
            norge_place_of_worship
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
            norge_retail
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
            norge_nursing_home
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
            norge_restaurant
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
            norge_aerodrome
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
            norge_office
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
            norge_platform
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
            norge_university
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
            norge_college
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
            norge_kindergarten
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
            norge_schools
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
            norge_bakeries
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
            norge_residential
          )
        )
      )
    }
  )
)
norge_all <- dplyr::bind_rows(
  norge_aerodrome,
  norge_bakeries,
  norge_clinic,
  norge_college,
  norge_entertainment,
  norge_hairdresser,
  norge_hospital,
  norge_kindergarten,
  norge_marketplace,
  norge_nursing_home,
  norge_office,
  norge_place_of_worship,
  norge_platform,
  norge_residential,
  norge_restaurant,
  norge_retail,
  norge_schools,
  norge_shops,
  norge_sport,
  norge_university
)
norge_all_coordinates <- st_coordinates(norge_all)
norge_all$geometry <- NULL
norge_all$latitude <- norge_all_coordinates[, 2]
norge_all$longitude <- norge_all_coordinates[, 1]
readr::write_csv(norge_all, "norge_data/norge_all.csv")
colnames(norge_shape)[1] <- "kommune_no"
cols <- tibble(
  colnames = colnames(norge_shape)
)
write_csv(cols, "norge_data/colnames_shapefile.csv")
write_sf(norge_shape, "norge_data/shapefile_norway.shp")
rm(list = ls())
# load the shape file for germany
germany_shape <- read_sf("shapefiles/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")
# transform it
germany_shape <- st_transform(germany_shape, 4326)
# load all the osm data
load("osmdata/germany_hospital.Rda")
germany_hospital <- st_as_sf(dplyr::bind_rows(germany_hospital))
germany_hospital <- germany_hospital[
  !duplicated(germany_hospital$osm_id, incomparables = NA),
]
germany_hospital <- germany_hospital[
  !rownames(germany_hospital) %in% rownames(germany_hospital[
    is.na(germany_hospital$osm_id),
  ][
    duplicated(st_coordinates(germany_hospital[
      is.na(germany_hospital$osm_id),
    ])),
  ]),
]
germany_hospital$type <- "hospital"
load("osmdata/germany_place_of_worship.Rda")
germany_place_of_worship <- st_as_sf(dplyr::bind_rows(germany_place_of_worship))
germany_place_of_worship <- germany_place_of_worship[
  !duplicated(germany_place_of_worship$osm_id, incomparables = NA),
]
germany_place_of_worship <- germany_place_of_worship[
  !rownames(germany_place_of_worship) %in% rownames(germany_place_of_worship[
    is.na(germany_place_of_worship$osm_id),
  ][
    duplicated(st_coordinates(germany_place_of_worship[
      is.na(germany_place_of_worship$osm_id),
    ])),
  ]),
]
germany_place_of_worship$type <- "place of worship"
load("osmdata/germany_retail.Rda")
germany_retail <- st_as_sf(dplyr::bind_rows(germany_retail))
germany_retail <- germany_retail[
  !duplicated(germany_retail$osm_id, incomparables = NA),
]
germany_retail <- germany_retail[
  !rownames(germany_retail) %in% rownames(germany_retail[
    is.na(germany_retail$osm_id),
  ][
    duplicated(st_coordinates(germany_retail[
      is.na(germany_retail$osm_id),
    ])),
  ]),
]
germany_retail$type <- "retail"
load("osmdata/germany_nursing_home.Rda")
germany_nursing_home <- st_as_sf(do.call(rbind, germany_nursing_home))
germany_nursing_home <- germany_nursing_home[
  !duplicated(germany_nursing_home$osm_id, incomparables = NA),
]
germany_nursing_home <- germany_nursing_home[
  !rownames(germany_nursing_home) %in% rownames(germany_nursing_home[
    is.na(germany_nursing_home$osm_id),
  ][
    duplicated(st_coordinates(germany_nursing_home[
      is.na(germany_nursing_home$osm_id),
    ])),
  ]),
]
germany_nursing_home$type <- "nursing home"
load("osmdata/germany_restaurant.Rda")
germany_restaurant <- st_as_sf(dplyr::bind_rows(germany_restaurant))
germany_restaurant <- germany_restaurant[
  !duplicated(germany_restaurant$osm_id, incomparables = NA),
]
germany_restaurant <- germany_restaurant[
  !rownames(germany_restaurant) %in% rownames(germany_restaurant[
    is.na(germany_restaurant$osm_id),
  ][
    duplicated(st_coordinates(germany_restaurant[
      is.na(germany_restaurant$osm_id),
    ])),
  ]),
]
germany_restaurant$type <- "restaurant"
load("osmdata/germany_aerodrome.Rda")
germany_aerodrome[unlist(lapply(germany_aerodrome, nrow)) > 1] <- lapply(
  germany_aerodrome[unlist(lapply(germany_aerodrome, nrow)) > 1],
  function(x) {
    coordinates <- st_coordinates(x)
    distances <- distHaversine(coordinates)
    if (any(distances <= 1000)) {
      dups_exist <- TRUE
      i <- 1
      while (dups_exist) {
        if (length(which(distances <= 1000)) > 0) {
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
germany_aerodrome <- st_as_sf(do.call(rbind, germany_aerodrome))
germany_aerodrome <- germany_aerodrome[
  !duplicated(germany_aerodrome$osm_id, incomparables = NA),
]
germany_aerodrome <- germany_aerodrome[
  !rownames(germany_aerodrome) %in% rownames(germany_aerodrome[
    is.na(germany_aerodrome$osm_id),
  ][
    duplicated(st_coordinates(germany_aerodrome[
      is.na(germany_aerodrome$osm_id),
    ])),
  ]),
]
germany_aerodrome$type <- "aerodrome"
load("osmdata/germany_office.Rda")
germany_office <- st_as_sf(dplyr::bind_rows(germany_office))
germany_office <- germany_office[
  !duplicated(germany_office$osm_id, incomparables = NA),
]
germany_office <- germany_office[
  !rownames(germany_office) %in% rownames(germany_office[
    is.na(germany_office$osm_id),
  ][
    duplicated(st_coordinates(germany_office[
      is.na(germany_office$osm_id),
    ])),
  ]),
]
germany_office$type <- "office"
load("osmdata/germany_shops.Rda")
germany_shops <- st_as_sf(dplyr::bind_rows(germany_shops))
germany_shops <- germany_shops[
  !duplicated(germany_shops$osm_id, incomparables = NA),
]
germany_shops <- germany_shops[
  !rownames(germany_shops) %in% rownames(germany_shops[
    is.na(germany_shops$osm_id),
  ][
    duplicated(st_coordinates(germany_shops[
      is.na(germany_shops$osm_id),
    ])),
  ]),
]
germany_shops$type <- "shop"
load("osmdata/germany_platform.Rda")
germany_platform <- st_as_sf(dplyr::bind_rows(germany_platform))
germany_platform <- germany_platform[
  !duplicated(germany_platform$osm_id, incomparables = NA),
]
germany_platform <- germany_platform[
  !rownames(germany_platform) %in% rownames(germany_platform[
    is.na(germany_platform$osm_id),
  ][
    duplicated(st_coordinates(germany_platform[
      is.na(germany_platform$osm_id),
    ])),
  ]),
]
germany_platform$type <- "platform"
load("osmdata/germany_university.Rda")
germany_university <- st_as_sf(do.call(rbind, germany_university))
germany_university <- germany_university[
  !duplicated(germany_university$osm_id, incomparables = NA),
]
germany_university <- germany_university[
  !rownames(germany_university) %in% rownames(germany_university[
    is.na(germany_university$osm_id),
  ][
    duplicated(st_coordinates(germany_university[
      is.na(germany_university$osm_id),
    ])),
  ]),
]
germany_university$type <- "higher education"
load("osmdata/germany_college.Rda")
germany_college <- st_as_sf(do.call(rbind, germany_college))
germany_college <- germany_college[
  !duplicated(germany_college$osm_id, incomparables = NA),
]
germany_college <- germany_college[
  !rownames(germany_college) %in% rownames(germany_college[
    is.na(germany_college$osm_id),
  ][
    duplicated(st_coordinates(germany_college[
      is.na(germany_college$osm_id),
    ])),
  ]),
]
germany_college$type <- "higher education"
load("osmdata/germany_kindergarten.Rda")
germany_kindergarten <- st_as_sf(dplyr::bind_rows(germany_kindergarten))
germany_kindergarten <- germany_kindergarten[
  !duplicated(germany_kindergarten$osm_id, incomparables = NA),
]
germany_kindergarten <- germany_kindergarten[
  !rownames(germany_kindergarten) %in% rownames(germany_kindergarten[
    is.na(germany_kindergarten$osm_id),
  ][
    duplicated(st_coordinates(germany_kindergarten[
      is.na(germany_kindergarten$osm_id),
    ])),
  ]),
]
germany_kindergarten$type <- "kindergarten"
load("osmdata/germany_schools.Rda")
germany_schools <- st_as_sf(dplyr::bind_rows(germany_schools))
germany_schools <- germany_schools[
  !duplicated(germany_schools$osm_id, incomparables = NA),
]
germany_schools <- germany_schools[
  !rownames(germany_schools) %in% rownames(germany_schools[
    is.na(germany_schools$osm_id),
  ][
    duplicated(st_coordinates(germany_schools[
      is.na(germany_schools$osm_id),
    ])),
  ]),
]
germany_schools$type <- "school"
load("osmdata/germany_bakery.Rda")
germany_bakeries <- st_as_sf(do.call(rbind, germany_bakeries))
germany_bakeries <- germany_bakeries[
  !duplicated(germany_bakeries$osm_id, incomparables = NA),
]
germany_bakeries <- germany_bakeries[
  !rownames(germany_bakeries) %in% rownames(germany_bakeries[
    is.na(germany_bakeries$osm_id),
  ][
    duplicated(st_coordinates(germany_bakeries[
      is.na(germany_bakeries$osm_id),
    ])),
  ]),
]
germany_bakeries$type <- "bakery"
load("osmdata/germany_hairdresser.Rda")
germany_hairdresser <- st_as_sf(dplyr::bind_rows(germany_hairdresser))
germany_hairdresser <- germany_hairdresser[
  !duplicated(germany_hairdresser$osm_id, incomparables = NA),
]
germany_hairdresser <- germany_hairdresser[
  !rownames(germany_hairdresser) %in% rownames(germany_hairdresser[
    is.na(germany_hairdresser$osm_id),
  ][
    duplicated(st_coordinates(germany_hairdresser[
      is.na(germany_hairdresser$osm_id),
    ])),
  ]),
]
germany_hairdresser$type <- "hairdresser"
load("osmdata/germany_clinic.Rda")
germany_clinic <- st_as_sf(dplyr::bind_rows(germany_clinic))
germany_clinic <- germany_clinic[
  !duplicated(germany_clinic$osm_id, incomparables = NA),
]
germany_clinic <- germany_clinic[
  !rownames(germany_clinic) %in% rownames(germany_clinic[
    is.na(germany_clinic$osm_id),
  ][
    duplicated(st_coordinates(germany_clinic[
      is.na(germany_clinic$osm_id),
    ])),
  ]),
]
germany_clinic$type <- "clinic"
load("osmdata/germany_sport.Rda")
germany_sport <- st_as_sf(dplyr::bind_rows(germany_sport))
germany_sport <- germany_sport[
  !duplicated(germany_sport$osm_id, incomparables = NA),
]
germany_sport <- germany_sport[
  !rownames(germany_sport) %in% rownames(germany_sport[
    is.na(germany_sport$osm_id),
  ][
    duplicated(st_coordinates(germany_sport[
      is.na(germany_sport$osm_id),
    ])),
  ]),
]
germany_sport$type <- "sport"
load("osmdata/germany_entertainment.Rda")
germany_entertainment <- st_as_sf(dplyr::bind_rows(germany_entertainment))
germany_entertainment <- germany_entertainment[
  !duplicated(germany_entertainment$osm_id, incomparables = NA),
]
germany_entertainment <- germany_entertainment[
  !rownames(germany_entertainment) %in% rownames(germany_entertainment[
    is.na(germany_entertainment$osm_id),
  ][
    duplicated(st_coordinates(germany_entertainment[
      is.na(germany_entertainment$osm_id),
    ])),
  ]),
]
germany_entertainment$type <- "entertainment"
load("osmdata/germany_marketplace.Rda")
germany_marketplace <- st_as_sf(do.call(rbind, germany_marketplace))
germany_marketplace <- germany_marketplace[
  !duplicated(germany_marketplace$osm_id, incomparables = NA),
]
germany_marketplace <- germany_marketplace[
  !rownames(germany_marketplace) %in% rownames(germany_marketplace[
    is.na(germany_marketplace$osm_id),
  ][
    duplicated(st_coordinates(germany_marketplace[
      is.na(germany_marketplace$osm_id),
    ])),
  ]),
]
germany_marketplace$type <- "marketplace"
load("osmdata/germany_residential1.Rda")
load("osmdata/germany_residential2.Rda")
# create the residential list
germany_residential <- c(germany_residential1, germany_residential2)
rm(germany_residential1)
rm(germany_residential2)
germany_residential <- st_as_sf(dplyr::bind_rows(germany_residential))
germany_residential <- germany_residential[
  !duplicated(germany_residential$osm_id, incomparables = NA),
]
germany_residential <- germany_residential[
  !rownames(germany_residential) %in% rownames(germany_residential[
    is.na(germany_residential$osm_id),
  ][
    duplicated(st_coordinates(germany_residential[
      is.na(germany_residential$osm_id),
    ])),
  ]),
]
germany_residential$type <- "residential"
# perform all spatial matching
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
            germany_marketplace
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
            germany_entertainment
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
            germany_sport
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
            germany_clinic
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
            germany_hairdresser
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
            germany_shops
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
            germany_place_of_worship
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
            germany_retail
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
            germany_nursing_home
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
            germany_restaurant
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
            germany_aerodrome
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
            germany_office
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
            germany_platform
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
            germany_university
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
            germany_college
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
            germany_kindergarten
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
            germany_schools
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
            germany_bakeries
          )
        )
      )
    }
  )
)
coordinates <- st_coordinates(germany_residential)
germany_shape$residential <- unlist(
  lapply(
    seq_len(
      nrow(germany_shape)
    ),
    function(x, ...) {
      bbox <- st_bbox(germany_shape[x, ])
      in_box <- germany_residential[
        (coordinates[, 1] > bbox[1] & coordinates[, 1] < bbox[3]) &
          (coordinates[, 2] > bbox[2] & coordinates[, 2] < bbox[4]),
      ]
      length(
        unlist(
          st_intersects(
            germany_shape[x, ],
            in_box
          )
        )
      )
    }
  )
)
germany_aerodrome <- as_tibble(st_coordinates(germany_aerodrome))
germany_aerodrome$type <- "aerodrome"
germany_bakeries <- as_tibble(st_coordinates(germany_bakeries))
germany_bakeries$type <- "bakery"
germany_clinic <- as_tibble(st_coordinates(germany_clinic))
germany_clinic$type <- "clinic"
germany_college <- as_tibble(st_coordinates(germany_college))
germany_college$type <- "higher education"
germany_entertainment <- as_tibble(st_coordinates(germany_entertainment))
germany_entertainment$type <- "entertainment"
germany_hairdresser <- as_tibble(st_coordinates(germany_hairdresser))
germany_hairdresser$type <- "hairdresser"
germany_hospital <- as_tibble(st_coordinates(germany_hospital))
germany_hospital$type <- "hospital"
germany_kindergarten <- as_tibble(st_coordinates(germany_kindergarten))
germany_kindergarten$type <- "kindergarten"
germany_marketplace <- as_tibble(st_coordinates(germany_marketplace))
germany_marketplace$type <- "marketplace"
germany_nursing_home <- as_tibble(st_coordinates(germany_nursing_home))
germany_nursing_home$type <- "nursing home"
germany_office <- as_tibble(st_coordinates(germany_office))
germany_office$type <- "office"
germany_place_of_worship <- as_tibble(st_coordinates(germany_place_of_worship))
germany_place_of_worship$type <- "place of worship"
germany_platform <- as_tibble(st_coordinates(germany_platform))
germany_platform$type <- "platform"
germany_residential <- as_tibble(st_coordinates(germany_residential))
germany_residential$type <- "residential"
germany_restaurant <- as_tibble(st_coordinates(germany_restaurant))
germany_restaurant$type <- "restaurant"
germany_retail <- as_tibble(st_coordinates(germany_retail))
germany_retail$type <- "retail"
germany_schools <- as_tibble(st_coordinates(germany_schools))
germany_schools$type <- "school"
germany_shops <- as_tibble(st_coordinates(germany_shops))
germany_shops$type <- "shop"
germany_sport <- as_tibble(st_coordinates(germany_sport))
germany_sport$type <- "sport"
germany_university <- as_tibble(st_coordinates(germany_university))
germany_university$type <- "higher education"
germany_all <- dplyr::bind_rows(
  germany_aerodrome,
  germany_bakeries,
  germany_clinic,
  germany_college,
  germany_entertainment,
  germany_hairdresser,
  germany_hospital,
  germany_kindergarten,
  germany_marketplace,
  germany_nursing_home,
  germany_office,
  germany_place_of_worship,
  germany_platform,
  germany_residential,
  germany_restaurant,
  germany_retail,
  germany_schools,
  germany_shops,
  germany_sport,
  germany_university
)
colnames(germany_all) <- c("longitude", "latitude", "type")
readr::write_csv(germany_all, "germany_data/germany_all.csv")
cols <- tibble(
  colnames = colnames(germany_shape)
)
write_csv(cols, "germany_data/colnames_shapefile.csv")
write_sf(germany_shape, "germany_data/shapefile_germany.shp")
