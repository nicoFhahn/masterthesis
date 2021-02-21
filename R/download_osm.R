library(pbapply)
source("R/functions.R")
norge <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
norge_bbox <- lapply(norge$geometry, st_bbox)
norge_schools <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "school"
)
save(norge_schools, file = "osmdata/norge_schools.Rda")
norge_kindergarten <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "kindergarten"
)
save(norge_kindergarten, file = "osmdata/norge_kindergarten.Rda")
norge_college <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "college"
)
save(norge_college, file = "osmdata/norge_college.Rda")
norge_university <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "university"
)
save(norge_university, file = "osmdata/norge_university.Rda")
norge_platform <- pblapply(
  norge_bbox,
  download_key_data,
  key = "public_transport",
  value = "platform"
)
save(norge_platform, file = "osmdata/norge_platform.Rda")
norge_shops <- pblapply(
  norge_bbox,
  download_key_data,
  key = "shop",
  value = c("supermarket", "convenience", "chemist")
)
save(norge_shops, file = "osmdata/norge_shops.Rda")
norge_office <- pblapply(
  norge_bbox,
  download_key_data,
  key = "building",
  value = "office"
)
save(norge_office, file = "osmdata/norge_office.Rda")
norge_aerodrome <- pblapply(
  norge_bbox,
  download_key_data,
  key = "aeroway",
  value = "aerodrome"
)
save(norge_aerodrome, file = "osmdata/norge_aerodrome.Rda")
norge_terminal <- pblapply(
  norge_bbox,
  download_key_data,
  key = "aeroway",
  value = "terminal"
)
save(norge_terminal, file = "osmdata/norge_terminal.Rda")
norge_restaurant <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "restaurant"
)
save(norge_restaurant, file = "osmdata/norge_restaurant.Rda")
norge_hospital <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "hospital"
)
save(norge_hospital, file = "osmdata/norge_hospital.Rda")
norge_nursing_home <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "nursing_home"
)
save(norge_nursing_home, file = "osmdata/norge_nursing_home.Rda")
norge_retail <- pblapply(
  norge_bbox,
  download_key_data,
  key = "building",
  value = "retail"
)
save(norge_retail, file = "osmdata/norge_retail.Rda")
norge_place_of_worship <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "place_of_worship"
)
save(norge_place_of_worship, file = "osmdata/norge_place_of_worship.Rda")
norge_bakeries <- pblapply(
  norge_bbox,
  download_key_data,
  key = "shop",
  value = "bakery"
)
save(norge_bakeries, file = "osmdata/norge_bakery.Rda")
norge_gas <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "fuel"
)
save(norge_gas, file = "osmdata/norge_gas.Rda")
norge_banks <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "bank"
)
save(norge_banks, file = "osmdata/norge_banks.Rda")
norge_atms <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "atm"
)
save(norge_atms, file = "osmdata/norge_atms.Rda")
norge_hairdresser <- pblapply(
  norge_bbox,
  download_key_data,
  key = "shop",
  value = "hairdresser"
)
save(norge_hairdresser, file = "osmdata/norge_hairdresser.Rda")
norge_toilets <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "toilets"
)
save(norge_toilets, file = "osmdata/norge_toilets.Rda")
norge_clinic <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = c("clinic", "dentist", "doctors")
)
save(norge_clinic, file = "osmdata/norge_clinic.Rda")
norge_sport <- pblapply(
  norge_bbox,
  download_key_data,
  key = "leisure",
  value = c("fitness_centre", "sports_centre")
)
save(norge_sport, file = "osmdata/norge_sport.Rda")
norge_entertainment <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = c("cinema", "theatre", "nightclub")
)
save(norge_entertainment, file = "osmdata/norge_entertainment.Rda")
norge_marketplace <- pblapply(
  norge_bbox,
  download_key_data,
  key = "amenity",
  value = "marketplace"
)
save(norge_marketplace, file = "osmdata/norge_marketplace.Rda")
norge_residential <- pblapply(
  norge_bbox,
  download_key_data,
  key = "building",
  value = c(
    "apartments", "bungalow", "cabin", "detached", "dormitory", "farm", "ger",
    "hotel", "house", "houseboat", "residential", "semidetached_house",
    "static caravan", "terrace"
  )
)
norge_residential <- lapply(
  norge_residential,
  function(x) {
    if (nrow(x) > 0) {
      x[, "osm_id"]
    } else {
      x
    }
  }
)
save(norge_residential, file = "osmdata/norge_residential.Rda")

germany <- read_sf("shapefiles/Kreisgrenzen_2017_mit_Einwohnerzahl.shp")
germany <- st_transform(germany, 4326)

germany_bbox <- lapply(germany$geometry, st_bbox)


germany_schools <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "school"
)
save(germany_schools, file = "osmdata/germany_schools.Rda")
germany_kindergarten <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "kindergarten"
)
save(germany_kindergarten, file = "osmdata/germany_kindergarten.Rda")
germany_college <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "college"
)
save(germany_college, file = "osmdata/germany_college.Rda")
germany_university <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "university"
)
save(germany_university, file = "osmdata/germany_university.Rda")
germany_platform <- pblapply(
  germany_bbox,
  download_key_data,
  key = "public_transport",
  value = "platform"
)
germany_platform <- lapply(germany_platform, function(x) x[, "osm_id"])
save(germany_platform, file = "osmdata/germany_platform.Rda")
germany_shops <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = c("supermarket", "convenience", "chemist")
)
save(germany_shops, file = "osmdata/germany_shops.Rda")
germany_office <- pblapply(
  germany_bbox,
  download_key_data,
  key = "building",
  value = "office"
)
load("osmdata/germany_office.Rda")
save(germany_office, file = "osmdata/germany_office.Rda")
germany_aerodrome <- pblapply(
  germany_bbox,
  download_key_data,
  key = "aeroway",
  value = "aerodrome"
)
save(germany_aerodrome, file = "osmdata/germany_aerodrome.Rda")
germany_terminal <- pblapply(
  germany_bbox,
  download_key_data,
  key = "aeroway",
  value = "terminal"
)
save(germany_terminal, file = "osmdata/germany_terminal.Rda")
germany_restaurant <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "restaurant"
)
save(germany_restaurant, file = "osmdata/germany_restaurant.Rda")
germany_hospital <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "hospital"
)
save(germany_hospital, file = "osmdata/germany_hospital.Rda")
germany_nursing_home <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "nursing_home"
)
save(germany_nursing_home, file = "osmdata/germany_nursing_home.Rda")
germany_retail <- pblapply(
  germany_bbox,
  download_key_data,
  key = "building",
  value = "retail"
)
save(germany_retail, file = "osmdata/germany_retail.Rda")
germany_place_of_worship <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "place_of_worship"
)
save(germany_place_of_worship, file = "osmdata/germany_place_of_worship.Rda")
germany_bakeries <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = "bakery"
)

save(germany_bakeries, file = "osmdata/germany_bakery.Rda")
germany_gas <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "fuel"
)

save(germany_gas, file = "osmdata/germany_gas.Rda")
germany_banks <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "bank"
)
save(germany_banks, file = "osmdata/germany_banks.Rda")
germany_atms <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "atm"
)
save(germany_atms, file = "osmdata/germany_atms.Rda")
germany_hairdresser <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = "hairdresser"
)
save(germany_hairdresser, file = "osmdata/germany_hairdresser.Rda")
germany_toilets <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "toilet"
)
load("osmdata/germany_toilets.Rda")
table(
  unlist(
    lapply(
      germany_toilets2,
      nrow
    )
  )
)
germany_toilets2 <- pblapply(
  germany_bbox[unlist(lapply(germany_toilets, nrow)) == 0],
  download_key_data,
  key = "amenity",
  value = "toilets"
)
germany_toilets[unlist(lapply(germany_toilets, nrow)) == 0] <- germany_toilets2
save(germany_toilets, file = "osmdata/germany_toilets.Rda")
germany_clinic <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = c("clinic", "dentist", "doctors")
)
save(germany_clinic, file = "osmdata/germany_clinic.Rda")
germany_sport <- pblapply(
  germany_bbox,
  download_key_data,
  key = "leisure",
  value = c("fitness_centre", "sports_centre")
)
save(germany_sport, file = "osmdata/germany_sport.Rda")
germany_entertainment <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = c("cinema", "theatre", "nightclub")
)
save(germany_entertainment, file = "osmdata/germany_entertainment.Rda")
germany_marketplace <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "marketplace"
)
save(germany_marketplace, file = "osmdata/germany_marketplace.Rda")
germany_residential <- pblapply(
  germany_bbox,
  function(x) {
    download_key_data(
      x,
      key = "building",
      value = c(
        "apartments", "bungalow", "cabin", "detached", "dormitory", "farm", "ger",
        "hotel", "house", "houseboat", "residential", "semidetached_house",
        "static caravan", "terrace"
      )
    )
  }
)
germany_residential <- lapply(
  germany_residential,
  function(x) {
    if (nrow(x) > 0) {
      x[, "osm_id"]
    } else {
      x
    }
  }
)
a <- germany_residential
germany_residential <- lapply(
  germany_residential,
  function(x) x[!duplicated(x$osm_id), ]
)

save(germany_residential, file = "osmdata/germany_residential.Rda")
