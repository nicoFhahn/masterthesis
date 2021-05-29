# this is the script showing which data was download from OpenStreetMap
# pretty selfexplanatory
library(pbapply)
source("R/functions.R")
# create a dummy frame
df <- st_sf(
  osm_id = 0,
  geometry = st_sfc(lapply(
    0, function(x) st_geometrycollection()
  )),
  crs = 4326
)[0, ]
norge <- read_sf("shapefiles/kommuner_komprimert-polygon.shp")
norge_bbox <- lapply(norge$geometry, st_bbox)
norge_schools <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "school"
    )
  }
)
norge_schools <- lapply(
  norge_schools,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_schools) <- norge$kommunenum
save(norge_schools, file = "osmdata/norge_schools.Rda")
norge_kindergarten <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "kindergarten"
    )
  }
)
norge_kindergarten <- lapply(
  norge_kindergarten,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_kindergarten) <- norge$kommunenum
save(norge_kindergarten, file = "osmdata/norge_kindergarten.Rda")
norge_college <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "college"
    )
  }
)
norge_college <- lapply(
  norge_college,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_college) <- norge$kommunenum
save(norge_college, file = "osmdata/norge_college.Rda")
norge_university <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "university"
    )
  }
)
norge_university <- lapply(
  norge_university,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_university) <- norge$kommunenum
save(norge_university, file = "osmdata/norge_university.Rda")
norge_platform <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "public_transport",
      value = "platform"
    )
  }
)
norge_platform <- lapply(
  norge_platform,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_platform) <- norge$kommunenum
save(norge_platform, file = "osmdata/norge_platform.Rda")
norge_shops <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "shop",
      value = c("supermarket", "convenience", "chemist")
    )
  }
)
norge_shops <- lapply(
  norge_shops,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_shops) <- norge$kommunenum
save(norge_shops, file = "osmdata/norge_shops.Rda")
norge_office <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "building",
      value = "office"
    )
  }
)
norge_office <- lapply(
  norge_office,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_office) <- norge$kommunenum
save(norge_office, file = "osmdata/norge_office.Rda")
norge_aerodrome <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "aeroway",
      value = "aerodrome"
    )
  }
)
norge_aerodrome <- lapply(
  norge_aerodrome,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_aerodrome) <- norge$kommunenum
save(norge_aerodrome, file = "osmdata/norge_aerodrome.Rda")
norge_restaurant <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "restaurant"
    )
  }
)
norge_restaurant <- lapply(
  norge_restaurant,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_restaurant) <- norge$kommunenum
save(norge_restaurant, file = "osmdata/norge_restaurant.Rda")
norge_hospital <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "hospital"
    )
  }
)
norge_hospital <- lapply(
  norge_hospital,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_hospital) <- norge$kommunenum
save(norge_hospital, file = "osmdata/norge_hospital.Rda")
norge_nursing_home <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "nursing_home"
    )
  }
)
norge_nursing_home <- lapply(
  norge_nursing_home,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_nursing_home) <- norge$kommunenum
save(norge_nursing_home, file = "osmdata/norge_nursing_home.Rda")
norge_retail <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "building",
      value = "retail"
    )
  }
)
norge_retail <- lapply(
  norge_retail,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_retail) <- norge$kommunenum
save(norge_retail, file = "osmdata/norge_retail.Rda")
norge_place_of_worship <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "place_of_worship"
    )
  }
)
norge_place_of_worship <- lapply(
  norge_place_of_worship,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_place_of_worship) <- norge$kommunenum
save(norge_place_of_worship, file = "osmdata/norge_place_of_worship.Rda")
norge_bakeries <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "shop",
      value = "bakery"
    )
  }
)
norge_bakeries <- lapply(
  norge_bakeries,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_bakeries) <- norge$kommunenum
save(norge_bakeries, file = "osmdata/norge_bakery.Rda")
norge_hairdresser <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "shop",
      value = "hairdresser"
    )
  }
)
norge_hairdresser <- lapply(
  norge_hairdresser,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_hairdresser) <- norge$kommunenum
save(norge_hairdresser, file = "osmdata/norge_hairdresser.Rda")
norge_clinic <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = c("clinic", "dentist", "doctors")
    )
  }
)
norge_clinic <- lapply(
  norge_clinic,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_clinic) <- norge$kommunenum
save(norge_clinic, file = "osmdata/norge_clinic.Rda")
norge_sport <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "leisure",
      value = c("fitness_centre", "sports_centre")
    )
  }
)
norge_sport <- lapply(
  norge_sport,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_sport) <- norge$kommunenum
save(norge_sport, file = "osmdata/norge_sport.Rda")
norge_entertainment <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = c("cinema", "theatre", "nightclub")
    )
  }
)
norge_entertainment <- lapply(
  norge_entertainment,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_entertainment) <- norge$kommunenum
save(norge_entertainment, file = "osmdata/norge_entertainment.Rda")
norge_marketplace <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "amenity",
      value = "marketplace"
    )
  }
)
norge_marketplace <- lapply(
  norge_marketplace,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_marketplace) <- norge$kommunenum
save(norge_marketplace, file = "osmdata/norge_marketplace.Rda")
norge_residential <- pblapply(
  norge_bbox,
  function(x) {
    Sys.sleep(5)
    download_key_data(
      x,
      key = "building",
      value = c(
        "apartments", "bungalow", "cabin", "detached", "dormitory", "farm",
        "ger", "hotel", "house", "houseboat", "residential",
        "semidetached_house", "static caravan", "terrace"
      )
    )
  }
)
norge_residential <- lapply(
  norge_residential,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(norge_residential) <- norge$kommunenum
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
germany_schools <- lapply(
  germany_schools,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_schools) <- germany$Kennziffer
save(germany_schools, file = "osmdata/germany_schools.Rda")
germany_kindergarten <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "kindergarten"
)
germany_kindergarten <- lapply(
  germany_kindergarten,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_kindergarten) <- germany$Kennziffer
save(germany_kindergarten, file = "osmdata/germany_kindergarten.Rda")
germany_college <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "college"
)
germany_college <- lapply(
  germany_college,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_college) <- germany$Kennziffer
save(germany_college, file = "osmdata/germany_college.Rda")
germany_university <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "university"
)
germany_university <- lapply(
  germany_university,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_university) <- germany$Kennziffer
save(germany_university, file = "osmdata/germany_university.Rda")
germany_platform <- pblapply(
  germany_bbox,
  download_key_data,
  key = "public_transport",
  value = "platform"
)
germany_platform <- lapply(
  germany_platform,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_platform) <- germany$Kennziffer
save(germany_platform, file = "osmdata/germany_platform.Rda")
germany_shops <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = c("supermarket", "convenience", "chemist")
)
germany_shops <- lapply(
  germany_shops,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_shops) <- germany$Kennziffer
save(germany_shops, file = "osmdata/germany_shops.Rda")
germany_office <- pblapply(
  germany_bbox,
  download_key_data,
  key = "building",
  value = "office"
)
germany_office <- lapply(
  germany_office,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_office) <- germany$Kennziffer
save(germany_office, file = "osmdata/germany_office.Rda")
germany_aerodrome <- pblapply(
  germany_bbox,
  download_key_data,
  key = "aeroway",
  value = "aerodrome"
)
germany_aerodrome <- lapply(
  germany_aerodrome,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_aerodrome) <- germany$Kennziffer
save(germany_aerodrome, file = "osmdata/germany_aerodrome.Rda")
germany_restaurant <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "restaurant"
)
germany_restaurant <- lapply(
  germany_restaurant,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_restaurant) <- germany$Kennziffer
save(germany_restaurant, file = "osmdata/germany_restaurant.Rda")
germany_hospital <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "hospital"
)
germany_hospital <- lapply(
  germany_hospital,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_hospital) <- germany$Kennziffer
save(germany_hospital, file = "osmdata/germany_hospital.Rda")
germany_nursing_home <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "nursing_home"
)
germany_nursing_home <- lapply(
  germany_nursing_home,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_nursing_home) <- germany$Kennziffer
save(germany_nursing_home, file = "osmdata/germany_nursing_home.Rda")
germany_retail <- pblapply(
  germany_bbox,
  download_key_data,
  key = "building",
  value = "retail"
)
germany_retail <- lapply(
  germany_retail,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_retail) <- germany$Kennziffer
save(germany_retail, file = "osmdata/germany_retail.Rda")
germany_place_of_worship <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "place_of_worship"
)
germany_place_of_worship <- lapply(
  germany_place_of_worship,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_place_of_worship) <- germany$Kennziffer
save(germany_place_of_worship, file = "osmdata/germany_place_of_worship.Rda")
germany_bakeries <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = "bakery"
)
germany_bakeries <- lapply(
  germany_bakeries,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_bakeries) <- germany$Kennziffer
save(germany_bakeries, file = "osmdata/germany_bakery.Rda")
germany_hairdresser <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = "hairdresser"
)
germany_hairdresser <- lapply(
  germany_hairdresser,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_hairdresser) <- germany$Kennziffer
save(germany_hairdresser, file = "osmdata/germany_hairdresser.Rda")
germany_clinic <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = c("clinic", "dentist", "doctors")
)
germany_clinic <- lapply(
  germany_clinic,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_clinic) <- germany$Kennziffer
save(germany_clinic, file = "osmdata/germany_clinic.Rda")
germany_sport <- pblapply(
  germany_bbox,
  download_key_data,
  key = "leisure",
  value = c("fitness_centre", "sports_centre")
)
germany_sport <- lapply(
  germany_sport,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_sport) <- germany$Kennziffer
save(germany_sport, file = "osmdata/germany_sport.Rda")
germany_entertainment <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = c("cinema", "theatre", "nightclub")
)
germany_entertainment <- lapply(
  germany_entertainment,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_entertainment) <- germany$Kennziffer
save(germany_entertainment, file = "osmdata/germany_entertainment.Rda")
germany_marketplace <- pblapply(
  germany_bbox,
  download_key_data,
  key = "amenity",
  value = "marketplace"
)
germany_marketplace <- lapply(
  germany_marketplace,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_marketplace) <- germany$Kennziffer
save(germany_marketplace, file = "osmdata/germany_marketplace.Rda")
germany_residential <- pblapply(
  germany_bbox,
  function(x) {
    download_key_data(
      x,
      key = "building",
      value = c(
        "apartments", "bungalow", "cabin", "detached", "dormitory", "farm",
        "ger", "hotel", "house", "houseboat", "residential",
        "semidetached_house", "static caravan", "terrace"
      )
    )
  }
)
germany_residential <- lapply(
  germany_residential,
  function(x, ...) {
    if ("osm_id" %in% colnames(x)) {
      x[, "osm_id"]
    } else {
      df
    }
  }
)
names(germany_residential) <- germany$Kennziffer
germany_residential1 <- germany_residential[1:120]
germany_residential2 <- germany_residential[121:401]
save(germany_residential1, file = "osmdata/germany_residential1.Rda")
save(germany_residential2, file = "osmdata/germany_residential2.Rda")
