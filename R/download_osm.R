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
norge_supermarket <- pblapply(
  norge_bbox,
  download_key_data,
  key = "shop",
  value = "supermarket"
)
save(norge_supermarket, file = "osmdata/norge_supermarket.Rda")
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
save(germany_platform, file = "osmdata/germany_platform.Rda")
germany_supermarket <- pblapply(
  germany_bbox,
  download_key_data,
  key = "shop",
  value = "supermarket"
)
save(germany_supermarket, file = "osmdata/germany_supermarket.Rda")
germany_office <- pblapply(
  germany_bbox,
  download_key_data,
  key = "building",
  value = "office"
)
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