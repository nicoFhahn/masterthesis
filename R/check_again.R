source("functions.R")
norge <- read_sf("data/kommuner_komprimert-polygon.shp")
norge_bbox <- lapply(norge$geometry, st_bbox)
load("norge_university.Rda")
norge_university_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_university, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "university"
)
table(unlist(lapply(norge_university_2, ncol)))
load("norge_schools.Rda")
norge_schools_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_schools, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "school"
)
load("norge_kindergarten.Rda")
norge_kindergarten_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_kindergarten, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "kindergarten"
)
table(unlist(lapply(norge_kindergarten_2, ncol)))
load("norge_college.Rda")
norge_college_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_college, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "college"
)
table(unlist(lapply(norge_college_2, ncol)))
load("norge_restaurant.Rda")
norge_restaurant_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_restaurant, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "restaurant"
)
table(unlist(lapply(norge_restaurant_2, ncol)))
load("norge_hospital.Rda")
norge_hospital_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_hospital, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "hospital"
)
table(unlist(lapply(norge_hospital_2, ncol)))
load("norge_nursing_home.Rda")
norge_nursing_home_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_nursing_home, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "nursing_home"
)
table(unlist(lapply(norge_nursing_home_2, ncol)))
load("norge_place_of_worship.Rda")
norge_place_of_worship_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_place_of_worship, ncol)) == 3],
  download_key_data,
  key = "amenity",
  value = "place_of_worship"
)
table(unlist(lapply(norge_place_of_worship_2, ncol)))
load("norge_platform.Rda")
norge_platform_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_platform, ncol)) == 3],
  download_key_data,
  key = "public_transport",
  value = "platform"
)
load("norge_supermarket.Rda")
norge_supermarket_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_supermarket, ncol)) == 3],
  download_key_data,
  key = "shop",
  value = "supermarket"
)
table(unlist(lapply(norge_supermarket_2, ncol)))
load("norge_office.Rda")
norge_office_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_office, ncol)) == 3],
  download_key_data,
  key = "building",
  value = "office"
)
load("norge_aerodrome.Rda")
norge_aerodrome_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_aerodrome, ncol)) == 3],
  download_key_data,
  key = "aeroway",
  value = "aerodrome"
)
table(unlist(lapply(norge_aerodrome_2, ncol)))
load("norge_terminal.Rda")
norge_terminal_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_terminal, ncol)) == 3],
  download_key_data,
  key = "aeroway",
  value = "terminal"
)
table(unlist(lapply(norge_terminal_2, ncol)))
load("norge_retail.Rda")
norge_retail_2 <- pblapply(
  norge_bbox[unlist(lapply(norge_retail, ncol)) == 3],
  download_key_data,
  key = "building",
  value = "retail"
)
table(unlist(lapply(norge_retail_2, ncol)))
