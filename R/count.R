library(sf)
norge <- read_sf("data/kommuner_komprimert-polygon.shp")
load("norge_university.Rda")
no_universities <- lapply(seq_len(nrow(norge)), function(x, ...) {
  if (nrow(norge_university[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_university[[x]])))
  } else {
    0
  }
})
load("norge_college.Rda")
no_college <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_college[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_college[[x]])))
  } else {
    0
  }
})
load("norge_schools.Rda")
no_schools <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_schools[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_schools[[x]])))
  } else {
    0
  }
})
load("norge_kindergarten.Rda")
no_kindergarten <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_kindergarten[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_kindergarten[[x]])))
  } else {
    0
  }
})
load("norge_restaurant.Rda")
no_restaurant <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_restaurant[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_restaurant[[x]])))
  } else {
    0
  }
})
load("norge_hospital.Rda")
no_hospital <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_hospital[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_hospital[[x]])))
  } else {
    0
  }
})
load("norge_nursing_home.Rda")
no_nursing_home <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_nursing_home[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_nursing_home[[x]])))
  } else {
    0
  }
})
load("norge_place_of_worship.Rda")
no_place_of_worship <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_place_of_worship[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_place_of_worship[[x]])))
  } else {
    0
  }
})
load("norge_platform.Rda")
no_platform <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_platform[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_platform[[x]])))
  } else {
    0
  }
})
load("norge_supermarket.Rda")
no_supermarket <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_supermarket[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_supermarket[[x]])))
  } else {
    0
  }
})
load("norge_office.Rda")
no_office <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_office[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_office[[x]])))
  } else {
    0
  }
})
load("norge_aerodrome.Rda")
no_aerodrome <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_aerodrome[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_aerodrome[[x]])))
  } else {
    0
  }
})
load("norge_terminal.Rda")
no_terminal <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_terminal[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_terminal[[x]])))
  } else {
    0
  }
})
load("norge_retail.Rda")
no_retail <- lapply(seq_len(nrow(norge)), function(x, ...) {
  kommune <- norge[x, ]
  if (nrow(norge_retail[[x]]) > 0) {
    length(unlist(st_intersects(norge[x, ], norge_retail[[x]])))
  } else {
    0
  }
})









