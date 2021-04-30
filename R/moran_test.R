library(spdep)
source("R/preprocess_germany.R")
nb <- poly2nb(newest_numbers)
lw <- nb2listw(
  nb,
  style = "W",
  zero.policy = TRUE
)
moran.test(
  newest_numbers$value,
  lw,
  alternative = "two.sided"
)
source("R/preprocess_norge.R")
nb <- poly2nb(newest_numbers)
lw <- nb2listw(
  nb,
  style = "W",
  zero.policy = TRUE
)
moran.test(
  newest_numbers$value,
  lw,
  alternative = "two.sided"
)
