# this is the script for the moran test
library(spdep)
# load the data
source("R/preprocess_germany.R")
# create the neighbourhood
nb <- poly2nb(newest_numbers)
# create a weighted list
lw <- nb2listw(
  nb,
  style = "W",
  zero.policy = TRUE
)
# conduct the test
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
