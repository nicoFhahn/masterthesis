library(ggplot2)
library(patchwork)
library(readr)
library(sf)
library(tibble)
library(INLA)
library(latex2exp)
# source("R/nontemporal_models_norway.R")
norge <- read_csv("eval_data/norge_may2.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
norge <- merge(
  norge,
  norge_sf,
  by = "kommune_no"
)
norge <- st_as_sf(norge)
norge <- norge[
  norge$date %in% seq(from = min(norge$date), to = max(norge$date), by = 5),
]
load("models/temporal_norway.Rda")
# show the 5 municipalities with the most infections
newest <- norge[norge$date == max(norge$date), ]
newest[
  order(newest$value, decreasing = TRUE),
][1:5, c("kommune_name", "population", "value", "vaccine_shots")]
# get the dic values
# first besag, then bym2, then leroux
models_final[[2]][[1]]$dic
models_final[[2]][[2]]$dic
models_final[[2]][[3]]$dic
models_final[[2]][[4]]$dic
models_final[[2]][[5]]$dic
# now the waic
models_final[[2]][[1]]$waic
models_final[[2]][[2]]$waic
models_final[[2]][[3]]$waic
models_final[[2]][[4]]$waic
models_final[[2]][[5]]$waic
# now the cpo
models_final[[2]][[1]]$cpo
models_final[[2]][[2]]$cpo
models_final[[2]][[3]]$cpo
models_final[[2]][[4]]$cpo
models_final[[2]][[5]]$cpo
# now the mae
models_final[[3]][[1]]
models_final[[3]][[2]]
models_final[[3]][[3]]
models_final[[3]][[4]]
models_final[[3]][[5]]
options(scipen = 10)
# create a tibble with the credibility intervals and posterior coefficients
# for the bym2 model and the model without the spatial component
marginal_frame <- tibble(
  lower = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$office
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$office
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$office
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$median_age
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$median_age
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$median_age
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$vaccine_shots
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$vaccine_shots
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$vaccine_shots
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$marketplace
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$marketplace
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$marketplace
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$unemp_tot
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$unemp_tot
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$unemp_tot
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$unemp_immg
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$unemp_immg
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$unemp_immg
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$immigrants_total
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$immigrants_total
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$immigrants_total
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$urb_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$urb_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$urb_dens
      )
    )[1]
  ),
  mean = c(
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$office
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$office
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$office
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$median_age
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$median_age
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$median_age
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$vaccine_shots
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$vaccine_shots
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$vaccine_shots
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$marketplace
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$marketplace
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$marketplace
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$unemp_tot
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$unemp_tot
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$unemp_tot
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$unemp_immg
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$unemp_immg
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$unemp_immg
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$immigrants_total
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$immigrants_total
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$immigrants_total
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$urb_dens
    )
  ),
  upper = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$office
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$office
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$office
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$median_age
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$median_age
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$median_age
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$vaccine_shots
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$vaccine_shots
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$vaccine_shots
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$marketplace
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$marketplace
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$marketplace
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$unemp_tot
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$unemp_tot
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$unemp_tot
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$unemp_immg
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$unemp_immg
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$unemp_immg
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$immigrants_total
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$immigrants_total
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$immigrants_total
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$urb_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$urb_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$urb_dens
      )
    )[2]
  ),
  variable = c(
    rep("Intercept", 3),
    rep("sex", 3),
    rep("aerodrome", 3),
    rep("office", 3),
    rep("median_age", 3),
    rep("place_of_worship", 3),
    rep("vaccine_shots", 3),
    rep("higher_education", 3),
    rep("nursing_home", 3),
    rep("marketplace", 3),
    rep("platform", 3),
    rep("unemp_total", 3),
    rep("unemp_immg", 3),
    rep("immigrants_total", 3),
    rep("urb_dens", 3)
  ),
  model = rep(
    c("No spatial", "BYM2 spatial", "BYM2 spatio-temporal"), 15
  )
)
# order the variable
marginal_frame$variable <- ordered(
  marginal_frame$variable,
  levels = unique(marginal_frame$variable)
)
# plot the intervals and mean
ggplot(
  data = marginal_frame
) +
  geom_errorbar(
    aes(
      y = variable,
      xmin = lower,
      xmax = upper,
      colour = model
    ),
    position = "dodge",
    size = 0.5
  ) +
  geom_pointrange(
    aes(
      y = variable,
      xmin = lower,
      xmax = upper,
      x = mean,
      colour = model
    ),
    position = position_dodge2(width = 0.9, padding = 0),
    size = 0.5
  ) +
  theme_minimal() +
  xlab(
    "Marginals"
  ) +
  ylab(
    "Variable"
  ) +
  ggtitle(
    "Credibility intervals of the coefficients"
  ) +
  scale_colour_manual(
    values = c("#1B998B", "#ED217C", "#2D3047")
  ) +
  geom_vline(xintercept = 1) +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  )
# get the summary of the bym2 model
models_final[[1]][[2]]$summary.fixed[
  order(models_final[[1]][[2]]$summary.fixed$mean),
]
# get the exponentiated coefficients
sapply(
  models_final[[1]][[2]]$marginals.fixed[
    rownames(models_final[[1]][[2]]$summary.fixed[
      order(models_final[[1]][[2]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
# and the credibility intervals
sapply(
  models_final[[1]][[2]]$marginals.fixed[
    rownames(models_final[[1]][[2]]$summary.fixed[
      order(models_final[[1]][[2]]$summary.fixed$mean),
    ])
  ],
  function(x) {
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, x
      )
    )
  }
)
temporal_car <- lapply(
  models_final[[1]][[2]]$marginals.random$id_date_1,
  function(x) {
    marg <- inla.tmarginal(
      function(y) exp(y),
      x
    )
    inla.emarginal(mean, marg)
  }
)
# temporal_iid <- lapply(
#   models_final[[1]][[2]]$marginals.random$id_date_2,
#   function(x) {
#     marg <- inla.tmarginal(
#       function(y) exp(y),
#       x
#     )
#     inla.emarginal(mean, marg)
#   }
# )
time_tibble <- tibble(
  date = rep(sort(unique(norge$date)), 1),#2),
  trend = c(
    unlist(temporal_car)#,
    # unlist(temporal_iid)
  ),
  type = c(
    rep("CAR", length(temporal_car))#,
    # rep("iid", length(temporal_car))
  )
)
ggplot(
  data = time_tibble
) +
  geom_line(
    aes(
      x = date,
      y = trend,
      colour = type
    )
  ) +
  guides(
    colour = guide_legend(
      title = "Trend type"
    )
  ) +
  ggtitle(
    "Posterior temporal trend",
  ) +
  labs(
    x = "Date",
    y = TeX("$\\exp\\left(\\phi_t\\right)$")
  ) +
  scale_colour_manual(
    values = c("#FF715B", "#3DA35D")
  ) +
  theme_minimal()
# add new variable for the relative risk
norge$rr <- models_final[[1]][[2]]$summary.fitted.values$mean
# now calculate the posterior probability
csi <- models_final[[1]][[2]]$marginals.random$idarea_1[
  seq_len(nrow(norge_sf))
]
a <- 0
prob_csi <- lapply(csi, function(x) {
  1 - inla.pmarginal(a, x)
})
csi_cutoff <- c(0, 0.25, 0.5, 0.75, 1)
cat_csi <- cut(
  unlist(prob_csi),
  breaks = csi_cutoff,
  include.lowest = TRUE
)
# calculate the posterior mean of the relative risk (also log)
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_log <- lapply(csi, function(x) log10(inla.emarginal(exp, x)))
zeta_cutoff <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 5, 7.5, 10, 15, 25)
zeta_log_cutoff <- c(-2, -1.6, -1.2, -0.8, -0.4, 0, 0.4, 0.8, 1.2, 1.6)
# group it
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
cat_zeta_log <- cut(
  unlist(zeta_log),
  breaks = zeta_log_cutoff,
  include.lowest = TRUE
)
norge_sf$cat_zeta <- cat_zeta
norge_sf$cat_zeta_log <- cat_zeta_log
norge_sf$prob_csi <- cat_csi
mat_marg <- matrix(NA, nrow = nrow(norge_sf), ncol = 100000)
m <- models_final[[1]][[2]]$marginals.random$idarea_1
for (i in seq_len(nrow(norge_sf))) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_final[[1]][[2]]$marginals.hyperpar$`Precision for idarea_1`
  )
)
perc_var_u <- mean(var_u / (var_u + var_v))
perc_var_u

color_low <- "#20A4F3"
color_high <- "#FF206E"
norge_sf$rr_1 <- norge[norge$date == min(norge$date), ]$rr
norge_sf$rr_2 <- norge[norge$date == max(norge$date), ]$rr
# hier mit limits arbeiten
plot_1 <- ggplot(data = norge_sf) +
  geom_sf(aes(fill = rr_2)) +
  ggtitle(
    label = "Relative risk based on all variables",
    subtitle = "Norway"
  ) +
  scale_fill_gradient2(
    "RR",
    low = color_low,
    high = color_high,
    midpoint = 1
  ) +
  theme_minimal()
plot_1
plot_2 <- ggplot(data = norge_sf) +
  geom_sf(aes(fill = cat_zeta)) +
  ggtitle(
    label = "Posterior mean of the relative risk",
    subtitle = "Norway"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Relative risk"
    )
  )
plot_2
plot_3 <- ggplot(data = norge_sf) +
  geom_sf(aes(fill = prob_csi)) +
  ggtitle(
    label = "Posterior probability",
    subtitle = "Norway"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Posterior probability"
    )
  )
plot_3

plot_2 + plot_3
plot_4 <- ggplot(data = norge_sf) +
  geom_sf(aes(fill = cat_zeta_log)) +
  ggtitle(
    label = "Log10 Posterior mean of the relative risk",
    subtitle = "Norway"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Relative risk"
    )
  )
plot_4
# get the summary of the hyperparameters
models_final[[1]][[2]]$summary.hyperpar
