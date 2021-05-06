library(ggplot2)
library(patchwork)
library(readr)
library(sf)
library(tibble)
library(INLA)
library(latex2exp)
# source("R/nontemporal_models_germany.R")
germany <- read_csv("eval_data/germany_april28.csv")
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
germany <- merge(
  germany,
  germany_sf,
  by.x = "municipality_id",
  by.y = "Kennziffer"
)
germany <- st_as_sf(germany)
germany <- germany[
  germany$Date %in% seq(from = min(germany$Date), to = max(germany$Date), by = 5),
]
load("models/temporal_germany.Rda")
# show the 5 municipalities with the most infections
newest <- germany[germany$Date == max(germany$Date), ]
newest[
  order(newest$value, decreasing = TRUE),
][1:5, c("municipality", "population", "value")]
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
        exp, models_final[[1]][[4]]$marginals.fixed$Gruene
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$Gruene
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$Gruene
      )
    )[1],
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
        exp, models_final[[1]][[4]]$marginals.fixed$SPD
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$SPD
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$SPD
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
        exp, models_final[[1]][[4]]$marginals.fixed$FDP
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$FDP
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$FDP
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
        exp, models_final[[1]][[4]]$marginals.fixed$clinic
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$clinic
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$clinic
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
        exp, models_final[[1]][[4]]$marginals.fixed$die_linke
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$die_linke
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$die_linke
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
        exp, models_final[[1]][[4]]$marginals.fixed$trade_tax
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$trade_tax
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$trade_tax
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$pop_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$pop_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$pop_dens
      )
    )[1]
  ),
  mean = c(
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$Gruene
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$Gruene
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$Gruene
    ),
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
      models_final[[1]][[4]]$marginals.fixed$SPD
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$SPD
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$SPD
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
      models_final[[1]][[4]]$marginals.fixed$FDP
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$FDP
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$FDP
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
      models_final[[1]][[4]]$marginals.fixed$clinic
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$clinic
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$clinic
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
      models_final[[1]][[4]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$urb_dens
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
      models_final[[1]][[4]]$marginals.fixed$die_linke
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$die_linke
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$die_linke
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
      models_final[[1]][[4]]$marginals.fixed$trade_tax
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$trade_tax
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$trade_tax
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[4]]$marginals.fixed$pop_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[5]]$marginals.fixed$pop_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[2]]$marginals.fixed$pop_dens
    )
  ),
  upper = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$Gruene
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$Gruene
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$Gruene
      )
    )[2],
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
        exp, models_final[[1]][[4]]$marginals.fixed$SPD
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$SPD
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$SPD
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
        exp, models_final[[1]][[4]]$marginals.fixed$FDP
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$FDP
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$FDP
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
        exp, models_final[[1]][[4]]$marginals.fixed$clinic
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$clinic
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$clinic
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
        exp, models_final[[1]][[4]]$marginals.fixed$die_linke
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$die_linke
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$die_linke
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
        exp, models_final[[1]][[4]]$marginals.fixed$trade_tax
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$trade_tax
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$trade_tax
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[4]]$marginals.fixed$pop_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[5]]$marginals.fixed$pop_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[2]]$marginals.fixed$pop_dens
      )
    )[2]
  ),
  variable = c(
    rep("Greens", 3),
    rep("Intercept", 3),
    rep("higher_education", 3),
    rep("SPD", 3),
    rep("office", 3),
    rep("place_of_worship", 3),
    rep("FDP", 3),
    rep("nursing_home", 3),
    rep("clinic", 3),
    rep("sex", 3),
    rep("urb_dens", 3),
    rep("aerodrome", 3),
    rep("marketplace", 3),
    rep("die_linke", 3),
    rep("platform", 3),
    rep("log(trade_tax)", 3),
    rep("pop_dens", 3)
  ),
  model = rep(
    c("No spatial", "BYM2 spatial", "BYM2 spatio-temporal"), 17
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
#   models_final[[1]][[2]]$marginals.random$id_Date_2,
#   function(x) {
#     marg <- inla.tmarginal(
#       function(y) exp(y),
#       x
#     )
#     inla.emarginal(mean, marg)
#   }
# )
time_tibble <- tibble(
  Date = rep(sort(unique(germany$Date)), 1),#2),
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
      x = Date,
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
germany$rr <- models_final[[1]][[2]]$summary.fitted.values$mean
# now calculate the posterior probability
csi <- models_final[[1]][[2]]$marginals.random$idarea_1[
  seq_len(nrow(germany_sf))
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
zeta_cutoff <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
# group it
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
germany_sf$cat_zeta <- cat_zeta
germany_sf$prob_csi <- cat_csi
mat_marg <- matrix(NA, nrow = nrow(germany_sf), ncol = 100000)
m <- models_final[[1]][[2]]$marginals.random$idarea_1
for (i in seq_len(nrow(germany_sf))) {
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
germany_sf$rr_1 <- germany[germany$Date == min(germany$Date), ]$rr
germany_sf$rr_2 <- germany[germany$Date == max(germany$Date), ]$rr
# hier mit limits arbeiten
plot_1 <- ggplot(data = germany_sf) +
  geom_sf(aes(fill = rr_2)) +
  ggtitle(
    label = "Relative risk based on all variables",
    subtitle = "germany"
  ) +
  scale_fill_gradient2(
    "RR",
    low = color_low,
    high = color_high,
    midpoint = 1
  ) +
  theme_minimal()
plot_1
plot_2 <- ggplot(data = germany_sf) +
  geom_sf(aes(fill = cat_zeta)) +
  ggtitle(
    label = "Posterior mean of the relative risk",
    subtitle = "Germany"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Relative risk"
    )
  )
plot_2
plot_3 <- ggplot(data = germany_sf) +
  geom_sf(aes(fill = prob_csi)) +
  ggtitle(
    label = "Posterior probability",
    subtitle = "Germany"
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
# get the summary of the hyperparameters
models_final[[1]][[2]]$summary.hyperpar
