library(INLA)
library(spdep)
source("R/preprocess_germany_temporal.R")
set.seed(7918)
backup <- germany
germany <- backup
germany <- germany[
  germany$Date %in% seq(from = min(germany$Date), to = max(germany$Date), by = 5),
]
test <- sample(seq_len(nrow(germany)), size = floor(0.2 * nrow(germany)))
test_value <- germany$value[test]
germany$value[test] <- NA
link <- rep(NA, nrow(germany))
link[which(is.na(germany$value))] <- 1
#####################################################
# specify penalized prior
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
models <- list()
gof <- list()
mae <- list()
# create the neighbordhood matrix
nb <- poly2nb(germany[!duplicated(germany$municipality_id), ])
# save the matrix
nb2INLA("maps/map_4.adj", nb)
g <- inla.read.graph(filename = "maps/map_4.adj")
Q <- Diagonal(x = sapply(nb, length))
for (i in 2:length(nb)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = length(nb)) - Q
# formula for the besag model
formula_1 <- value ~
  pop_dens + urb_dens + sex + log(trade_tax) + SPD + Gruene + FDP + die_linke +
  clinic + place_of_worship + retail + nursing_home + aerodrome + platform +
  higher_education +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
# formula for the bym2 model
formula_2 <- value ~
  pop_dens + urb_dens + sex + log(trade_tax) + SPD + Gruene + FDP + die_linke +
  clinic + place_of_worship + retail + nursing_home + aerodrome + platform +
  higher_education +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
# formula for the leroux model
formula_3 <- value ~
  pop_dens + urb_dens + sex + log(trade_tax) + SPD + Gruene + FDP + die_linke +
  clinic + place_of_worship + retail + nursing_home + aerodrome + platform +
  higher_education +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
# compute the models
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = germany,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  verbose = TRUE
)
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = germany,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  verbose = TRUE
)
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = germany,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  verbose = TRUE
)
models <- c(models, list(res_1, res_2, res_3))
# get the goodness of fit indicators
gof <- c(gof, list(
  list(
    dic = res_1$dic$dic,
    waic = res_1$waic$waic,
    cpo = sum(log(res_1$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_2$dic$dic,
    waic = res_2$waic$waic,
    cpo = sum(log(res_2$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_3$dic$dic,
    waic = res_3$waic$waic,
    cpo = sum(log(res_3$cpo$cpo), na.rm = TRUE)
  )
))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
# make predictions
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_1$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_2$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_3$marginals.fitted.values[[i]]
  )
}
# calculate the mae
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value)),
  mean(abs(predicted_3[test] - test_value))
))
models_final <- list(models, gof, mae)
# save the models
save(models_final, file = "models/temporal_germany.RDa")
rm(list = ls())
