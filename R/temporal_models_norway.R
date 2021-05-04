library(INLA)
library(spdep)
source("R/preprocess_norge_temporal.R")
set.seed(7918)
backup <- norge
norge <- backup
norge <- norge[
  norge$date %in% seq(from = min(norge$date), to = max(norge$date), by = 5),
]
test <- sample(seq_len(nrow(norge)), size = floor(0.2 * nrow(norge)))
test_value <- norge$value[test]
norge$value[test] <- NA
link <- rep(NA, nrow(norge))
link[which(is.na(norge$value))] <- 1
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
nb <- poly2nb(norge[!duplicated(norge$kommune_no), ])
# save the matrix
nb2INLA("maps/map_3.adj", nb)
g <- inla.read.graph(filename = "maps/map_3.adj")
Q <- Diagonal(x = sapply(nb, length))
for (i in 2:length(nb)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = length(nb)) - Q
lcs <- inla.make.lincombs(
  id_date_1 = diag(length(unique(norge$id_date_1))),
  id_date_2 = diag(length(unique(norge$id_date_1)))
)
# formula for the besag model
formula_1 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2", hyper = prior_1)
# formula for the bym2 model
formula_2 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1) +
  f(id_date_1, model = "rw2", hyper = prior_1)
# formula for the leroux model
formula_3 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(id_date_1, model = "rw2", hyper = prior_1)
# compute the models
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  verbose = TRUE
)
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  verbose = TRUE
)
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
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
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_1$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_2$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
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
save(models_final, file = "models/temporal_norway.RDa")
rm(list = ls())
