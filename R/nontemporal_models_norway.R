library(INLA)
library(spdep)
source("R/preprocess_norge.R")
set.seed(7918)
test <- sample(
  seq_len(nrow(newest_numbers)),
  size = floor(0.2 * nrow(newest_numbers))
)
test_value <- newest_numbers$value[test]
newest_numbers$value[test] <- NA
link <- rep(NA, nrow(newest_numbers))
link[which(is.na(newest_numbers$value))] <- 1
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
#
# create the neighbordhood matrix
nb <- poly2nb(newest_numbers)
# save the matrix
nb2INLA("maps/map_1.adj", nb)
g <- inla.read.graph(filename = "maps/map_1.adj")
Q <- Diagonal(x = sapply(nb, length))
for (i in 2:nrow(newest_numbers)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = nrow(newest_numbers)) - Q
# formula for the non spatial model
formula_1 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots
# formula for the besag model
formula_2 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1)
# formula for the bym2 model
formula_3 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
# formula for the leroux model
formula_4 <- value ~
urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
# compute the models
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = newest_numbers$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = newest_numbers$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = newest_numbers$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = newest_numbers$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
models <- c(models, list(res_1, res_2, res_3, res_4))
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
  ),
  list(
    dic = res_4$dic$dic,
    waic = res_4$waic$waic,
    cpo = sum(log(res_4$cpo$cpo), na.rm = TRUE)
  )
))
# calculate the mae
mae <- c(mae, list(
  mean(abs(res_1$summary.fitted.values$mean[test] * newest_numbers$expected_count[test] - test_value)),
  mean(abs(res_2$summary.fitted.values$mean[test] * newest_numbers$expected_count[test] - test_value)),
  mean(abs(res_3$summary.fitted.values$mean[test] * newest_numbers$expected_count[test] - test_value)),
  mean(abs(res_4$summary.fitted.values$mean[test] * newest_numbers$expected_count[test] - test_value)),
  mean(abs(res_1$summary.fitted.values$mean[-test] * newest_numbers$expected_count[-test] - newest_numbers$value[-test])),
  mean(abs(res_2$summary.fitted.values$mean[-test] * newest_numbers$expected_count[-test] - newest_numbers$value[-test])),
  mean(abs(res_3$summary.fitted.values$mean[-test] * newest_numbers$expected_count[-test] - newest_numbers$value[-test])),
  mean(abs(res_4$summary.fitted.values$mean[-test] * newest_numbers$expected_count[-test] - newest_numbers$value[-test]))
))
models_final <- list(models, gof, mae)
# save the models
save(models_final, file = "models/nontemporal_norway.RDa")
rm(list = ls())
