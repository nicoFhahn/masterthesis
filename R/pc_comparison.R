library(readr)
library(sf)
library(spdep)
library(INLA)
library(tibble)
newest_numbers <- read_csv("eval_data/newest_numbers_norway_march24.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
newest_numbers <- merge(
  newest_numbers,
  norge_sf,
  by = "kommune_no"
)
newest_numbers <- st_as_sf(newest_numbers)
set.seed(7918)
test <- sample(
  seq_len(nrow(newest_numbers)),
  size = floor(0.2 * nrow(newest_numbers))
)
test_value <- newest_numbers$value[test]
newest_numbers$value[test] <- NA
link <- rep(NA, nrow(newest_numbers))
link[which(is.na(newest_numbers$value))] <- 1
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
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.01, 0.01)
  )
)
prior_2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.1, 0.01)
  )
)
prior_3 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.2, 0.01)
  )
)
prior_4 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.4, 0.01)
  )
)
prior_5 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.6, 0.01)
  )
)
prior_6 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.8, 0.01)
  )
)
prior_7 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
prior_8 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1.5, 0.01)
  )
)
prior_9 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(2.5, 0.01)
  )
)
prior_10 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(5, 0.01)
  )
)
formula_besag_1 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1)
formula_bym2_1 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_leroux_1 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_besag_2 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2)
formula_bym2_2 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_leroux_2 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_besag_3 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_3)
formula_bym2_3 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_3)
formula_leroux_3 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_3)
formula_besag_4 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_4)
formula_bym2_4 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_4)
formula_leroux_4 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_4)
formula_besag_5 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_5)
formula_bym2_5 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_5)
formula_leroux_5 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_5)
formula_besag_6 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_6)
formula_bym2_6 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_6)
formula_leroux_6 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_6)
formula_besag_7 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_7)
formula_bym2_7 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_7)
formula_leroux_7 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_7)
formula_besag_8 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_8)
formula_bym2_8 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_8)
formula_leroux_8 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_8)
formula_besag_9 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_9)
formula_bym2_9 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_9)
formula_leroux_9 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_9)
formula_besag_10 <- value ~
urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_10)
formula_bym2_10 <- value ~
urb_dens + sex +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_10)
formula_leroux_10 <- value ~
urb_dens + sex +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_10)
res_besag_1 <- inla(
  formula_besag_1,
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
res_bym2_1 <- inla(
  formula_bym2_1,
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
res_leroux_1 <- inla(
  formula_leroux_1,
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
res_besag_2 <- inla(
  formula_besag_2,
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
res_bym2_2 <- inla(
  formula_bym2_2,
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
res_leroux_2 <- inla(
  formula_leroux_2,
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
res_besag_3 <- inla(
  formula_besag_3,
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
res_bym2_3 <- inla(
  formula_bym2_3,
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
res_leroux_3 <- inla(
  formula_leroux_3,
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
res_besag_4 <- inla(
  formula_besag_4,
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
res_bym2_4 <- inla(
  formula_bym2_4,
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
res_leroux_4 <- inla(
  formula_leroux_4,
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
res_besag_5 <- inla(
  formula_besag_5,
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
res_bym2_5 <- inla(
  formula_bym2_5,
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
res_leroux_5 <- inla(
  formula_leroux_5,
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
res_besag_6 <- inla(
  formula_besag_6,
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
res_bym2_6 <- inla(
  formula_bym2_6,
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
res_leroux_6 <- inla(
  formula_leroux_6,
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
res_besag_7 <- inla(
  formula_besag_7,
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
res_bym2_7 <- inla(
  formula_bym2_7,
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
res_leroux_7 <- inla(
  formula_leroux_7,
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
res_besag_8 <- inla(
  formula_besag_8,
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
res_bym2_8 <- inla(
  formula_bym2_8,
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
res_leroux_8 <- inla(
  formula_leroux_8,
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
res_besag_9 <- inla(
  formula_besag_9,
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
res_bym2_9 <- inla(
  formula_bym2_9,
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
res_leroux_9 <- inla(
  formula_leroux_9,
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
res_besag_10 <- inla(
  formula_besag_10,
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
res_bym2_10 <- inla(
  formula_bym2_10,
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
res_leroux_10 <- inla(
  formula_leroux_10,
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
predicted_besag_1 <- c()
predicted_bym2_1 <- c()
predicted_leroux_1 <- c()
predicted_besag_2 <- c()
predicted_bym2_2 <- c()
predicted_leroux_2 <- c()
predicted_besag_3 <- c()
predicted_bym2_3 <- c()
predicted_leroux_3 <- c()
predicted_besag_4 <- c()
predicted_bym2_4 <- c()
predicted_leroux_4 <- c()
predicted_besag_5 <- c()
predicted_bym2_5 <- c()
predicted_leroux_5 <- c()
predicted_besag_6 <- c()
predicted_bym2_6 <- c()
predicted_leroux_6 <- c()
predicted_besag_7 <- c()
predicted_bym2_7 <- c()
predicted_leroux_7 <- c()
predicted_besag_8 <- c()
predicted_bym2_8 <- c()
predicted_leroux_8 <- c()
predicted_besag_9 <- c()
predicted_bym2_9 <- c()
predicted_leroux_9 <- c()
predicted_besag_10 <- c()
predicted_bym2_10 <- c()
predicted_leroux_10 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_besag_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_1$marginals.fitted.values[[i]]
  )
  predicted_bym2_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_1$marginals.fitted.values[[i]]
  )
  predicted_leroux_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_1$marginals.fitted.values[[i]]
  )
  predicted_besag_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_2$marginals.fitted.values[[i]]
  )
  predicted_bym2_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_2$marginals.fitted.values[[i]]
  )
  predicted_leroux_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_2$marginals.fitted.values[[i]]
  )
  predicted_besag_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_3$marginals.fitted.values[[i]]
  )
  predicted_bym2_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_3$marginals.fitted.values[[i]]
  )
  predicted_leroux_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_3$marginals.fitted.values[[i]]
  )
  predicted_besag_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_4$marginals.fitted.values[[i]]
  )
  predicted_bym2_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_4$marginals.fitted.values[[i]]
  )
  predicted_leroux_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_4$marginals.fitted.values[[i]]
  )
  predicted_besag_5[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_5$marginals.fitted.values[[i]]
  )
  predicted_bym2_5[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_5$marginals.fitted.values[[i]]
  )
  predicted_leroux_5[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_5$marginals.fitted.values[[i]]
  )
  predicted_besag_6[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_6$marginals.fitted.values[[i]]
  )
  predicted_bym2_6[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_6$marginals.fitted.values[[i]]
  )
  predicted_leroux_6[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_6$marginals.fitted.values[[i]]
  )
  predicted_besag_7[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_7$marginals.fitted.values[[i]]
  )
  predicted_bym2_7[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_7$marginals.fitted.values[[i]]
  )
  predicted_leroux_7[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_7$marginals.fitted.values[[i]]
  )
  predicted_besag_8[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_8$marginals.fitted.values[[i]]
  )
  predicted_bym2_8[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_8$marginals.fitted.values[[i]]
  )
  predicted_leroux_8[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_8$marginals.fitted.values[[i]]
  )
  predicted_besag_9[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_9$marginals.fitted.values[[i]]
  )
  predicted_bym2_9[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_9$marginals.fitted.values[[i]]
  )
  predicted_leroux_9[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_9$marginals.fitted.values[[i]]
  )
  predicted_besag_10[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_10$marginals.fitted.values[[i]]
  )
  predicted_bym2_10[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_10$marginals.fitted.values[[i]]
  )
  predicted_leroux_10[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_10$marginals.fitted.values[[i]]
  )
}

mae <- c(list(
  mean(abs(predicted_besag_1[test] - test_value)),
  mean(abs(predicted_bym2_1[test] - test_value)),
  mean(abs(predicted_leroux_1[test] - test_value)),
  mean(abs(predicted_besag_2[test] - test_value)),
  mean(abs(predicted_bym2_2[test] - test_value)),
  mean(abs(predicted_leroux_2[test] - test_value)),
  mean(abs(predicted_besag_3[test] - test_value)),
  mean(abs(predicted_bym2_3[test] - test_value)),
  mean(abs(predicted_leroux_3[test] - test_value)),
  mean(abs(predicted_besag_4[test] - test_value)),
  mean(abs(predicted_bym2_4[test] - test_value)),
  mean(abs(predicted_leroux_4[test] - test_value)),
  mean(abs(predicted_besag_5[test] - test_value)),
  mean(abs(predicted_bym2_5[test] - test_value)),
  mean(abs(predicted_leroux_5[test] - test_value)),
  mean(abs(predicted_besag_6[test] - test_value)),
  mean(abs(predicted_bym2_6[test] - test_value)),
  mean(abs(predicted_leroux_6[test] - test_value)),
  mean(abs(predicted_besag_7[test] - test_value)),
  mean(abs(predicted_bym2_7[test] - test_value)),
  mean(abs(predicted_leroux_7[test] - test_value)),
  mean(abs(predicted_besag_8[test] - test_value)),
  mean(abs(predicted_bym2_8[test] - test_value)),
  mean(abs(predicted_leroux_8[test] - test_value)),
  mean(abs(predicted_besag_9[test] - test_value)),
  mean(abs(predicted_bym2_9[test] - test_value)),
  mean(abs(predicted_leroux_9[test] - test_value)),
  mean(abs(predicted_besag_10[test] - test_value)),
  mean(abs(predicted_bym2_10[test] - test_value)),
  mean(abs(predicted_leroux_10[test] - test_value))
))

dic <- c(list(
  res_besag_1$dic$dic,
  res_bym2_1$dic$dic,
  res_leroux_1$dic$dic,
  res_besag_2$dic$dic,
  res_bym2_2$dic$dic,
  res_leroux_2$dic$dic,
  res_besag_3$dic$dic,
  res_bym2_3$dic$dic,
  res_leroux_3$dic$dic,
  res_besag_4$dic$dic,
  res_bym2_4$dic$dic,
  res_leroux_4$dic$dic,
  res_besag_5$dic$dic,
  res_bym2_5$dic$dic,
  res_leroux_5$dic$dic,
  res_besag_6$dic$dic,
  res_bym2_6$dic$dic,
  res_leroux_6$dic$dic,
  res_besag_7$dic$dic,
  res_bym2_7$dic$dic,
  res_leroux_7$dic$dic,
  res_besag_8$dic$dic,
  res_bym2_8$dic$dic,
  res_leroux_8$dic$dic,
  res_besag_9$dic$dic,
  res_bym2_9$dic$dic,
  res_leroux_9$dic$dic,
  res_besag_10$dic$dic,
  res_bym2_10$dic$dic,
  res_leroux_10$dic$dic
))

waic <- c(list(
  res_besag_1$waic$waic,
  res_bym2_1$waic$waic,
  res_leroux_1$waic$waic,
  res_besag_2$waic$waic,
  res_bym2_2$waic$waic,
  res_leroux_2$waic$waic,
  res_besag_3$waic$waic,
  res_bym2_3$waic$waic,
  res_leroux_3$waic$waic,
  res_besag_4$waic$waic,
  res_bym2_4$waic$waic,
  res_leroux_4$waic$waic,
  res_besag_5$waic$waic,
  res_bym2_5$waic$waic,
  res_leroux_5$waic$waic,
  res_besag_6$waic$waic,
  res_bym2_6$waic$waic,
  res_leroux_6$waic$waic,
  res_besag_7$waic$waic,
  res_bym2_7$waic$waic,
  res_leroux_7$waic$waic,
  res_besag_8$waic$waic,
  res_bym2_8$waic$waic,
  res_leroux_8$waic$waic,
  res_besag_9$waic$waic,
  res_bym2_9$waic$waic,
  res_leroux_9$waic$waic,
  res_besag_10$waic$waic,
  res_bym2_10$waic$waic,
  res_leroux_10$waic$waic
))

cpo <- c(list(
  sum(log(res_besag_1$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_1$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_1$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_2$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_2$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_2$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_3$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_3$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_3$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_4$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_4$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_4$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_5$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_5$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_5$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_6$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_6$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_6$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_7$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_7$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_7$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_8$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_8$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_8$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_9$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_9$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_9$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_10$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_10$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_10$cpo$cpo), na.rm = TRUE)
))

results <- tibble(
  dic = unlist(dic),
  waic = unlist(waic),
  cpo = unlist(cpo),
  mae = unlist(mae),
  model = rep(c("Besag", "Bym2", "Leroux"), 10),
  U = c(
    rep(0.01, 3),
    rep(0.1, 3),
    rep(0.2, 3),
    rep(0.4, 3),
    rep(0.6, 3),
    rep(0.8, 3),
    rep(1, 3),
    rep(1.5, 3),
    rep(2.5, 3),
    rep(5, 3)
  ),
  alpha = c(
    rep(0.01, 30)
  )
)
library(ggplot2)
library(patchwork)
plot_1 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = dic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "DIC, alpha = 0.01, country = Norway"
  ) +
  labs(
    x = "U",
    y = "DIC"
  ) +
  theme(
    legend.position = "none"
  )
plot_2 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = waic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "WAIC, alpha = 0.01, country = Norway"
  ) +
  labs(
    x = "U",
    y = "WAIC"
  )
plot_3 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = cpo,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "CPO, alpha = 0.01, country = Norway"
  ) +
  labs(
    x = "U",
    y = "CPO"
  ) +
  theme(
    legend.position = "none"
  )
plot_4 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = mae,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "MAE, alpha = 0.01, country = Norway"
  ) +
  labs(
    x = "U",
    y = "MAE"
  )
plot_1 + plot_2
plot_3 + plot_4
library(readr)
library(sf)
library(spdep)
library(INLA)
newest_numbers <- read_csv("eval_data/newest_numbers_germany_march24.csv")
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
newest_numbers <- merge(
  newest_numbers,
  germany_sf,
  by.x = "municipality_id",
  by.y = "Kennziffer"
)
newest_numbers <- st_as_sf(newest_numbers)
set.seed(145)
test <- sample(
  seq_len(nrow(newest_numbers)),
  size = floor(0.2 * nrow(newest_numbers))
)
test_value <- newest_numbers$value[test]
newest_numbers$value[test] <- NA
link <- rep(NA, nrow(newest_numbers))
link[which(is.na(newest_numbers$value))] <- 1
nb <- poly2nb(newest_numbers)
# save the matrix
nb2INLA("maps/map_2.adj", nb)
g <- inla.read.graph(filename = "maps/map_2.adj")
Q <- Diagonal(x = sapply(nb, length))
for (i in 2:nrow(newest_numbers)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = nrow(newest_numbers)) - Q
formula_besag_1 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1)
formula_bym2_1 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_leroux_1 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_besag_2 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2)
formula_bym2_2 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_leroux_2 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_besag_3 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_3)
formula_bym2_3 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_3)
formula_leroux_3 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_3)
formula_besag_4 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_4)
formula_bym2_4 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_4)
formula_leroux_4 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_4)
formula_besag_5 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_5)
formula_bym2_5 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_5)
formula_leroux_5 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_5)
formula_besag_6 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_6)
formula_bym2_6 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_6)
formula_leroux_6 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_6)
formula_besag_7 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_7)
formula_bym2_7 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_7)
formula_leroux_7 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_7)
formula_besag_8 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_8)
formula_bym2_8 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_8)
formula_leroux_8 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_8)
formula_besag_9 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_9)
formula_bym2_9 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_9)
formula_leroux_9 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_9)
formula_besag_10 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_10)
formula_bym2_10 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_10)
formula_leroux_10 <- value ~
pop_dens + urb_dens + Gruene + FDP + die_linke + SPD +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_10)

res_besag_1 <- inla(
  formula_besag_1,
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
res_bym2_1 <- inla(
  formula_bym2_1,
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
res_leroux_1 <- inla(
  formula_leroux_1,
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
res_besag_2 <- inla(
  formula_besag_2,
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
res_bym2_2 <- inla(
  formula_bym2_2,
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
res_leroux_2 <- inla(
  formula_leroux_2,
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
res_besag_3 <- inla(
  formula_besag_3,
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
res_bym2_3 <- inla(
  formula_bym2_3,
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
res_leroux_3 <- inla(
  formula_leroux_3,
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
res_besag_4 <- inla(
  formula_besag_4,
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
res_bym2_4 <- inla(
  formula_bym2_4,
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
res_leroux_4 <- inla(
  formula_leroux_4,
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
res_besag_5 <- inla(
  formula_besag_5,
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
res_bym2_5 <- inla(
  formula_bym2_5,
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
res_leroux_5 <- inla(
  formula_leroux_5,
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
res_besag_6 <- inla(
  formula_besag_6,
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
res_bym2_6 <- inla(
  formula_bym2_6,
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
res_leroux_6 <- inla(
  formula_leroux_6,
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
res_besag_7 <- inla(
  formula_besag_7,
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
res_bym2_7 <- inla(
  formula_bym2_7,
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
res_leroux_7 <- inla(
  formula_leroux_7,
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
res_besag_8 <- inla(
  formula_besag_8,
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
res_bym2_8 <- inla(
  formula_bym2_8,
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
res_leroux_8 <- inla(
  formula_leroux_8,
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
res_besag_9 <- inla(
  formula_besag_9,
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
res_bym2_9 <- inla(
  formula_bym2_9,
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
res_leroux_9 <- inla(
  formula_leroux_9,
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
res_besag_10 <- inla(
  formula_besag_10,
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
res_bym2_10 <- inla(
  formula_bym2_10,
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
res_leroux_10 <- inla(
  formula_leroux_10,
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

predicted_besag_1 <- c()
predicted_bym2_1 <- c()
predicted_leroux_1 <- c()
predicted_besag_2 <- c()
predicted_bym2_2 <- c()
predicted_leroux_2 <- c()
predicted_besag_3 <- c()
predicted_bym2_3 <- c()
predicted_leroux_3 <- c()
predicted_besag_4 <- c()
predicted_bym2_4 <- c()
predicted_leroux_4 <- c()
predicted_besag_5 <- c()
predicted_bym2_5 <- c()
predicted_leroux_5 <- c()
predicted_besag_6 <- c()
predicted_bym2_6 <- c()
predicted_leroux_6 <- c()
predicted_besag_7 <- c()
predicted_bym2_7 <- c()
predicted_leroux_7 <- c()
predicted_besag_8 <- c()
predicted_bym2_8 <- c()
predicted_leroux_8 <- c()
predicted_besag_9 <- c()
predicted_bym2_9 <- c()
predicted_leroux_9 <- c()
predicted_besag_10 <- c()
predicted_bym2_10 <- c()
predicted_leroux_10 <- c()

for (i in seq_len(nrow(newest_numbers))) {
  predicted_besag_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_1$marginals.fitted.values[[i]]
  )
  predicted_bym2_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_1$marginals.fitted.values[[i]]
  )
  predicted_leroux_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_1$marginals.fitted.values[[i]]
  )
  predicted_besag_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_2$marginals.fitted.values[[i]]
  )
  predicted_bym2_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_2$marginals.fitted.values[[i]]
  )
  predicted_leroux_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_2$marginals.fitted.values[[i]]
  )
  predicted_besag_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_3$marginals.fitted.values[[i]]
  )
  predicted_bym2_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_3$marginals.fitted.values[[i]]
  )
  predicted_leroux_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_3$marginals.fitted.values[[i]]
  )
  predicted_besag_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_4$marginals.fitted.values[[i]]
  )
  predicted_bym2_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_4$marginals.fitted.values[[i]]
  )
  predicted_leroux_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_4$marginals.fitted.values[[i]]
  )
  predicted_besag_5[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_5$marginals.fitted.values[[i]]
  )
  predicted_bym2_5[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_5$marginals.fitted.values[[i]]
  )
  predicted_leroux_5[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_5$marginals.fitted.values[[i]]
  )
  predicted_besag_6[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_6$marginals.fitted.values[[i]]
  )
  predicted_bym2_6[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_6$marginals.fitted.values[[i]]
  )
  predicted_leroux_6[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_6$marginals.fitted.values[[i]]
  )
  predicted_besag_7[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_7$marginals.fitted.values[[i]]
  )
  predicted_bym2_7[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_7$marginals.fitted.values[[i]]
  )
  predicted_leroux_7[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_7$marginals.fitted.values[[i]]
  )
  predicted_besag_8[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_8$marginals.fitted.values[[i]]
  )
  predicted_bym2_8[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_8$marginals.fitted.values[[i]]
  )
  predicted_leroux_8[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_8$marginals.fitted.values[[i]]
  )
  predicted_besag_9[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_9$marginals.fitted.values[[i]]
  )
  predicted_bym2_9[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_9$marginals.fitted.values[[i]]
  )
  predicted_leroux_9[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_9$marginals.fitted.values[[i]]
  )
  predicted_besag_10[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_10$marginals.fitted.values[[i]]
  )
  predicted_bym2_10[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_10$marginals.fitted.values[[i]]
  )
  predicted_leroux_10[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_10$marginals.fitted.values[[i]]
  )
}

mae <- c(list(
  mean(abs(predicted_besag_1[test] - test_value)),
  mean(abs(predicted_bym2_1[test] - test_value)),
  mean(abs(predicted_leroux_1[test] - test_value)),
  mean(abs(predicted_besag_2[test] - test_value)),
  mean(abs(predicted_bym2_2[test] - test_value)),
  mean(abs(predicted_leroux_2[test] - test_value)),
  mean(abs(predicted_besag_3[test] - test_value)),
  mean(abs(predicted_bym2_3[test] - test_value)),
  mean(abs(predicted_leroux_3[test] - test_value)),
  mean(abs(predicted_besag_4[test] - test_value)),
  mean(abs(predicted_bym2_4[test] - test_value)),
  mean(abs(predicted_leroux_4[test] - test_value)),
  mean(abs(predicted_besag_5[test] - test_value)),
  mean(abs(predicted_bym2_5[test] - test_value)),
  mean(abs(predicted_leroux_5[test] - test_value)),
  mean(abs(predicted_besag_6[test] - test_value)),
  mean(abs(predicted_bym2_6[test] - test_value)),
  mean(abs(predicted_leroux_6[test] - test_value)),
  mean(abs(predicted_besag_7[test] - test_value)),
  mean(abs(predicted_bym2_7[test] - test_value)),
  mean(abs(predicted_leroux_7[test] - test_value)),
  mean(abs(predicted_besag_8[test] - test_value)),
  mean(abs(predicted_bym2_8[test] - test_value)),
  mean(abs(predicted_leroux_8[test] - test_value)),
  mean(abs(predicted_besag_9[test] - test_value)),
  mean(abs(predicted_bym2_9[test] - test_value)),
  mean(abs(predicted_leroux_9[test] - test_value)),
  mean(abs(predicted_besag_10[test] - test_value)),
  mean(abs(predicted_bym2_10[test] - test_value)),
  mean(abs(predicted_leroux_10[test] - test_value))
))

dic <- c(list(
  res_besag_1$dic$dic,
  res_bym2_1$dic$dic,
  res_leroux_1$dic$dic,
  res_besag_2$dic$dic,
  res_bym2_2$dic$dic,
  res_leroux_2$dic$dic,
  res_besag_3$dic$dic,
  res_bym2_3$dic$dic,
  res_leroux_3$dic$dic,
  res_besag_4$dic$dic,
  res_bym2_4$dic$dic,
  res_leroux_4$dic$dic,
  res_besag_5$dic$dic,
  res_bym2_5$dic$dic,
  res_leroux_5$dic$dic,
  res_besag_6$dic$dic,
  res_bym2_6$dic$dic,
  res_leroux_6$dic$dic,
  res_besag_7$dic$dic,
  res_bym2_7$dic$dic,
  res_leroux_7$dic$dic,
  res_besag_8$dic$dic,
  res_bym2_8$dic$dic,
  res_leroux_8$dic$dic,
  res_besag_9$dic$dic,
  res_bym2_9$dic$dic,
  res_leroux_9$dic$dic,
  res_besag_10$dic$dic,
  res_bym2_10$dic$dic,
  res_leroux_10$dic$dic
))

waic <- c(list(
  res_besag_1$waic$waic,
  res_bym2_1$waic$waic,
  res_leroux_1$waic$waic,
  res_besag_2$waic$waic,
  res_bym2_2$waic$waic,
  res_leroux_2$waic$waic,
  res_besag_3$waic$waic,
  res_bym2_3$waic$waic,
  res_leroux_3$waic$waic,
  res_besag_4$waic$waic,
  res_bym2_4$waic$waic,
  res_leroux_4$waic$waic,
  res_besag_5$waic$waic,
  res_bym2_5$waic$waic,
  res_leroux_5$waic$waic,
  res_besag_6$waic$waic,
  res_bym2_6$waic$waic,
  res_leroux_6$waic$waic,
  res_besag_7$waic$waic,
  res_bym2_7$waic$waic,
  res_leroux_7$waic$waic,
  res_besag_8$waic$waic,
  res_bym2_8$waic$waic,
  res_leroux_8$waic$waic,
  res_besag_9$waic$waic,
  res_bym2_9$waic$waic,
  res_leroux_9$waic$waic,
  res_besag_10$waic$waic,
  res_bym2_10$waic$waic,
  res_leroux_10$waic$waic
))

cpo <- c(list(
  sum(log(res_besag_1$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_1$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_1$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_2$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_2$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_2$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_3$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_3$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_3$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_4$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_4$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_4$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_5$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_5$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_5$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_6$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_6$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_6$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_7$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_7$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_7$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_8$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_8$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_8$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_9$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_9$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_9$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_10$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_10$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_10$cpo$cpo), na.rm = TRUE)
))

results <- tibble(
  dic = unlist(dic),
  waic = unlist(waic),
  cpo = unlist(cpo),
  mae = unlist(mae),
  model = rep(c("Besag", "Bym2", "Leroux"), 10),
  U = c(
    rep(0.01, 3),
    rep(0.1, 3),
    rep(0.2, 3),
    rep(0.4, 3),
    rep(0.6, 3),
    rep(0.8, 3),
    rep(1, 3),
    rep(1.5, 3),
    rep(2.5, 3),
    rep(5, 3)
  ),
  alpha = c(
    rep(0.01, 30)
  )
)
plot_1 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = dic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "DIC, alpha = 0.01, country = Germany"
  ) +
  labs(
    x = "U",
    y = "DIC"
  ) +
  theme(
    legend.position = "none"
  )
plot_2 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = waic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "WAIC, alpha = 0.01, country = Germany"
  ) +
  labs(
    x = "U",
    y = "WAIC"
  )
plot_3 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = cpo,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "CPO, alpha = 0.01, country = Germany"
  ) +
  labs(
    x = "U",
    y = "CPO"
  ) +
  theme(
    legend.position = "none"
  )
plot_4 <- ggplot(
  data = results[1:30, ],
  aes(
    x = U,
    y = mae,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "MAE, alpha = 0.01, country = Germany"
  ) +
  labs(
    x = "U",
    y = "MAE"
  )
plot_1 + plot_2
plot_3 + plot_4