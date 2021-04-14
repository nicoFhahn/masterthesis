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
    param = c(0.1, 0.01)
  )
)
prior_2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5, 0.01)
  )
)
prior_3 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
prior_4 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1.5, 0.01)
  )
)
prior_5 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(2, 0.01)
  )
)
prior_6 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(2.5, 0.01)
  )
)
prior_7 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(3, 0.01)
  )
)
prior_8 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(5, 0.01)
  )
)
prior_9 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(10, 0.01)
  )
)
prior_10 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(50, 0.01)
  )
)
prior_11 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.025)
  )
)
prior_12 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.05)
  )
)
prior_13 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.1)
  )
)
prior_14 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.25)
  )
)
prior_15 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.5)
  )
)
prior_16 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.75)
  )
)
prior_17 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.99)
  )
)
prior_18 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.001)
  )
)
formula_besag_1 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1)
formula_bym2_1 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_leroux_1 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_besag_2 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2)
formula_bym2_2 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_leroux_2 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_besag_3 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_3)
formula_bym2_3 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_3)
formula_leroux_3 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_3)
formula_besag_4 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_4)
formula_bym2_4 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_4)
formula_leroux_4 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_4)
formula_besag_5 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_5)
formula_bym2_5 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_5)
formula_leroux_5 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_5)
formula_besag_6 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_6)
formula_bym2_6 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_6)
formula_leroux_6 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_6)
formula_besag_7 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_7)
formula_bym2_7 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_7)
formula_leroux_7 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_7)
formula_besag_8 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_8)
formula_bym2_8 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_8)
formula_leroux_8 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_8)
formula_besag_9 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_9)
formula_bym2_9 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_9)
formula_leroux_9 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_9)
formula_besag_10 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_10)
formula_bym2_10 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_10)
formula_leroux_10 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_10)
formula_besag_11 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_11)
formula_bym2_11 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_11)
formula_leroux_11 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_11)
formula_besag_12 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_12)
formula_bym2_12 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_12)
formula_leroux_12 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_12)
formula_besag_13 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_13)
formula_bym2_13 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_13)
formula_leroux_13 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_13)
formula_besag_14 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_14)
formula_bym2_14 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_14)
formula_leroux_14 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_14)
formula_besag_15 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_15)
formula_bym2_15 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_15)
formula_leroux_15 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_15)
formula_besag_16 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_16)
formula_bym2_16 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_16)
formula_leroux_16 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_16)
formula_besag_17 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_17)
formula_bym2_17 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_17)
formula_leroux_17 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_17)
formula_besag_18 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_18)
formula_bym2_18 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_18)
formula_leroux_18 <- value ~
unemp_tot + unemp_immg +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_18)

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
res_besag_11 <- inla(
  formula_besag_11,
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
res_bym2_11 <- inla(
  formula_bym2_11,
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
res_leroux_11 <- inla(
  formula_leroux_11,
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
res_besag_12 <- inla(
  formula_besag_12,
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
res_bym2_12 <- inla(
  formula_bym2_12,
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
res_leroux_12 <- inla(
  formula_leroux_12,
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
res_besag_13 <- inla(
  formula_besag_13,
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
res_bym2_13 <- inla(
  formula_bym2_13,
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
res_leroux_13 <- inla(
  formula_leroux_13,
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
res_besag_14 <- inla(
  formula_besag_14,
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
res_bym2_14 <- inla(
  formula_bym2_14,
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
res_leroux_14 <- inla(
  formula_leroux_14,
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
res_besag_15 <- inla(
  formula_besag_15,
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
res_bym2_15 <- inla(
  formula_bym2_15,
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
res_leroux_15 <- inla(
  formula_leroux_15,
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
res_besag_16 <- inla(
  formula_besag_16,
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
res_bym2_16 <- inla(
  formula_bym2_16,
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
res_leroux_16 <- inla(
  formula_leroux_16,
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
res_besag_17 <- inla(
  formula_besag_17,
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
res_bym2_17 <- inla(
  formula_bym2_17,
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
res_leroux_17 <- inla(
  formula_leroux_17,
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
res_besag_18 <- inla(
  formula_besag_18,
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
res_bym2_18 <- inla(
  formula_bym2_18,
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
res_leroux_18 <- inla(
  formula_leroux_18,
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
predicted_besag_11 <- c()
predicted_bym2_11 <- c()
predicted_leroux_11 <- c()
predicted_besag_12 <- c()
predicted_bym2_12 <- c()
predicted_leroux_12 <- c()
predicted_besag_13 <- c()
predicted_bym2_13 <- c()
predicted_leroux_13 <- c()
predicted_besag_14 <- c()
predicted_bym2_14 <- c()
predicted_leroux_14 <- c()
predicted_besag_15 <- c()
predicted_bym2_15 <- c()
predicted_leroux_15 <- c()
predicted_besag_16 <- c()
predicted_bym2_16 <- c()
predicted_leroux_16 <- c()
predicted_besag_17 <- c()
predicted_bym2_17 <- c()
predicted_leroux_17 <- c()
predicted_besag_18 <- c()
predicted_bym2_18 <- c()
predicted_leroux_18 <- c()
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
  predicted_besag_11[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_11$marginals.fitted.values[[i]]
  )
  predicted_bym2_11[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_11$marginals.fitted.values[[i]]
  )
  predicted_leroux_11[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_11$marginals.fitted.values[[i]]
  )
  predicted_besag_12[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_12$marginals.fitted.values[[i]]
  )
  predicted_bym2_12[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_12$marginals.fitted.values[[i]]
  )
  predicted_leroux_12[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_12$marginals.fitted.values[[i]]
  )
  predicted_besag_13[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_13$marginals.fitted.values[[i]]
  )
  predicted_bym2_13[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_13$marginals.fitted.values[[i]]
  )
  predicted_leroux_13[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_13$marginals.fitted.values[[i]]
  )
  predicted_besag_14[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_14$marginals.fitted.values[[i]]
  )
  predicted_bym2_14[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_14$marginals.fitted.values[[i]]
  )
  predicted_leroux_14[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_14$marginals.fitted.values[[i]]
  )
  predicted_besag_15[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_15$marginals.fitted.values[[i]]
  )
  predicted_bym2_15[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_15$marginals.fitted.values[[i]]
  )
  predicted_leroux_15[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_15$marginals.fitted.values[[i]]
  )
  predicted_besag_16[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_16$marginals.fitted.values[[i]]
  )
  predicted_bym2_16[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_16$marginals.fitted.values[[i]]
  )
  predicted_leroux_16[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_16$marginals.fitted.values[[i]]
  )
  predicted_besag_17[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_17$marginals.fitted.values[[i]]
  )
  predicted_bym2_17[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_17$marginals.fitted.values[[i]]
  )
  predicted_leroux_17[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_17$marginals.fitted.values[[i]]
  )
  predicted_besag_18[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_18$marginals.fitted.values[[i]]
  )
  predicted_bym2_18[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_18$marginals.fitted.values[[i]]
  )
  predicted_leroux_18[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_18$marginals.fitted.values[[i]]
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
  mean(abs(predicted_leroux_10[test] - test_value)),
  mean(abs(predicted_besag_11[test] - test_value)),
  mean(abs(predicted_bym2_11[test] - test_value)),
  mean(abs(predicted_leroux_11[test] - test_value)),
  mean(abs(predicted_besag_12[test] - test_value)),
  mean(abs(predicted_bym2_12[test] - test_value)),
  mean(abs(predicted_leroux_12[test] - test_value)),
  mean(abs(predicted_besag_13[test] - test_value)),
  mean(abs(predicted_bym2_13[test] - test_value)),
  mean(abs(predicted_leroux_13[test] - test_value)),
  mean(abs(predicted_besag_14[test] - test_value)),
  mean(abs(predicted_bym2_14[test] - test_value)),
  mean(abs(predicted_leroux_14[test] - test_value)),
  mean(abs(predicted_besag_15[test] - test_value)),
  mean(abs(predicted_bym2_15[test] - test_value)),
  mean(abs(predicted_leroux_15[test] - test_value)),
  mean(abs(predicted_besag_16[test] - test_value)),
  mean(abs(predicted_bym2_16[test] - test_value)),
  mean(abs(predicted_leroux_16[test] - test_value)),
  mean(abs(predicted_besag_17[test] - test_value)),
  mean(abs(predicted_bym2_17[test] - test_value)),
  mean(abs(predicted_leroux_17[test] - test_value)),
  mean(abs(predicted_besag_18[test] - test_value)),
  mean(abs(predicted_bym2_18[test] - test_value)),
  mean(abs(predicted_leroux_18[test] - test_value))
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
  res_leroux_10$dic$dic,
  res_besag_11$dic$dic,
  res_bym2_11$dic$dic,
  res_leroux_11$dic$dic,
  res_besag_12$dic$dic,
  res_bym2_12$dic$dic,
  res_leroux_12$dic$dic,
  res_besag_13$dic$dic,
  res_bym2_13$dic$dic,
  res_leroux_13$dic$dic,
  res_besag_14$dic$dic,
  res_bym2_14$dic$dic,
  res_leroux_14$dic$dic,
  res_besag_15$dic$dic,
  res_bym2_15$dic$dic,
  res_leroux_15$dic$dic,
  res_besag_16$dic$dic,
  res_bym2_16$dic$dic,
  res_leroux_16$dic$dic,
  res_besag_17$dic$dic,
  res_bym2_17$dic$dic,
  res_leroux_17$dic$dic,
  res_besag_18$dic$dic,
  res_bym2_18$dic$dic,
  res_leroux_18$dic$dic
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
  res_leroux_10$waic$waic,
  res_besag_11$waic$waic,
  res_bym2_11$waic$waic,
  res_leroux_11$waic$waic,
  res_besag_12$waic$waic,
  res_bym2_12$waic$waic,
  res_leroux_12$waic$waic,
  res_besag_13$waic$waic,
  res_bym2_13$waic$waic,
  res_leroux_13$waic$waic,
  res_besag_14$waic$waic,
  res_bym2_14$waic$waic,
  res_leroux_14$waic$waic,
  res_besag_15$waic$waic,
  res_bym2_15$waic$waic,
  res_leroux_15$waic$waic,
  res_besag_16$waic$waic,
  res_bym2_16$waic$waic,
  res_leroux_16$waic$waic,
  res_besag_17$waic$waic,
  res_bym2_17$waic$waic,
  res_leroux_17$waic$waic,
  res_besag_18$waic$waic,
  res_bym2_18$waic$waic,
  res_leroux_18$waic$waic
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
  sum(log(res_leroux_10$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_11$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_11$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_11$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_12$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_12$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_12$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_13$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_13$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_13$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_14$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_14$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_14$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_15$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_15$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_15$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_16$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_16$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_16$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_17$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_17$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_17$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_18$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_18$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_18$cpo$cpo), na.rm = TRUE)
))

results <- tibble(
  dic = unlist(dic),
  waic = unlist(waic),
  cpo = unlist(cpo),
  mae = unlist(mae),
  model = rep(c("Besag", "Bym2", "Leroux"), 18),
  U = c(
    rep(0.1, 3),
    rep(0.5, 3),
    rep(1, 3),
    rep(1.5, 3),
    rep(2, 3),
    rep(2.5, 3),
    rep(3, 3),
    rep(5, 3),
    rep(10, 3),
    rep(50, 3),
    rep(1, 24)
  ),
  alpha = c(
    rep(0.01, 30),
    rep(0.025, 3),
    rep(0.05, 3),
    rep(0.1, 3),
    rep(0.25, 3),
    rep(0.5, 3),
    rep(0.75, 3),
    rep(0.99, 3),
    rep(0.001, 3)
  )
)
results[9, 1] <- sum(res_leroux_3$dic$local.dic, na.rm = TRUE)
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
plot_5 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = dic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "DIC, U = 1, country = Norway"
  ) +
  labs(
    x = expression(alpha),
    y = "DIC"
  ) +
  theme(
    legend.position = "none"
  )
plot_6 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = waic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "WAIC, U = 1, country = Norway"
  ) +
  labs(
    x = expression(alpha),
    y = "WAIC"
  )
plot_7 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = cpo,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "CPO, U = 1, country = Norway"
  ) +
  labs(
    x = expression(alpha),
    y = "CPO"
  ) +
  theme(
    legend.position = "none"
  )
plot_8 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = mae,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "MAE, U = 1, country = Norway"
  ) +
  labs(
    x = expression(alpha),
    y = "MAE"
  )
plot_5 + plot_6
plot_7 + plot_8
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
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.1, 0.01)
  )
)
prior_2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5, 0.01)
  )
)
prior_3 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
prior_4 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1.5, 0.01)
  )
)
prior_5 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(2, 0.01)
  )
)
prior_6 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(2.5, 0.01)
  )
)
prior_7 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(3, 0.01)
  )
)
prior_8 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(5, 0.01)
  )
)
prior_9 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(10, 0.01)
  )
)
prior_10 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(50, 0.01)
  )
)
prior_11 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.025)
  )
)
prior_12 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.05)
  )
)
prior_13 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.1)
  )
)
prior_14 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.25)
  )
)
prior_15 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.5)
  )
)
prior_16 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.75)
  )
)
prior_17 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.99)
  )
)
prior_18 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.001)
  )
)
formula_besag_1 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1)
formula_bym2_1 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_leroux_1 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_besag_2 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2)
formula_bym2_2 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_leroux_2 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_besag_3 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_3)
formula_bym2_3 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_3)
formula_leroux_3 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_3)
formula_besag_4 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_4)
formula_bym2_4 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_4)
formula_leroux_4 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_4)
formula_besag_5 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_5)
formula_bym2_5 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_5)
formula_leroux_5 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_5)
formula_besag_6 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_6)
formula_bym2_6 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_6)
formula_leroux_6 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_6)
formula_besag_7 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_7)
formula_bym2_7 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_7)
formula_leroux_7 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_7)
formula_besag_8 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_8)
formula_bym2_8 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_8)
formula_leroux_8 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_8)
formula_besag_9 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_9)
formula_bym2_9 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_9)
formula_leroux_9 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_9)
formula_besag_10 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_10)
formula_bym2_10 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_10)
formula_leroux_10 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_10)
formula_besag_11 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_11)
formula_bym2_11 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_11)
formula_leroux_11 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_11)
formula_besag_12 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_12)
formula_bym2_12 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_12)
formula_leroux_12 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_12)
formula_besag_13 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_13)
formula_bym2_13 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_13)
formula_leroux_13 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_13)
formula_besag_14 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_14)
formula_bym2_14 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_14)
formula_leroux_14 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_14)
formula_besag_15 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_15)
formula_bym2_15 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_15)
formula_leroux_15 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_15)
formula_besag_16 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_16)
formula_bym2_16 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_16)
formula_leroux_16 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_16)
formula_besag_17 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_17)
formula_bym2_17 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_17)
formula_leroux_17 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_17)
formula_besag_18 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_18)
formula_bym2_18 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_18)
formula_leroux_18 <- value ~
Union + SPD + Gruene + FDP + die_linke + afd +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_18)

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
res_besag_11 <- inla(
  formula_besag_11,
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
res_bym2_11 <- inla(
  formula_bym2_11,
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
res_leroux_11 <- inla(
  formula_leroux_11,
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
res_besag_12 <- inla(
  formula_besag_12,
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
res_bym2_12 <- inla(
  formula_bym2_12,
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
res_leroux_12 <- inla(
  formula_leroux_12,
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
res_besag_13 <- inla(
  formula_besag_13,
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
res_bym2_13 <- inla(
  formula_bym2_13,
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
res_leroux_13 <- inla(
  formula_leroux_13,
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
res_besag_14 <- inla(
  formula_besag_14,
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
res_bym2_14 <- inla(
  formula_bym2_14,
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
res_leroux_14 <- inla(
  formula_leroux_14,
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
res_besag_15 <- inla(
  formula_besag_15,
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
res_bym2_15 <- inla(
  formula_bym2_15,
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
res_leroux_15 <- inla(
  formula_leroux_15,
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
res_besag_16 <- inla(
  formula_besag_16,
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
res_bym2_16 <- inla(
  formula_bym2_16,
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
res_leroux_16 <- inla(
  formula_leroux_16,
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
res_besag_17 <- inla(
  formula_besag_17,
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
res_bym2_17 <- inla(
  formula_bym2_17,
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
res_leroux_17 <- inla(
  formula_leroux_17,
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
res_besag_18 <- inla(
  formula_besag_18,
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
res_bym2_18 <- inla(
  formula_bym2_18,
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
res_leroux_18 <- inla(
  formula_leroux_18,
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
predicted_besag_11 <- c()
predicted_bym2_11 <- c()
predicted_leroux_11 <- c()
predicted_besag_12 <- c()
predicted_bym2_12 <- c()
predicted_leroux_12 <- c()
predicted_besag_13 <- c()
predicted_bym2_13 <- c()
predicted_leroux_13 <- c()
predicted_besag_14 <- c()
predicted_bym2_14 <- c()
predicted_leroux_14 <- c()
predicted_besag_15 <- c()
predicted_bym2_15 <- c()
predicted_leroux_15 <- c()
predicted_besag_16 <- c()
predicted_bym2_16 <- c()
predicted_leroux_16 <- c()
predicted_besag_17 <- c()
predicted_bym2_17 <- c()
predicted_leroux_17 <- c()
predicted_besag_18 <- c()
predicted_bym2_18 <- c()
predicted_leroux_18 <- c()
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
  predicted_besag_11[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_11$marginals.fitted.values[[i]]
  )
  predicted_bym2_11[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_11$marginals.fitted.values[[i]]
  )
  predicted_leroux_11[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_11$marginals.fitted.values[[i]]
  )
  predicted_besag_12[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_12$marginals.fitted.values[[i]]
  )
  predicted_bym2_12[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_12$marginals.fitted.values[[i]]
  )
  predicted_leroux_12[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_12$marginals.fitted.values[[i]]
  )
  predicted_besag_13[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_13$marginals.fitted.values[[i]]
  )
  predicted_bym2_13[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_13$marginals.fitted.values[[i]]
  )
  predicted_leroux_13[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_13$marginals.fitted.values[[i]]
  )
  predicted_besag_14[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_14$marginals.fitted.values[[i]]
  )
  predicted_bym2_14[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_14$marginals.fitted.values[[i]]
  )
  predicted_leroux_14[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_14$marginals.fitted.values[[i]]
  )
  predicted_besag_15[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_15$marginals.fitted.values[[i]]
  )
  predicted_bym2_15[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_15$marginals.fitted.values[[i]]
  )
  predicted_leroux_15[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_15$marginals.fitted.values[[i]]
  )
  predicted_besag_16[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_16$marginals.fitted.values[[i]]
  )
  predicted_bym2_16[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_16$marginals.fitted.values[[i]]
  )
  predicted_leroux_16[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_16$marginals.fitted.values[[i]]
  )
  predicted_besag_17[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_17$marginals.fitted.values[[i]]
  )
  predicted_bym2_17[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_17$marginals.fitted.values[[i]]
  )
  predicted_leroux_17[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_17$marginals.fitted.values[[i]]
  )
  predicted_besag_18[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_besag_18$marginals.fitted.values[[i]]
  )
  predicted_bym2_18[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_bym2_18$marginals.fitted.values[[i]]
  )
  predicted_leroux_18[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_leroux_18$marginals.fitted.values[[i]]
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
  mean(abs(predicted_leroux_10[test] - test_value)),
  mean(abs(predicted_besag_11[test] - test_value)),
  mean(abs(predicted_bym2_11[test] - test_value)),
  mean(abs(predicted_leroux_11[test] - test_value)),
  mean(abs(predicted_besag_12[test] - test_value)),
  mean(abs(predicted_bym2_12[test] - test_value)),
  mean(abs(predicted_leroux_12[test] - test_value)),
  mean(abs(predicted_besag_13[test] - test_value)),
  mean(abs(predicted_bym2_13[test] - test_value)),
  mean(abs(predicted_leroux_13[test] - test_value)),
  mean(abs(predicted_besag_14[test] - test_value)),
  mean(abs(predicted_bym2_14[test] - test_value)),
  mean(abs(predicted_leroux_14[test] - test_value)),
  mean(abs(predicted_besag_15[test] - test_value)),
  mean(abs(predicted_bym2_15[test] - test_value)),
  mean(abs(predicted_leroux_15[test] - test_value)),
  mean(abs(predicted_besag_16[test] - test_value)),
  mean(abs(predicted_bym2_16[test] - test_value)),
  mean(abs(predicted_leroux_16[test] - test_value)),
  mean(abs(predicted_besag_17[test] - test_value)),
  mean(abs(predicted_bym2_17[test] - test_value)),
  mean(abs(predicted_leroux_17[test] - test_value)),
  mean(abs(predicted_besag_18[test] - test_value)),
  mean(abs(predicted_bym2_18[test] - test_value)),
  mean(abs(predicted_leroux_18[test] - test_value))
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
  res_leroux_10$dic$dic,
  res_besag_11$dic$dic,
  res_bym2_11$dic$dic,
  res_leroux_11$dic$dic,
  res_besag_12$dic$dic,
  res_bym2_12$dic$dic,
  res_leroux_12$dic$dic,
  res_besag_13$dic$dic,
  res_bym2_13$dic$dic,
  res_leroux_13$dic$dic,
  res_besag_14$dic$dic,
  res_bym2_14$dic$dic,
  res_leroux_14$dic$dic,
  res_besag_15$dic$dic,
  res_bym2_15$dic$dic,
  res_leroux_15$dic$dic,
  res_besag_16$dic$dic,
  res_bym2_16$dic$dic,
  res_leroux_16$dic$dic,
  res_besag_17$dic$dic,
  res_bym2_17$dic$dic,
  res_leroux_17$dic$dic,
  res_besag_18$dic$dic,
  res_bym2_18$dic$dic,
  res_leroux_18$dic$dic
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
  res_leroux_10$waic$waic,
  res_besag_11$waic$waic,
  res_bym2_11$waic$waic,
  res_leroux_11$waic$waic,
  res_besag_12$waic$waic,
  res_bym2_12$waic$waic,
  res_leroux_12$waic$waic,
  res_besag_13$waic$waic,
  res_bym2_13$waic$waic,
  res_leroux_13$waic$waic,
  res_besag_14$waic$waic,
  res_bym2_14$waic$waic,
  res_leroux_14$waic$waic,
  res_besag_15$waic$waic,
  res_bym2_15$waic$waic,
  res_leroux_15$waic$waic,
  res_besag_16$waic$waic,
  res_bym2_16$waic$waic,
  res_leroux_16$waic$waic,
  res_besag_17$waic$waic,
  res_bym2_17$waic$waic,
  res_leroux_17$waic$waic,
  res_besag_18$waic$waic,
  res_bym2_18$waic$waic,
  res_leroux_18$waic$waic
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
  sum(log(res_leroux_10$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_11$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_11$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_11$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_12$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_12$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_12$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_13$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_13$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_13$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_14$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_14$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_14$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_15$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_15$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_15$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_16$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_16$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_16$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_17$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_17$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_17$cpo$cpo), na.rm = TRUE),
  sum(log(res_besag_18$cpo$cpo), na.rm = TRUE),
  sum(log(res_bym2_18$cpo$cpo), na.rm = TRUE),
  sum(log(res_leroux_18$cpo$cpo), na.rm = TRUE)
))

results <- tibble(
  dic = unlist(dic),
  waic = unlist(waic),
  cpo = unlist(cpo),
  mae = unlist(mae),
  model = rep(c("Besag", "Bym2", "Leroux"), 18),
  U = c(
    rep(0.1, 3),
    rep(0.5, 3),
    rep(1, 3),
    rep(1.5, 3),
    rep(2, 3),
    rep(2.5, 3),
    rep(3, 3),
    rep(5, 3),
    rep(10, 3),
    rep(50, 3),
    rep(1, 24)
  ),
  alpha = c(
    rep(0.01, 30),
    rep(0.025, 3),
    rep(0.05, 3),
    rep(0.1, 3),
    rep(0.25, 3),
    rep(0.5, 3),
    rep(0.75, 3),
    rep(0.99, 3),
    rep(0.001, 3)
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
plot_5 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = dic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "DIC, U = 1, country = Germany"
  ) +
  labs(
    x = expression(alpha),
    y = "DIC"
  ) +
  theme(
    legend.position = "none"
  )
plot_6 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = waic,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "WAIC, U = 1, country = Germany"
  ) +
  labs(
    x = expression(alpha),
    y = "WAIC"
  )
plot_7 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = cpo,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "CPO, U = 1, country = Germany"
  ) +
  labs(
    x = expression(alpha),
    y = "CPO"
  ) +
  theme(
    legend.position = "none"
  )
plot_8 <- ggplot(
  data = results[c(1:3, 31:54), ],
  aes(
    x = alpha,
    y = mae,
    colour = model
  )
) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.01) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = "MAE, U = 1, country = Germany"
  ) +
  labs(
    x = expression(alpha),
    y = "MAE"
  )
plot_5 + plot_6
plot_7 + plot_8
