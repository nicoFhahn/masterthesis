library(INLA)
library(spdep)
source("R/preprocess_norge.R")
set.seed(420)
test <- sample(seq_len(nrow(newest_numbers)), size = floor(0.2 * nrow(newest_numbers)))
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
prior_2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)
  )
) #
models <- list()
results <- list()
mae <- list()
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
# specify the model formula
# we will start with demographic variables and pop/urban density
formula_1 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_2 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)

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

models <- c(models, list(res_1, res_2))
perf <- list(
  dic = c(
    res_1$dic$dic, res_2$dic$dic
  ),
  waic = c(
    res_1$waic$waic, res_2$waic$waic
  ),
  cpo = c(
    sum(log(res_1$cpo$cpo), na.rm = TRUE), sum(log(res_2$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_1 = perf))
predicted_1 <- c()
predicted_2 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_1$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_2$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test))
))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))
# now models with the mobility variables
formula_3 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_4 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex +  + unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
# now models with the mobility variables
formula_5 <- value ~
  # add the demographic vars and pop density
  unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_6 <- value ~
  # add the demographic vars and pop density
  unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)


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

res_5 <- inla(
  formula_5,
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


res_6 <- inla(
  formula_6,
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


models <- c(models, list(res_3, res_4, res_5, res_6))

perf <- list(
  dic = c(
    res_3$dic$dic, res_4$dic$dic,
    res_5$dic$dic, res_6$dic$dic
  ),
  waic = c(
    res_3$waic$waic, res_4$waic$waic,
    res_5$waic$waic, res_6$waic$waic
  ),
  cpo = c(
    sum(log(res_3$cpo$cpo), na.rm = TRUE), sum(log(res_4$cpo$cpo), na.rm = TRUE),
    sum(log(res_5$cpo$cpo), na.rm = TRUE), sum(log(res_6$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_2 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_3$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_4$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_5$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_6$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))
# now models with the infrastructure variables
formula_7 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_8 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_9 <- value ~
  immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_10 <- value ~
  immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)

res_7 <- inla(
  formula_7,
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

res_8 <- inla(
  formula_8,
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


res_9 <- inla(
  formula_9,
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


res_10 <- inla(
  formula_10,
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

models <- c(models, list(res_7, res_8, res_9, res_10))

perf <- list(
  dic = c(
    res_7$dic$dic, res_8$dic$dic,
    res_9$dic$dic, res_10$dic$dic
  ),
  waic = c(
    res_7$waic$waic, res_8$waic$waic,
    res_9$waic$waic, res_10$waic$waic
  ),
  cpo = c(
    sum(log(res_7$cpo$cpo), na.rm = TRUE), sum(log(res_8$cpo$cpo), na.rm = TRUE),
    sum(log(res_9$cpo$cpo), na.rm = TRUE), sum(log(res_10$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_3 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_7$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_8$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_9$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_10$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))
# now models with all the variables
formula_11 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft + workers_pt +
  construction_ft + construction_pt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_12 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_13 <- value ~
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_14 <- value ~
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
res_11 <- inla(
  formula_11,
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

res_12 <- inla(
  formula_12,
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

res_13 <- inla(
  formula_13,
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

res_14 <- inla(
  formula_14,
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

models <- c(models, list(res_11, res_12, res_13, res_14))
perf <- list(
  dic = c(
    res_11$dic$dic, res_12$dic$dic,
    res_13$dic$dic, res_14$dic$dic
  ),
  waic = c(
    res_11$waic$waic, res_12$waic$waic,
    res_13$waic$waic, res_14$waic$waic
  ),
  cpo = c(
    sum(log(res_11$cpo$cpo), na.rm = TRUE), sum(log(res_12$cpo$cpo), na.rm = TRUE),
    sum(log(res_13$cpo$cpo), na.rm = TRUE), sum(log(res_14$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_4 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_11$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_12$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_13$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_14$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))
########################################################
# Now with variable selection
formula_15 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  median_age + unemp_tot + unemp_immg + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_16 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  median_age + unemp_tot + unemp_immg + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_17 <- value ~
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  median_age + unemp_tot + unemp_immg + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_18 <- value ~
  workers_ft + workers_pt + 
  construction_ft + construction_pt +
  median_age + unemp_tot + unemp_immg + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)

res_15 <- inla(
  formula_15,
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

res_16 <- inla(
  formula_16,
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

res_17 <- inla(
  formula_17,
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

res_18 <- inla(
  formula_18,
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

models <- c(models, list(res_15, res_16, res_17, res_18))

perf <- list(
  dic = c(
    res_15$dic$dic, res_16$dic$dic,
    res_17$dic$dic, res_18$dic$dic
  ),
  waic = c(
    res_15$waic$waic, res_16$waic$waic,
    res_17$waic$waic, res_18$waic$waic
  ),
  cpo = c(
    sum(log(res_15$cpo$cpo), na.rm = TRUE), sum(log(res_16$cpo$cpo), na.rm = TRUE),
    sum(log(res_17$cpo$cpo), na.rm = TRUE), sum(log(res_18$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_5 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_15$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_16$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_17$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_18$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))
# now models with all the variables
formula_19 <- value ~
  median_age + unemp_tot + unemp_immg + workers_ft + 
  construction_ft + construction_pt + immigrants_total + pop_dens + 
  sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_20 <- value ~
  median_age + unemp_tot + unemp_immg + workers_ft + 
  construction_ft + construction_pt + immigrants_total + pop_dens + 
  sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_21 <- value ~
  pop_dens + median_age + sex + unemp_tot + 
  workers_ft + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_22 <- value ~
  pop_dens + median_age + sex + unemp_tot + 
  workers_ft + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)

res_19 <- inla(
  formula_19,
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

res_20 <- inla(
  formula_20,
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

res_21 <- inla(
  formula_21,
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

res_22 <- inla(
  formula_22,
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


models <- c(models, list(res_19, res_20, res_21, res_22))

perf <- list(
  dic = c(
    res_19$dic$dic, res_20$dic$dic,
    res_21$dic$dic, res_22$dic$dic
  ),
  waic = c(
    res_19$waic$waic, res_20$waic$waic,
    res_21$waic$waic, res_22$waic$waic
  ),
  cpo = c(
    sum(log(res_19$cpo$cpo), na.rm = TRUE), sum(log(res_20$cpo$cpo), na.rm = TRUE),
    sum(log(res_21$cpo$cpo), na.rm = TRUE), sum(log(res_22$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_6 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_19$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_20$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_21$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_22$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))

# now models with all the variables
formula_23 <- value ~
  pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_24 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_25 <- value ~
  marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_26 <- value ~
  # add the demographic vars and pop density
  marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)


res_23 <- inla(
  formula_23,
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

res_24 <- inla(
  formula_24,
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

res_25 <- inla(
  formula_25,
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


res_26 <- inla(
  formula_26,
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


models <- c(models, list(res_23, res_24, res_25, res_26))

perf <- list(
  dic = c(
    res_23$dic$dic, res_24$dic$dic,
    res_25$dic$dic, res_26$dic$dic
  ),
  waic = c(
    res_23$waic$waic, res_24$waic$waic,
    res_25$waic$waic, res_26$waic$waic
  ),
  cpo = c(
    sum(log(res_23$cpo$cpo), na.rm = TRUE), sum(log(res_24$cpo$cpo), na.rm = TRUE),
    sum(log(res_25$cpo$cpo), na.rm = TRUE), sum(log(res_26$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_7 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_23$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_24$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_25$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_26$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))


rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))

# now models with all the variables
formula_27 <- value ~
  marketplace + clinic + hairdresser + place_of_worship + 
  retail + nursing_home + restaurant + aerodrome + office + 
  platform + kindergarten + schools + higher_education + pop_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_28 <- value ~
  marketplace + clinic + hairdresser + place_of_worship + 
  retail + nursing_home + restaurant + aerodrome + office + 
  platform + kindergarten + schools + higher_education + pop_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
# now models with all the variables
formula_29 <- value ~
  pop_dens + shops + place_of_worship + office +
  schools + nursing_home + kindergarten + restaurant +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_30 <- value ~
  # add the demographic vars and pop density
  pop_dens + shops + place_of_worship + office +
  schools + nursing_home + kindergarten + restaurant +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)

res_27 <- inla(
  formula_27,
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

res_28 <- inla(
  formula_28,
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

res_29 <- inla(
  formula_29,
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


res_30 <- inla(
  formula_30,
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



models <- c(models, list(res_27, res_28, res_29, res_30))

perf <- list(
  dic = c(
    res_27$dic$dic, res_28$dic$dic,
    res_29$dic$dic, res_30$dic$dic
  ),
  waic = c(
    res_27$waic$waic, res_28$waic$waic,
    res_29$waic$waic, res_30$waic$waic
  ),
  cpo = c(
    sum(log(res_27$cpo$cpo), na.rm = TRUE), sum(log(res_28$cpo$cpo), na.rm = TRUE),
    sum(log(res_29$cpo$cpo), na.rm = TRUE), sum(log(res_30$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_8 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_27$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_28$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_29$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_30$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "C", "models", "results", "test", "test_value", "link", "mae")))
# now models with all the variables
formula_31 <- value ~
  median_age + unemp_tot + unemp_immg + workers_pt + 
  construction_ft + construction_pt + immigrants_total + marketplace + 
  entertainment + sport + clinic + shops + retail + nursing_home + 
  restaurant + aerodrome + office + platform + kindergarten + 
  schools + bakeries + higher_education + pop_dens + urb_dens + 
  sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_32 <- value ~
  median_age + unemp_tot + unemp_immg + workers_pt + 
  construction_ft + construction_pt + immigrants_total + marketplace + 
  entertainment + sport + clinic + shops + retail + nursing_home + 
  restaurant + aerodrome + office + platform + kindergarten + 
  schools + bakeries + higher_education + pop_dens + urb_dens + 
  sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
# now models with all the variables
formula_33 <- value ~
  schools + unemp_tot + sex + median_age + 
  construction_ft + pop_dens + hairdresser +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_34 <- value ~
  schools + unemp_tot + sex + median_age + 
  construction_ft + pop_dens + hairdresser +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)

res_31 <- inla(
  formula_31,
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

res_32 <- inla(
  formula_32,
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

res_33 <- inla(
  formula_33,
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


res_34 <- inla(
  formula_34,
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



models <- c(models, list(res_31, res_32, res_33, res_34))

perf <- list(
  dic = c(
    res_31$dic$dic, res_32$dic$dic,
    res_33$dic$dic, res_34$dic$dic
  ),
  waic = c(
    res_31$waic$waic, res_32$waic$waic,
    res_33$waic$waic, res_34$waic$waic
  ),
  cpo = c(
    sum(log(res_31$cpo$cpo), na.rm = TRUE), sum(log(res_32$cpo$cpo), na.rm = TRUE),
    sum(log(res_33$cpo$cpo), na.rm = TRUE), sum(log(res_34$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_9 = perf))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for(i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_31$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_32$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_33$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_34$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test)),
  mean(abs(predicted_2[test] - test)),
  mean(abs(predicted_3[test] - test)),
  mean(abs(predicted_4[test] - test))
))

# now models with all the variables
models_final <- list(models, results, mae)
save(models_final, file = "models/leroux_norway.Rda")

# results_frame <- newest_numbers
# dics[is.nan(dics)] <- 100000
# if (dics[1] == min(dics)) {
#   sfv <- res_1$summary.fitted.values
# } else if (dics[2] == min(dics)) {
#   sfv <- res_2$summary.fitted.values
# } else if (dics[3] == min(dics)) {
#   sfv <- res_3$summary.fitted.values
# } else if (dics[4] == min(dics)) {
#   sfv <- res_4$summary.fitted.values
# }
# results_frame$rr <- sfv$mean
# results_frame$q025 <- sfv$`0.025quant`
# results_frame$q5 <- sfv$`0.5quant`
# results_frame$q975 <- sfv$`0.975quant`
# rc1 <- colorRampPalette(
#   c(
#     "#86e7b8",
#     "#93ff96",
#     "#b2ffa8",
#     "#d0ffb7",
#     "#f2f5de",
#     "white"
#   ),
#   space = "Lab"
# )(10)
# rc2 <- colorRampPalette(
#   c(
#     "white",
#     "#fae0e4",
#     "#f7cad0",
#     "#f9bec7",
#     "#fbb1bd",
#     "#ff99ac",
#     "#ff85a1",
#     "#ff7096",
#     "#ff5c8a",
#     "#ff477e",
#     "#ff0a54"
#   ),
#   space = "Lab"
# )(round(10 * range(results_frame$rr)[2] - 10))
# pal <- colorNumeric(
#   c(rc1, rc2),
#   domain = results_frame$rr
# )
# map <- leaflet(results_frame) %>%
#   addMapboxGL(
#     style = "mapbox://styles/mapbox/streets-v9",
#     accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
#   ) %>%
#   addPolygons(
#     weight = 1,
#     fillColor = ~ pal(rr),
#     fillOpacity = 0.7,
#     color = "black",
#     group = "Relative risk",
#     label = paste(
#       "Kommune: ", results_frame$Landkreis, "<br>",
#       "Population: ", results_frame$PopulationTotal, "<br>",
#       "Population density: ", round(results_frame$pop_dens), "<br>",
#       "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
#       "Proportion of females: ", round(results_frame$sex, 3), "<br>",
#       "Number of infections: ", results_frame$value, "<br>",
#       "Expected number of infections: ", round(results_frame$expected), "<br>",
#       "SIR: ", round(results_frame$sir, 3), "<br>",
#       "Relative risk: ", round(results_frame$rr, 3)
#     ) %>%
#       lapply(htmltools::HTML)
#   ) %>%
#   addLegend(
#     data = results_frame,
#     pal = pal,
#     values = ~rr,
#     title = htmltools::HTML(
#       paste(
#         "RR<br><span style='font-size:0.8em'>DIC:",
#         round(min(dics)),
#         "</span>"
#       )
#     ),
#     group = "RR"
#   )
