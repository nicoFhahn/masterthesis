library(INLA)
library(spdep)
source("R/preprocess_germany.R")
set.seed(145)
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
results <- list()
mae <- list()
#
# create the neighbordhood matrix
nb <- poly2nb(newest_numbers)
# save the matrix
nb2INLA("maps/map_2.adj", nb)
g <- inla.read.graph(filename = "maps/map_2.adj")
# specify the model formula
# we will start with demographic variables and pop/urban density
formula_1 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)

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
models <- c(models, list(res_1))
perf <- list(
  dic = c(
    res_1$dic$dic
  ),
  waic = c(
    res_1$waic$waic
  ),
  cpo = c(
    sum(log(res_1$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_1 = perf))
predicted_1 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_1$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value))
))


rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
) # now models with the mobility variables
formula_2 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + asyl_benefits + protection_seekers +
  welfare_recipients + unemployed_total + unemployed_foreigners +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
# now models with the mobility variables
formula_3 <- value ~
# add the demographic vars and pop density
asyl_benefits + protection_seekers +
  welfare_recipients + unemployed_total + unemployed_foreigners +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)


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

models <- c(models, list(res_2, res_3))

perf <- list(
  dic = c(
    res_2$dic$dic, res_3$dic$dic
  ),
  waic = c(
    res_2$waic$waic, res_3$waic$waic
  ),
  cpo = c(
    sum(log(res_2$cpo$cpo), na.rm = TRUE),
    sum(log(res_3$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_2 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_2$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_3$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))

rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
) # now models with the infrastructure variables
formula_4 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + log(trade_tax) +
  log(income_total) + log(income_tax) +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_5 <- value ~
# add the demographic vars and pop density
log(trade_tax) + log(income_total) + log(income_tax) +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)

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

models <- c(models, list(res_4, res_5))

perf <- list(
  dic = c(
    res_4$dic$dic, res_5$dic$dic
  ),
  waic = c(
    res_4$waic$waic, res_5$waic$waic
  ),
  cpo = c(
    sum(log(res_4$cpo$cpo), na.rm = TRUE),
    sum(log(res_5$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_3 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_4$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_5$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))

rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
) # now models with all the variables
formula_6 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + Union + SPD + Gruene + FDP +
  die_linke + afd +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_7 <- value ~
# add the demographic vars and pop density
Union + SPD + Gruene + FDP +
  die_linke + afd +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
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

models <- c(models, list(res_6, res_7))
perf <- list(
  dic = c(
    res_6$dic$dic, res_7$dic$dic
  ),
  waic = c(
    res_6$waic$waic, res_7$waic$waic
  ),
  cpo = c(
    sum(log(res_6$cpo$cpo), na.rm = TRUE),
    sum(log(res_7$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_4 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_6$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_7$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))

rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
) ########################################################
# Now with variable selection
formula_8 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + asyl_benefits + log(trade_tax) +
  log(income_total) + log(income_tax) + Union + SPD + Gruene + FDP +
  die_linke + afd + protection_seekers + welfare_recipients +
  unemployed_total + unemployed_foreigners +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_9 <- value ~
# add the demographic vars and pop density
asyl_benefits + log(trade_tax) +
  log(income_total) + log(income_tax) + Union + SPD + Gruene + FDP +
  die_linke + afd + protection_seekers + welfare_recipients +
  unemployed_total + unemployed_foreigners +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)

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

models <- c(models, list(res_8, res_9))

perf <- list(
  dic = c(
    res_8$dic$dic, res_9$dic$dic
  ),
  waic = c(
    res_8$waic$waic, res_9$waic$waic
  ),
  cpo = c(
    sum(log(res_8$cpo$cpo), na.rm = TRUE),
    sum(log(res_9$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_5 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_8$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_9$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))

rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
) # now models with all the variables
formula_10 <- value ~
log(trade_tax) + log(income_total) + log(income_tax) + Union +
  SPD + Gruene + FDP + die_linke + afd + protection_seekers +
  welfare_recipients + unemployed_total + unemployed_foreigners +
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)

formula_11 <- value ~
log(income_total) + log(income_tax) + afd + die_linke + pop_dens +
  unemployed_total + log(trade_tax) + SPD + FDP + protection_seekers +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)


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

models <- c(models, list(res_10, res_11))

perf <- list(
  dic = c(
    res_10$dic$dic, res_11$dic$dic
  ),
  waic = c(
    res_10$waic$waic, res_11$waic$waic
  ),
  cpo = c(
    sum(log(res_10$cpo$cpo), na.rm = TRUE),
    sum(log(res_11$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_6 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_10$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_11$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))


rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with all the variables
formula_12 <- value ~
pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_13 <- value ~
marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)


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

models <- c(models, list(res_12, res_13))

perf <- list(
  dic = c(
    res_12$dic$dic, res_13$dic$dic
  ),
  waic = c(
    res_12$waic$waic, res_13$waic$waic
  ),
  cpo = c(
    sum(log(res_12$cpo$cpo), na.rm = TRUE),
    sum(log(res_13$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_7 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_12$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_13$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))


rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with all the variables
formula_14 <- value ~
# add the demographic vars and pop density
entertainment + sport + hairdresser + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + platform +
  kindergarten + schools + bakeries + pop_dens + higher_education +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
# now models with all the variables
formula_15 <- value ~
schools + place_of_worship + pop_dens + office +
  bakeries + entertainment + platform + kindergarten + nursing_home +
  sport +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)

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

models <- c(models, list(res_14, res_15))

perf <- list(
  dic = c(
    res_14$dic$dic, res_15$dic$dic
  ),
  waic = c(
    res_14$waic$waic, res_15$waic$waic
  ),
  cpo = c(
    sum(log(res_14$cpo$cpo), na.rm = TRUE),
    sum(log(res_15$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_8 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_14$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_15$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))


rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
formula_16 <- value ~
log(trade_tax) + log(income_total) + log(income_tax) + SPD +
  Gruene + FDP + die_linke + afd + protection_seekers + welfare_recipients +
  unemployed_total + unemployed_foreigners + entertainment +
  sport + clinic + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + kindergarten +
  schools + bakeries + pop_dens + sex + higher_education +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_17 <- value ~
schools + afd + die_linke + pop_dens + place_of_worship +
  entertainment + bakeries + SPD + platform + sport + nursing_home +
  welfare_recipients + FDP + kindergarten + log(trade_tax) + office +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)

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

models <- c(models, list(res_16, res_17))

perf <- list(
  dic = c(
    res_16$dic$dic, res_17$dic$dic
  ),
  waic = c(
    res_16$waic$waic, res_17$waic$waic
  ),
  cpo = c(
    sum(log(res_16$cpo$cpo), na.rm = TRUE),
    sum(log(res_17$cpo$cpo), na.rm = TRUE)
  )
)
results <- c(results, list(res_9 = perf))
predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(newest_numbers))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_16$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * newest_numbers$population[i],
    res_17$marginals.fitted.values[[i]]
  )
}
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value))
))


rm(
  list = setdiff(
    ls(),
    c(
      "newest_numbers", "prior_1", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with all the variables
models_final <- list(models, results, mae)
save(models_final, file = "models/bym2_germany.Rda")
