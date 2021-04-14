library(INLA)
library(spdep)
source("R/preprocess_norge_temporal.R")
set.seed(7918)
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
nb <- poly2nb(norge[!duplicated(norge$kommune_no), ])
# save the matrix
nb2INLA("maps/map_3.adj", nb)
g <- inla.read.graph(filename = "maps/map_3.adj")
# specify the model formula
# we will start with demographic variables and pop/urban density
formula_1 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_2 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")

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
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
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
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

predicted_1 <- c()
predicted_2 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_1$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_2$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
if (mae_1 < mae_2) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_1))
  results <- c(
    results,
    list(
      dic = res_1$dic$dic,
      waic = res_1$waic$waic,
      cpo = sum(log(res_1$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_2))
  results <- c(
    results,
    list(
      dic = res_2$dic$dic,
      waic = res_2$waic$waic,
      cpo = sum(log(res_2$cpo$cpo), na.rm = TRUE)
    )
  )
}
rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)

# now models with the mobility variables
formula_3 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_4 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + +unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
# now models with the mobility variables
formula_5 <- value ~
# add the demographic vars and pop density
unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_6 <- value ~
# add the demographic vars and pop density
unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")


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
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_3$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_4$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_5$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_6$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_3))
  results <- c(
    results,
    list(
      dic = res_3$dic$dic,
      waic = res_3$waic$waic,
      cpo = sum(log(res_3$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_4))
  results <- c(
    results,
    list(
      dic = res_4$dic$dic,
      waic = res_4$waic$waic,
      cpo = sum(log(res_4$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_5))
  results <- c(
    results,
    list(
      dic = res_5$dic$dic,
      waic = res_5$waic$waic,
      cpo = sum(log(res_5$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_6))
  results <- c(
    results,
    list(
      dic = res_6$dic$dic,
      waic = res_6$waic$waic,
      cpo = sum(log(res_6$cpo$cpo), na.rm = TRUE)
    )
  )
}

rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with the infrastructure variables
formula_7 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_8 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + sex + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_9 <- value ~
immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_10 <- value ~
immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")

res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_7$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_8$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_9$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_10$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_7))
  results <- c(
    results,
    list(
      dic = res_7$dic$dic,
      waic = res_7$waic$waic,
      cpo = sum(log(res_7$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_8))
  results <- c(
    results,
    list(
      dic = res_8$dic$dic,
      waic = res_8$waic$waic,
      cpo = sum(log(res_8$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_9))
  results <- c(
    results,
    list(
      dic = res_9$dic$dic,
      waic = res_9$waic$waic,
      cpo = sum(log(res_9$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_10))
  results <- c(
    results,
    list(
      dic = res_10$dic$dic,
      waic = res_10$waic$waic,
      cpo = sum(log(res_10$cpo$cpo), na.rm = TRUE)
    )
  )
}
rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with all the variables
formula_11 <- value ~
pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_12 <- value ~
pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_13 <- value ~
workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_14 <- value ~
workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_14 <- inla(
  formula_14,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_11$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_12$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_13$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_14$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_11))
  results <- c(
    results,
    list(
      dic = res_11$dic$dic,
      waic = res_11$waic$waic,
      cpo = sum(log(res_11$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_12))
  results <- c(
    results,
    list(
      dic = res_12$dic$dic,
      waic = res_12$waic$waic,
      cpo = sum(log(res_12$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_13))
  results <- c(
    results,
    list(
      dic = res_13$dic$dic,
      waic = res_13$waic$waic,
      cpo = sum(log(res_13$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_14))
  results <- c(
    results,
    list(
      dic = res_14$dic$dic,
      waic = res_14$waic$waic,
      cpo = sum(log(res_14$cpo$cpo), na.rm = TRUE)
    )
  )
}

rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
########################################################
# Now with variable selection
formula_15 <- value ~
pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  median_age + unemp_tot + unemp_immg + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_16 <- value ~
pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  median_age + unemp_tot + unemp_immg + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_17 <- value ~
workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  median_age + unemp_tot + unemp_immg + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_18 <- value ~
workers_ft_work + workers_pt_work +
  construction_ft_work + construction_pt_work +
  median_age + unemp_tot + unemp_immg + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")

res_15 <- inla(
  formula_15,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_16 <- inla(
  formula_16,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_17 <- inla(
  formula_17,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_18 <- inla(
  formula_18,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_15$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_16$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_17$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_18$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_15))
  results <- c(
    results,
    list(
      dic = res_15$dic$dic,
      waic = res_15$waic$waic,
      cpo = sum(log(res_15$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_16))
  results <- c(
    results,
    list(
      dic = res_16$dic$dic,
      waic = res_16$waic$waic,
      cpo = sum(log(res_16$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_17))
  results <- c(
    results,
    list(
      dic = res_17$dic$dic,
      waic = res_17$waic$waic,
      cpo = sum(log(res_17$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_18))
  results <- c(
    results,
    list(
      dic = res_18$dic$dic,
      waic = res_18$waic$waic,
      cpo = sum(log(res_18$cpo$cpo), na.rm = TRUE)
    )
  )
}

rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with all the variables
formula_19 <- value ~
median_age + unemp_tot + unemp_immg + workers_ft_work +
  workers_pt_work + construction_pt_work + immigrants_total +
  pop_dens + urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_20 <- value ~
median_age + unemp_tot + unemp_immg + workers_ft_work +
  workers_pt_work + construction_pt_work + immigrants_total +
  pop_dens + urb_dens + sex +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_21 <- value ~
construction_pt_work + unemp_tot + sex +
  median_age + pop_dens +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_22 <- value ~
construction_pt_work + unemp_tot + sex +
  median_age + pop_dens +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")

res_19 <- inla(
  formula_19,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_20 <- inla(
  formula_20,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_21 <- inla(
  formula_21,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_22 <- inla(
  formula_22,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_19$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_20$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_21$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_22$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_19))
  results <- c(
    results,
    list(
      dic = res_19$dic$dic,
      waic = res_19$waic$waic,
      cpo = sum(log(res_19$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_20))
  results <- c(
    results,
    list(
      dic = res_20$dic$dic,
      waic = res_20$waic$waic,
      cpo = sum(log(res_20$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_21))
  results <- c(
    results,
    list(
      dic = res_21$dic$dic,
      waic = res_21$waic$waic,
      cpo = sum(log(res_21$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_22))
  results <- c(
    results,
    list(
      dic = res_22$dic$dic,
      waic = res_22$waic$waic,
      cpo = sum(log(res_22$cpo$cpo), na.rm = TRUE)
    )
  )
}
models <- models[which(unlist(mae) %in% min(unlist(mae)))]
results <- results[
  seq(
    1 + (which(unlist(mae) %in% min(unlist(mae))) - 1) * 3,
    (which(unlist(mae) %in% min(unlist(mae))) - 1) * 3 + 3
  )
]
mae <- mae[which(unlist(mae) %in% min(unlist(mae)))]


rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)

# now models with all the variables
formula_23 <- value ~
pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_24 <- value ~
# add the demographic vars and pop density
pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_25 <- value ~
marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_26 <- value ~
# add the demographic vars and pop density
marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  kindergarten + bakeries +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")


res_23 <- inla(
  formula_23,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_24 <- inla(
  formula_24,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_25 <- inla(
  formula_25,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_26 <- inla(
  formula_26,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_23$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_24$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_25$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_26$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_23))
  results <- c(
    results,
    list(
      dic = res_23$dic$dic,
      waic = res_23$waic$waic,
      cpo = sum(log(res_23$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_24))
  results <- c(
    results,
    list(
      dic = res_24$dic$dic,
      waic = res_24$waic$waic,
      cpo = sum(log(res_24$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_25))
  results <- c(
    results,
    list(
      dic = res_25$dic$dic,
      waic = res_25$waic$waic,
      cpo = sum(log(res_25$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_26))
  results <- c(
    results,
    list(
      dic = res_26$dic$dic,
      waic = res_26$waic$waic,
      cpo = sum(log(res_26$cpo$cpo), na.rm = TRUE)
    )
  )
}

rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)

# now models with all the variables
formula_27 <- value ~
marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + restaurant + aerodrome +
  office + platform + kindergarten + schools + bakeries + pop_dens +
  urb_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_28 <- value ~
marketplace + entertainment + sport + clinic +
  hairdresser + shops + place_of_worship + restaurant + aerodrome +
  office + platform + kindergarten + schools + bakeries + pop_dens +
  urb_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
# now models with all the variables
formula_29 <- value ~
pop_dens + shops + place_of_worship + office +
  schools + nursing_home + kindergarten + restaurant +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_30 <- value ~
# add the demographic vars and pop density
pop_dens + shops + place_of_worship + office +
  schools + nursing_home + kindergarten + restaurant +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")

res_27 <- inla(
  formula_27,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_28 <- inla(
  formula_28,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_29 <- inla(
  formula_29,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_30 <- inla(
  formula_30,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_27$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_28$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_29$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_30$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_27))
  results <- c(
    results,
    list(
      dic = res_27$dic$dic,
      waic = res_27$waic$waic,
      cpo = sum(log(res_27$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_28))
  results <- c(
    results,
    list(
      dic = res_28$dic$dic,
      waic = res_28$waic$waic,
      cpo = sum(log(res_28$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_29))
  results <- c(
    results,
    list(
      dic = res_29$dic$dic,
      waic = res_29$waic$waic,
      cpo = sum(log(res_29$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_30))
  results <- c(
    results,
    list(
      dic = res_30$dic$dic,
      waic = res_30$waic$waic,
      cpo = sum(log(res_30$cpo$cpo), na.rm = TRUE)
    )
  )
}
models <- models[c(1, which(unlist(mae[2:3]) %in% min(unlist(mae[2:3]))) + 1)]
results <- results[
  c(
    seq_len(3),
    seq(
      1 + (which(unlist(mae[2:3]) %in% min(unlist(mae[2:3]))) - 1) * 3,
      (which(unlist(mae[2:3]) %in% min(unlist(mae[2:3]))) - 1) * 3 + 3
    ) + 3
  )
]
mae <- mae[c(1, which(unlist(mae[2:3]) %in% min(unlist(mae[2:3]))) + 1)]


rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)
# now models with all the variables
formula_31 <- value ~
median_age + unemp_tot + workers_ft_work +
  workers_pt_work + construction_pt_work + immigrants_total +
  marketplace + entertainment + clinic + hairdresser + shops +
  retail + nursing_home + restaurant + aerodrome + office +
  platform + kindergarten + schools + bakeries + higher_education +
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_32 <- value ~
median_age + unemp_tot + workers_ft_work +
  workers_pt_work + construction_pt_work + immigrants_total +
  marketplace + entertainment + clinic + hairdresser + shops +
  retail + nursing_home + restaurant + aerodrome + office +
  platform + kindergarten + schools + bakeries + higher_education +
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
# now models with all the variables
formula_33 <- value ~
schools + unemp_tot + restaurant + sex +
  median_age + pop_dens + construction_pt_work + workers_ft_work +
  higher_education + clinic +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_1) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")
formula_34 <- value ~
schools + unemp_tot + restaurant + sex +
  median_age + pop_dens + construction_pt_work + workers_ft_work +
  higher_education + clinic +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besagproper", graph = g, hyper = prior_2) +
  f(id_date_1, model = "rw2") +
  f(id_date_2, model = "iid")

res_31 <- inla(
  formula_31,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_32 <- inla(
  formula_32,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_33 <- inla(
  formula_33,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_34 <- inla(
  formula_34,
  family = "nbinomial",
  data = norge,
  E = expected_count,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = norge$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
for (i in seq_len(nrow(norge))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_31$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_32$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_33$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * norge$population[i],
    res_34$marginals.fitted.values[[i]]
  )
}
mae_1 <- mean(abs(predicted_1[test] - test_value))
mae_2 <- mean(abs(predicted_2[test] - test_value))
mae_3 <- mean(abs(predicted_3[test] - test_value))
mae_4 <- mean(abs(predicted_4[test] - test_value))
maes <- c(mae_1, mae_2, mae_3, mae_4)
if (mae_1 == min(maes)) {
  mae <- c(mae, list(mae_1))
  models <- c(models, list(res_31))
  results <- c(
    results,
    list(
      dic = res_31$dic$dic,
      waic = res_31$waic$waic,
      cpo = sum(log(res_31$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_2 == min(maes)) {
  mae <- c(mae, list(mae_2))
  models <- c(models, list(res_32))
  results <- c(
    results,
    list(
      dic = res_32$dic$dic,
      waic = res_32$waic$waic,
      cpo = sum(log(res_32$cpo$cpo), na.rm = TRUE)
    )
  )
} else if (mae_3 == min(maes)) {
  mae <- c(mae, list(mae_3))
  models <- c(models, list(res_33))
  results <- c(
    results,
    list(
      dic = res_33$dic$dic,
      waic = res_33$waic$waic,
      cpo = sum(log(res_33$cpo$cpo), na.rm = TRUE)
    )
  )
} else {
  mae <- c(mae, list(mae_4))
  models <- c(models, list(res_34))
  results <- c(
    results,
    list(
      dic = res_34$dic$dic,
      waic = res_34$waic$waic,
      cpo = sum(log(res_34$cpo$cpo), na.rm = TRUE)
    )
  )
}
rm(
  list = setdiff(
    ls(),
    c(
      "norge", "prior_1", "prior_2", "g", "models", "results",
      "test", "test_value", "link", "mae"
    )
  )
)

# now models with all the variables
models_final <- list(models, results, mae)


save(models_final, file = "models/besagproper_norway_temporal.Rda")
