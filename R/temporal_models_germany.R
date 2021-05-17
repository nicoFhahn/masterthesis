library(INLA)
library(SpatialEpi)
source("R/preprocess_timeseries.R")
ts_both <- rbind(ts_germany, ts_norway)
date_tibble <- tibble(
  Date = unique(ts_both$Date),
  id_date_1 = seq_len(length(unique(ts_both$Date))),
  id_date_2 = seq_len(length(unique(ts_both$Date)))
)
ts_both <- merge(
  ts_both,
  date_tibble,
  by = "Date"
)
population_germany <- sum(read_csv("wrangled_data/germany_features.csv")$PopulationTotal[!duplicated(read_csv("wrangled_data/germany_features.csv")$PopulationTotal)])
population_norway <- sum(read_csv("wrangled_data/norge_features.csv")$population_total[!duplicated(read_csv("wrangled_data/norge_features.csv")$population_total)])
ts_both$population <- population_norway
ts_both[ts_both$Country == "Germany", ]$population <- population_germany
expected <- expected(
  ts_both$population,
  ts_both$new_cases,
  n.strata = 1
)
ts_both$e <- expected
test <- c(950:999)
test_value <- ts_both$new_cases[test]
ts_both$new_cases[test] <- NA
link <- rep(NA, nrow(ts_both))
link[which(is.na(ts_both$new_cases))] <- 1
#####################################################
# specify penalized prior
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
uniques <- sapply(ts_both, function(x) length(unique(x)))
ts_both[, names(uniques[uniques == 1])[3:(length(uniques[uniques == 1]) - 1)]] <- NULL
formula_1 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_both)[6:28], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)"
  )
)
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_both,
  E = e,
  control.predictor = list(
    compute = TRUE,
    link = 1
  ),
  Ntrials = ts_both$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_2 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_both)[6:28], collapse = " + "),
    "+ f(id_date_1, model = 'rw1', hyper = prior_1)"
  )
)
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_both,
  E = e,
  control.predictor = list(
    compute = TRUE,
    link = 1
  ),
  Ntrials = ts_both$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

formula_3 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_both)[6:28], collapse = " + "),
    "+ f(id_date_1, model = 'iid')"
  )
)
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_both,
  E = e,
  control.predictor = list(
    compute = TRUE,
    link = 1
  ),
  Ntrials = ts_both$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_both)[6:28], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid')"
  )
)
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_both,
  E = e,
  control.predictor = list(
    compute = TRUE,
    link = 1
  ),
  Ntrials = ts_both$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_both)[6:28], collapse = " + "),
    "+ f(id_date_1, model = 'ar1', hyper = prior_1)"
  )
)
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_both,
  E = e,
  control.predictor = list(
    compute = TRUE,
    link = 1
  ),
  Ntrials = ts_both$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
models <- list()
gof <- list()
mae <- list()
models <- c(models, list(res_1, res_2, res_3, res_4, res_5))
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
  ),
  list(
    dic = res_5$dic$dic,
    waic = res_5$waic$waic,
    cpo = sum(log(res_5$cpo$cpo), na.rm = TRUE)
  )
))
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
predicted_5 <- c()
# make predictions
for (i in seq_len(nrow(ts_both))) {
  predicted_1[i] <- inla.emarginal(
    function(x) x * ts_both$population[i],
    res_1$marginals.fitted.values[[i]]
  )
  predicted_2[i] <- inla.emarginal(
    function(x) x * ts_both$population[i],
    res_2$marginals.fitted.values[[i]]
  )
  predicted_3[i] <- inla.emarginal(
    function(x) x * ts_both$population[i],
    res_3$marginals.fitted.values[[i]]
  )
  predicted_4[i] <- inla.emarginal(
    function(x) x * ts_both$population[i],
    res_4$marginals.fitted.values[[i]]
  )
  predicted_5[i] <- inla.emarginal(
    function(x) x * ts_both$population[i],
    res_5$marginals.fitted.values[[i]]
  )
}
# calculate the mae
mae <- c(mae, list(
  mean(abs(predicted_1[test] - test_value)),
  mean(abs(predicted_2[test] - test_value)),
  mean(abs(predicted_3[test] - test_value)),
  mean(abs(predicted_4[test] - test_value)),
  mean(abs(predicted_5[test] - test_value))
))
models_final <- list(models, gof, mae)
# save the models
save(models_final, file = "models/nontemporal_germany.RDa")
