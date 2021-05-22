library(INLA)
library(SpatialEpi)
library(MASS)
library(regclass)
ts_norway <- read_csv("wrangled_data/ts_norway.csv")
test <- seq(458, 471)
test_value <- ts_norway$new_cases[test]
ts_norway$new_cases[test] <- NA
link <- rep(NA, nrow(ts_norway))
link[which(is.na(ts_norway$new_cases))] <- 1
ts_norway$testing_policy <- as.factor(ts_norway$testing_policy)
ts_norway$contact_tracing <- as.factor(ts_norway$contact_tracing)
ts_norway$vaccination_policy <- as.factor(ts_norway$vaccination_policy)
ts_norway$facial_coverings <- as.factor(ts_norway$facial_coverings)
ts_norway$international_travel_controls <- as.factor(ts_norway$international_travel_controls)
ts_norway$public_information_campaigns <- as.factor(ts_norway$public_information_campaigns)
ts_norway$restriction_gatherings <- as.factor(ts_norway$restriction_gatherings)
ts_norway$close_public_transport <- as.factor(ts_norway$close_public_transport)
ts_norway$stay_home_requirements <- as.factor(ts_norway$stay_home_requirements)
ts_norway$workplace_closures <- as.factor(ts_norway$workplace_closures)
ts_norway$public_information_campaigns <- NULL
ts_norway$testing_policy <- NULL
set.seed(325234)
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
lcs <- inla.make.lincombs(
  id_date_1 = diag(471),
  id_date_2 = diag(471)
)
#####################################################
formula_1 <- new_cases ~
  1 + Date
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_2 <- new_cases ~
  f(id_date_1, model = "rw2", hyper = prior_1)
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_3 <- new_cases ~
  f(id_date_1, model = "rw2", hyper = prior_1) +
  f(id_date_2, model = "iid")
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:27], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)"
  )
)
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:27], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid')"
  )
)
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[c(12, 13, 16, 18, 21, 23, 25, 26)], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid')"
  )
)
res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
  
b_1 <- ts_norway[, 6:27]
b_1$geomety <- NULL
b_1$new_cases[is.na(b_1$new_cases)] <- test_value
sign <- TRUE
# multicollinearity
i <- 1
while (sign) {
  print(i) 
  i <- i + 1
  mod <- glm.nb(
    new_cases ~ .,
    data = b_1
  )
  vif_try <- try(VIF(mod)[, 1], silent = TRUE)
  if (class(vif_try) != "try-error") {
    if (!any(VIF(mod)[, 1] > 5)) {
      sign <- FALSE
    } else {
      b_1[, names(VIF(mod)[, 1][VIF(mod)[, 1] == max(VIF(mod)[, 1])])] <- NULL
    }
  } else {
    if (!any(VIF(mod) > 5)) {
      sign <- FALSE
    } else {
      b_1[, names(VIF(mod)[VIF(mod) == max(VIF(mod))])] <- NULL
    }
  }
}
backup <- ts_norway
ts_norway[, 7:27] <- NULL
ts_norway <- cbind(ts_norway[, 1:6], b_1[, 2:ncol(b_1)], ts_norway[, 7:13])
formula_7 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:13], collapse = " + "),
    "+ Date"
  )
)
res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_8 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:13], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)"
  )
)
res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_9 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:13], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid')"
  )
)
res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_10 <- new_cases ~ 1 +
  f(id_date_1, model = "ar1")
res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
ts_norway <- backup
b_2 <- ts_norway[, c(6, 12, 13, 16, 18, 21, 23, 25, 26)]
b_2$geomety <- NULL
b_2$new_cases[is.na(b_2$new_cases)] <- test_value
sign <- TRUE
# multicollinearity
i <- 1
while (sign) {
  print(i) 
  i <- i + 1
  mod <- glm.nb(
    new_cases ~ .,
    data = b_2
  )
  vif_try <- try(VIF(mod)[, 1], silent = TRUE)
  if (class(vif_try) != "try-error") {
    if (!any(VIF(mod)[, 1] > 5)) {
      sign <- FALSE
    } else {
      b_2[, names(VIF(mod)[, 1][VIF(mod)[, 1] == max(VIF(mod)[, 1])])] <- NULL
    }
  } else {
    if (!any(VIF(mod) > 5)) {
      sign <- FALSE
    } else {
      b_2[, names(VIF(mod)[VIF(mod) == max(VIF(mod))])] <- NULL
    }
  }
}
ts_norway[, 7:27] <- NULL
ts_norway <- cbind(ts_norway[, 1:6], b_2[, 2:ncol(b_2)], ts_norway[, 7:13])
formula_11 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:11], collapse = " + "),
    "+ Date"
  )
)
res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_12 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)"
  )
)
res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
formula_13 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid')"
  )
)
res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = ts_norway,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_norway$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
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
  ),
  list(
    dic = res_6$dic$dic,
    waic = res_6$waic$waic,
    cpo = sum(log(res_6$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_7$dic$dic,
    waic = res_7$waic$waic,
    cpo = sum(log(res_7$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_8$dic$dic,
    waic = res_8$waic$waic,
    cpo = sum(log(res_8$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_9$dic$dic,
    waic = res_9$waic$waic,
    cpo = sum(log(res_9$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_10$dic$dic,
    waic = res_10$waic$waic,
    cpo = sum(log(res_10$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_11$dic$dic,
    waic = res_11$waic$waic,
    cpo = sum(log(res_11$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_12$dic$dic,
    waic = res_12$waic$waic,
    cpo = sum(log(res_12$cpo$cpo), na.rm = TRUE)
  ),
  list(
    dic = res_13$dic$dic,
    waic = res_13$waic$waic,
    cpo = sum(log(res_13$cpo$cpo), na.rm = TRUE)
  )
))
# calculate the mae
mae <- c(mae, list(
  mean(abs(res_1$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_2$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_3$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_4$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_5$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_6$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_7$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_8$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_9$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_10$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_11$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_12$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_13$summary.fitted.values$mean[test] * ts_norway$expected[test] - test_value)),
  mean(abs(res_1$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_2$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_3$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_4$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_5$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_6$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_7$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_8$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_9$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_10$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_11$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_12$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test])),
  mean(abs(res_13$summary.fitted.values$mean[-test] * ts_norway$expected[-test] - ts_norway$new_cases[-test]))
))
pred_tibble <- tibble(
  q025 = c(
    res_1$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_2$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_3$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_4$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_5$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_6$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_7$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_8$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_9$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_10$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_11$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_12$summary.fitted.values$`0.025quant` * ts_norway$expected,
    res_13$summary.fitted.values$`0.025quant` * ts_norway$expected
  ),
  mean = c(
    res_1$summary.fitted.values$mean * ts_norway$expected,
    res_2$summary.fitted.values$mean * ts_norway$expected,
    res_3$summary.fitted.values$mean * ts_norway$expected,
    res_4$summary.fitted.values$mean * ts_norway$expected,
    res_5$summary.fitted.values$mean * ts_norway$expected,
    res_6$summary.fitted.values$mean * ts_norway$expected,
    res_7$summary.fitted.values$mean * ts_norway$expected,
    res_8$summary.fitted.values$mean * ts_norway$expected,
    res_9$summary.fitted.values$mean * ts_norway$expected,
    res_10$summary.fitted.values$mean * ts_norway$expected,
    res_11$summary.fitted.values$mean * ts_norway$expected,
    res_12$summary.fitted.values$mean * ts_norway$expected,
    res_13$summary.fitted.values$mean * ts_norway$expected
  ),
  q975 = c(
    res_1$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_2$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_3$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_4$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_5$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_6$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_7$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_8$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_9$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_10$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_11$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_12$summary.fitted.values$`0.975quant` * ts_norway$expected,
    res_13$summary.fitted.values$`0.975quant` * ts_norway$expected
  ),
  model = c(
    rep(1, nrow(ts_norway)),
    rep(2, nrow(ts_norway)),
    rep(3, nrow(ts_norway)),
    rep(4, nrow(ts_norway)),
    rep(5, nrow(ts_norway)),
    rep(6, nrow(ts_norway)),
    rep(7, nrow(ts_norway)),
    rep(8, nrow(ts_norway)),
    rep(9, nrow(ts_norway)),
    rep(10, nrow(ts_norway)),
    rep(11, nrow(ts_norway)),
    rep(12, nrow(ts_norway)),
    rep(13, nrow(ts_norway))
  ),
  Date = rep(ts_norway$Date, 13),
  actual = rep(c(ts_norway$new_cases[1:457], test_value), 13)
)
models <- c(models, list(
  res_1,
  res_2,
  res_3,
  res_4,
  res_5,
  res_6,
  res_7,
  res_8,
  res_9,
  res_10,
  res_11,
  res_12,
  res_13
))
models_final <- list(models, gof, mae, pred_tibble)
# save the models
save(models_final, file = "models/temporal_norway.Rda")
