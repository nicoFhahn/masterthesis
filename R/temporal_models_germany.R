# this script is used for calculating the temporal models for germany
library(INLA)
library(MASS)
library(readr)
library(regclass)
library(tibble)
# load the timeseries
ts_germany <- read_csv("wrangled_data/ts_germany.csv")
# set the cutoff day
cutoff <- 483
# get the test ids
test <- seq(cutoff, nrow(ts_germany))
# get the test values
test_value <- ts_germany$new_cases[test]
# set to NA in dataset
ts_germany$new_cases[test] <- NA
# create the link function
link <- rep(NA, nrow(ts_germany))
link[which(is.na(ts_germany$new_cases))] <- 1
# add the season variable
ts_germany$season <- "Winter"
ts_germany[ts_germany$Date >= "2020-03-20", ]$season <- "Spring"
ts_germany[ts_germany$Date >= "2020-06-20", ]$season <- "Summer"
ts_germany[ts_germany$Date >= "2020-09-22", ]$season <- "Fall"
ts_germany[ts_germany$Date >= "2020-12-21", ]$season <- "Winter"
ts_germany[ts_germany$Date >= "2021-03-20", ]$season <- "Spring"
# add the variables for the variant
ts_germany$main_variant <- 1
ts_germany$variant_20e <- 0
ts_germany$variant_20l <- 0
# set the values according to covariants.org
ts_germany[ts_germany$Date >= "2020-05-11", ]$main_variant <- 0.99
ts_germany[ts_germany$Date >= "2020-05-25", ]$main_variant <- 0.97
ts_germany[ts_germany$Date >= "2020-06-08", ]$main_variant <- 1
ts_germany[ts_germany$Date >= "2020-08-03", ]$main_variant <- 0.84
ts_germany[ts_germany$Date >= "2020-08-17", ]$main_variant <- 0.63
ts_germany[ts_germany$Date >= "2020-08-31", ]$main_variant <- 0.77
ts_germany[ts_germany$Date >= "2020-09-14", ]$main_variant <- 0.69
ts_germany[ts_germany$Date >= "2020-09-28", ]$main_variant <- 0.72
ts_germany[ts_germany$Date >= "2020-10-12", ]$main_variant <- 0.51
ts_germany[ts_germany$Date >= "2020-10-26", ]$main_variant <- 0.61
ts_germany[ts_germany$Date >= "2020-11-09", ]$main_variant <- 0.57
ts_germany[ts_germany$Date >= "2020-11-23", ]$main_variant <- 0.56
ts_germany[ts_germany$Date >= "2020-12-07", ]$main_variant <- 0.43
ts_germany[ts_germany$Date >= "2020-12-21", ]$main_variant <- 0.35
ts_germany[ts_germany$Date >= "2020-12-28", ]$main_variant <- 0.34
ts_germany[ts_germany$Date >= "2021-01-11", ]$main_variant <- 0.27
ts_germany[ts_germany$Date >= "2021-01-25", ]$main_variant <- 0.26
ts_germany[ts_germany$Date >= "2021-02-08", ]$main_variant <- 0.20
ts_germany[ts_germany$Date >= "2021-02-22", ]$main_variant <- 0.18
ts_germany[ts_germany$Date >= "2021-03-08", ]$main_variant <- 0.09
ts_germany[ts_germany$Date >= "2021-03-22", ]$main_variant <- 0.05
ts_germany[ts_germany$Date >= "2021-04-05", ]$main_variant <- 0.03
ts_germany[ts_germany$Date >= "2020-05-25", ]$variant_20e <- 0.03
ts_germany[ts_germany$Date >= "2020-06-08", ]$variant_20e <- 0
ts_germany[ts_germany$Date >= "2020-08-03", ]$variant_20e <- 0.04
ts_germany[ts_germany$Date >= "2020-08-17", ]$variant_20e <- 0.09
ts_germany[ts_germany$Date >= "2020-08-31", ]$variant_20e <- 0.06
ts_germany[ts_germany$Date >= "2020-09-14", ]$variant_20e <- 0.17
ts_germany[ts_germany$Date >= "2020-10-26", ]$variant_20e <- 0.13
ts_germany[ts_germany$Date >= "2020-11-09", ]$variant_20e <- 0.19
ts_germany[ts_germany$Date >= "2020-11-23", ]$variant_20e <- 0.22
ts_germany[ts_germany$Date >= "2020-12-07", ]$variant_20e <- 0.25
ts_germany[ts_germany$Date >= "2020-12-21", ]$variant_20e <- 0.31
ts_germany[ts_germany$Date >= "2020-12-28", ]$variant_20e <- 0.41
ts_germany[ts_germany$Date >= "2021-01-11", ]$variant_20e <- 0.30
ts_germany[ts_germany$Date >= "2021-01-25", ]$variant_20e <- 0.29
ts_germany[ts_germany$Date >= "2021-02-08", ]$variant_20e <- 0.20
ts_germany[ts_germany$Date >= "2021-02-22", ]$variant_20e <- 0.14
ts_germany[ts_germany$Date >= "2021-03-08", ]$variant_20e <- 0.06
ts_germany[ts_germany$Date >= "2021-03-22", ]$variant_20e <- 0.02
ts_germany[ts_germany$Date >= "2021-04-05", ]$variant_20e <- 0.01
ts_germany[ts_germany$Date >= "2021-04-19", ]$variant_20e <- 0
ts_germany[ts_germany$Date >= "2020-12-07", ]$variant_20l <- 0.01
ts_germany[ts_germany$Date >= "2020-12-21", ]$variant_20l <- 0.04
ts_germany[ts_germany$Date >= "2020-12-28", ]$variant_20l <- 0.01
ts_germany[ts_germany$Date >= "2021-01-11", ]$variant_20l <- 0.09
ts_germany[ts_germany$Date >= "2021-01-25", ]$variant_20l <- 0.18
ts_germany[ts_germany$Date >= "2021-02-08", ]$variant_20l <- 0.44
ts_germany[ts_germany$Date >= "2021-02-22", ]$variant_20l <- 0.55
ts_germany[ts_germany$Date >= "2021-03-08", ]$variant_20l <- 0.77
ts_germany[ts_germany$Date >= "2021-03-22", ]$variant_20l <- 0.87
ts_germany[ts_germany$Date >= "2021-04-05", ]$variant_20l <- 0.92
ts_germany[ts_germany$Date >= "2021-04-19", ]$variant_20l <- 0.95
ts_germany[ts_germany$Date >= "2021-05-03", ]$variant_20l <- 0.94
# scale the variant variables
ts_germany$main_variant <- scale(ts_germany$main_variant)[, 1]
ts_germany$variant_20e <- scale(ts_germany$variant_20e)[, 1]
ts_germany$variant_20l <- scale(ts_germany$variant_20l)[, 1]
# remove some variables
ts_germany$people_fully_vaccinated_per_hundred <- NULL
ts_germany$people_vaccinated_per_hundred <- NULL
ts_germany$school_closures <- NULL
set.seed(325234)
# specify penalized prior
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
# create lists for saving everything
models <- list()
gof <- list()
mae <- list()
formulas <- list()
# create the linear combination
lcs <- inla.make.lincombs(
  id_date_1 = diag(nrow(ts_germany)),
  id_date_2 = diag(nrow(ts_germany))
)
# define the formula
formula_1 <- new_cases ~
1 + Date
# run the model
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_2 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1)
# run the model
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_3 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1) +
  f(id_date_2, model = "iid")
# run the model
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:26], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:26], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[c(12, 14, 17, 19, 23, 25)], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# next, do some variable selection by removing variables with a VIF of above 5
# create this frame with all the variables of interest
b <- ts_germany[, 6:26]
# create a backup of the original frame
backup <- ts_germany
# remove the geometry column
b$geomety <- NULL
# add the original case numbers again
b$new_cases[is.na(b$new_cases)] <- test_value
sign <- TRUE
# remove these variables due to high correlation
b$international_travel_controls <- NULL
b$cancel_public_events <- NULL
i <- 1
while (sign) {
  # calculate a glm
  mod <- glm.nb(
    new_cases ~ .,
    data = b
  )
  # if the are variables with a VIF > 5 they will be removed, else we are done
  if (!any(VIF(mod)[, 1] > 5)) {
    sign <- FALSE
  } else {
    b[, names(VIF(mod)[, 1][VIF(mod)[, 1] == max(VIF(mod)[, 1])])] <- NULL
  }
}
# remove the variables from the original frame
ts_germany[, 7:26] <- NULL
# add only the relevant variables
ts_germany <- cbind(ts_germany[, 1:6], b[, 2:ncol(b)], ts_germany[, 7:17])
# define the formula
formula_7 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ Date"
  )
)
# run the model
res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_8 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_9 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_10 <- new_cases ~ 1 +
  f(id_date_1, model = "ar1")
# run the model
res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# now the VIF procedure again, this time using select variables
ts_germany <- backup
b_2 <- ts_germany[, c(6, 12, 14, 17, 19, 23, 25)]
b_2$geomety <- NULL
b_2$new_cases[is.na(b_2$new_cases)] <- test_value
sign <- TRUE
i <- 1
while (sign) {
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
ts_germany[, 7:26] <- NULL
ts_germany <- cbind(ts_germany[, 1:6], b_2[, 2:ncol(b_2)], ts_germany[, 7:17])
# define the formula
formula_11 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ Date"
  )
)
# run the model
res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_12 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_13 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# get all the GOF measures
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
# calculate the mae for test and train
mae <- c(mae, list(
  mean(abs(res_1$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_2$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_3$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_4$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_5$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_6$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_7$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_8$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_9$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_10$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_11$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_12$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_13$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_1$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_2$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_3$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_4$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_5$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_6$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_7$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_8$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_9$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_10$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_11$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_12$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_13$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test]))
))
# create a df containing the confidence intervals of the predictions
pred_tibble <- tibble(
  q025 = c(
    res_1$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_2$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_3$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_4$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_5$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_6$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_7$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_8$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_9$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_10$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_11$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_12$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_13$summary.fitted.values$`0.025quant` * ts_germany$expected
  ),
  mean = c(
    res_1$summary.fitted.values$mean * ts_germany$expected,
    res_2$summary.fitted.values$mean * ts_germany$expected,
    res_3$summary.fitted.values$mean * ts_germany$expected,
    res_4$summary.fitted.values$mean * ts_germany$expected,
    res_5$summary.fitted.values$mean * ts_germany$expected,
    res_6$summary.fitted.values$mean * ts_germany$expected,
    res_7$summary.fitted.values$mean * ts_germany$expected,
    res_8$summary.fitted.values$mean * ts_germany$expected,
    res_9$summary.fitted.values$mean * ts_germany$expected,
    res_10$summary.fitted.values$mean * ts_germany$expected,
    res_11$summary.fitted.values$mean * ts_germany$expected,
    res_12$summary.fitted.values$mean * ts_germany$expected,
    res_13$summary.fitted.values$mean * ts_germany$expected
  ),
  q975 = c(
    res_1$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_2$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_3$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_4$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_5$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_6$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_7$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_8$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_9$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_10$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_11$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_12$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_13$summary.fitted.values$`0.975quant` * ts_germany$expected
  ),
  model = c(
    rep(1, nrow(ts_germany)),
    rep(2, nrow(ts_germany)),
    rep(3, nrow(ts_germany)),
    rep(4, nrow(ts_germany)),
    rep(5, nrow(ts_germany)),
    rep(6, nrow(ts_germany)),
    rep(7, nrow(ts_germany)),
    rep(8, nrow(ts_germany)),
    rep(9, nrow(ts_germany)),
    rep(10, nrow(ts_germany)),
    rep(11, nrow(ts_germany)),
    rep(12, nrow(ts_germany)),
    rep(13, nrow(ts_germany))
  ),
  Date = rep(ts_germany$Date, 13),
  actual = rep(c(ts_germany$new_cases[-test], test_value), 13)
)
# get all the models
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
# get all the formulas
formulas <- c(formulas, list(
  formula_1,
  formula_2,
  formula_3,
  formula_4,
  formula_5,
  formula_6,
  formula_7,
  formula_8,
  formula_9,
  formula_10,
  formula_11,
  formula_12,
  formula_13
))
# add everything to a list
models_final <- list(models, gof, mae, pred_tibble, formulas)
# save the models
save(models_final, file = "models/temporal_germany.Rda")
rm(list = ls())
ts_germany <- read_csv("wrangled_data/ts_germany.csv")
# set the cutoff day
cutoff <- 463
# get the test ids
test <- seq(cutoff, nrow(ts_germany))
# get the test values
test_value <- ts_germany$new_cases[test]
# set to NA in dataset
ts_germany$new_cases[test] <- NA
# create the link function
link <- rep(NA, nrow(ts_germany))
link[which(is.na(ts_germany$new_cases))] <- 1
# add the season variable
ts_germany$season <- "Winter"
ts_germany[ts_germany$Date >= "2020-03-20", ]$season <- "Spring"
ts_germany[ts_germany$Date >= "2020-06-20", ]$season <- "Summer"
ts_germany[ts_germany$Date >= "2020-09-22", ]$season <- "Fall"
ts_germany[ts_germany$Date >= "2020-12-21", ]$season <- "Winter"
ts_germany[ts_germany$Date >= "2021-03-20", ]$season <- "Spring"
# add the variables for the variant
ts_germany$main_variant <- 1
ts_germany$variant_20e <- 0
ts_germany$variant_20l <- 0
# set the values according to covariants.org
ts_germany[ts_germany$Date >= "2020-05-11", ]$main_variant <- 0.99
ts_germany[ts_germany$Date >= "2020-05-25", ]$main_variant <- 0.97
ts_germany[ts_germany$Date >= "2020-06-08", ]$main_variant <- 1
ts_germany[ts_germany$Date >= "2020-08-03", ]$main_variant <- 0.84
ts_germany[ts_germany$Date >= "2020-08-17", ]$main_variant <- 0.63
ts_germany[ts_germany$Date >= "2020-08-31", ]$main_variant <- 0.77
ts_germany[ts_germany$Date >= "2020-09-14", ]$main_variant <- 0.69
ts_germany[ts_germany$Date >= "2020-09-28", ]$main_variant <- 0.72
ts_germany[ts_germany$Date >= "2020-10-12", ]$main_variant <- 0.51
ts_germany[ts_germany$Date >= "2020-10-26", ]$main_variant <- 0.61
ts_germany[ts_germany$Date >= "2020-11-09", ]$main_variant <- 0.57
ts_germany[ts_germany$Date >= "2020-11-23", ]$main_variant <- 0.56
ts_germany[ts_germany$Date >= "2020-12-07", ]$main_variant <- 0.43
ts_germany[ts_germany$Date >= "2020-12-21", ]$main_variant <- 0.35
ts_germany[ts_germany$Date >= "2020-12-28", ]$main_variant <- 0.34
ts_germany[ts_germany$Date >= "2021-01-11", ]$main_variant <- 0.27
ts_germany[ts_germany$Date >= "2021-01-25", ]$main_variant <- 0.26
ts_germany[ts_germany$Date >= "2021-02-08", ]$main_variant <- 0.20
ts_germany[ts_germany$Date >= "2021-02-22", ]$main_variant <- 0.18
ts_germany[ts_germany$Date >= "2021-03-08", ]$main_variant <- 0.09
ts_germany[ts_germany$Date >= "2021-03-22", ]$main_variant <- 0.05
ts_germany[ts_germany$Date >= "2021-04-05", ]$main_variant <- 0.03
ts_germany[ts_germany$Date >= "2020-05-25", ]$variant_20e <- 0.03
ts_germany[ts_germany$Date >= "2020-06-08", ]$variant_20e <- 0
ts_germany[ts_germany$Date >= "2020-08-03", ]$variant_20e <- 0.04
ts_germany[ts_germany$Date >= "2020-08-17", ]$variant_20e <- 0.09
ts_germany[ts_germany$Date >= "2020-08-31", ]$variant_20e <- 0.06
ts_germany[ts_germany$Date >= "2020-09-14", ]$variant_20e <- 0.17
ts_germany[ts_germany$Date >= "2020-10-26", ]$variant_20e <- 0.13
ts_germany[ts_germany$Date >= "2020-11-09", ]$variant_20e <- 0.19
ts_germany[ts_germany$Date >= "2020-11-23", ]$variant_20e <- 0.22
ts_germany[ts_germany$Date >= "2020-12-07", ]$variant_20e <- 0.25
ts_germany[ts_germany$Date >= "2020-12-21", ]$variant_20e <- 0.31
ts_germany[ts_germany$Date >= "2020-12-28", ]$variant_20e <- 0.41
ts_germany[ts_germany$Date >= "2021-01-11", ]$variant_20e <- 0.30
ts_germany[ts_germany$Date >= "2021-01-25", ]$variant_20e <- 0.29
ts_germany[ts_germany$Date >= "2021-02-08", ]$variant_20e <- 0.20
ts_germany[ts_germany$Date >= "2021-02-22", ]$variant_20e <- 0.14
ts_germany[ts_germany$Date >= "2021-03-08", ]$variant_20e <- 0.06
ts_germany[ts_germany$Date >= "2021-03-22", ]$variant_20e <- 0.02
ts_germany[ts_germany$Date >= "2021-04-05", ]$variant_20e <- 0.01
ts_germany[ts_germany$Date >= "2021-04-19", ]$variant_20e <- 0
ts_germany[ts_germany$Date >= "2020-12-07", ]$variant_20l <- 0.01
ts_germany[ts_germany$Date >= "2020-12-21", ]$variant_20l <- 0.04
ts_germany[ts_germany$Date >= "2020-12-28", ]$variant_20l <- 0.01
ts_germany[ts_germany$Date >= "2021-01-11", ]$variant_20l <- 0.09
ts_germany[ts_germany$Date >= "2021-01-25", ]$variant_20l <- 0.18
ts_germany[ts_germany$Date >= "2021-02-08", ]$variant_20l <- 0.44
ts_germany[ts_germany$Date >= "2021-02-22", ]$variant_20l <- 0.55
ts_germany[ts_germany$Date >= "2021-03-08", ]$variant_20l <- 0.77
ts_germany[ts_germany$Date >= "2021-03-22", ]$variant_20l <- 0.87
ts_germany[ts_germany$Date >= "2021-04-05", ]$variant_20l <- 0.92
ts_germany[ts_germany$Date >= "2021-04-19", ]$variant_20l <- 0.95
ts_germany[ts_germany$Date >= "2021-05-03", ]$variant_20l <- 0.94
# scale the variant variables
ts_germany$main_variant <- scale(ts_germany$main_variant)[, 1]
ts_germany$variant_20e <- scale(ts_germany$variant_20e)[, 1]
ts_germany$variant_20l <- scale(ts_germany$variant_20l)[, 1]
# remove some variables
ts_germany$people_fully_vaccinated_per_hundred <- NULL
ts_germany$people_vaccinated_per_hundred <- NULL
ts_germany$school_closures <- NULL
set.seed(325234)
# specify penalized prior
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
# create lists for saving everything
models <- list()
gof <- list()
mae <- list()
formulas <- list()
# create the linear combination
lcs <- inla.make.lincombs(
  id_date_1 = diag(nrow(ts_germany)),
  id_date_2 = diag(nrow(ts_germany))
)
# define the formula
formula_1 <- new_cases ~
1 + Date
# run the model
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_2 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1)
# run the model
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_3 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1) +
  f(id_date_2, model = "iid")
# run the model
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:26], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:26], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[c(12, 14, 17, 19, 23, 25)], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# next, do some variable selection by removing variables with a VIF of above 5
# create this frame with all the variables of interest
b <- ts_germany[, 6:26]
# create a backup of the original frame
backup <- ts_germany
# remove the geometry column
b$geomety <- NULL
# add the original case numbers again
b$new_cases[is.na(b$new_cases)] <- test_value
sign <- TRUE
# remove these variables due to high correlation
b$international_travel_controls <- NULL
b$cancel_public_events <- NULL
i <- 1
while (sign) {
  # calculate a glm
  mod <- glm.nb(
    new_cases ~ .,
    data = b
  )
  # if the are variables with a VIF > 5 they will be removed, else we are done
  if (!any(VIF(mod)[, 1] > 5)) {
    sign <- FALSE
  } else {
    b[, names(VIF(mod)[, 1][VIF(mod)[, 1] == max(VIF(mod)[, 1])])] <- NULL
  }
}
# remove the variables from the original frame
ts_germany[, 7:26] <- NULL
# add only the relevant variables
ts_germany <- cbind(ts_germany[, 1:6], b[, 2:ncol(b)], ts_germany[, 7:17])
# define the formula
formula_7 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ Date"
  )
)
# run the model
res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_8 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_9 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_10 <- new_cases ~ 1 +
  f(id_date_1, model = "ar1")
# run the model
res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# now the VIF procedure again, this time using select variables
ts_germany <- backup
b_2 <- ts_germany[, c(6, 12, 14, 17, 19, 23, 25)]
b_2$geomety <- NULL
b_2$new_cases[is.na(b_2$new_cases)] <- test_value
sign <- TRUE
i <- 1
while (sign) {
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
ts_germany[, 7:26] <- NULL
ts_germany <- cbind(ts_germany[, 1:6], b_2[, 2:ncol(b_2)], ts_germany[, 7:17])
# define the formula
formula_11 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ Date"
  )
)
# run the model
res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_12 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_13 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# get all the GOF measures
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
# calculate the mae for test and train
mae <- c(mae, list(
  mean(abs(res_1$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_2$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_3$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_4$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_5$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_6$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_7$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_8$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_9$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_10$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_11$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_12$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_13$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_1$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_2$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_3$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_4$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_5$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_6$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_7$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_8$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_9$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_10$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_11$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_12$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_13$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test]))
))
# create a df containing the confidence intervals of the predictions
pred_tibble <- tibble(
  q025 = c(
    res_1$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_2$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_3$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_4$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_5$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_6$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_7$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_8$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_9$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_10$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_11$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_12$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_13$summary.fitted.values$`0.025quant` * ts_germany$expected
  ),
  mean = c(
    res_1$summary.fitted.values$mean * ts_germany$expected,
    res_2$summary.fitted.values$mean * ts_germany$expected,
    res_3$summary.fitted.values$mean * ts_germany$expected,
    res_4$summary.fitted.values$mean * ts_germany$expected,
    res_5$summary.fitted.values$mean * ts_germany$expected,
    res_6$summary.fitted.values$mean * ts_germany$expected,
    res_7$summary.fitted.values$mean * ts_germany$expected,
    res_8$summary.fitted.values$mean * ts_germany$expected,
    res_9$summary.fitted.values$mean * ts_germany$expected,
    res_10$summary.fitted.values$mean * ts_germany$expected,
    res_11$summary.fitted.values$mean * ts_germany$expected,
    res_12$summary.fitted.values$mean * ts_germany$expected,
    res_13$summary.fitted.values$mean * ts_germany$expected
  ),
  q975 = c(
    res_1$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_2$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_3$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_4$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_5$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_6$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_7$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_8$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_9$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_10$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_11$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_12$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_13$summary.fitted.values$`0.975quant` * ts_germany$expected
  ),
  model = c(
    rep(1, nrow(ts_germany)),
    rep(2, nrow(ts_germany)),
    rep(3, nrow(ts_germany)),
    rep(4, nrow(ts_germany)),
    rep(5, nrow(ts_germany)),
    rep(6, nrow(ts_germany)),
    rep(7, nrow(ts_germany)),
    rep(8, nrow(ts_germany)),
    rep(9, nrow(ts_germany)),
    rep(10, nrow(ts_germany)),
    rep(11, nrow(ts_germany)),
    rep(12, nrow(ts_germany)),
    rep(13, nrow(ts_germany))
  ),
  Date = rep(ts_germany$Date, 13),
  actual = rep(c(ts_germany$new_cases[-test], test_value), 13)
)
# get all the models
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
# get all the formulas
formulas <- c(formulas, list(
  formula_1,
  formula_2,
  formula_3,
  formula_4,
  formula_5,
  formula_6,
  formula_7,
  formula_8,
  formula_9,
  formula_10,
  formula_11,
  formula_12,
  formula_13
))
# add everything to a list
models_final <- list(models, gof, mae, pred_tibble, formulas)
# save the models
save(models_final, file = "models/temporal_germany_2.Rda")
rm(list = ls())
ts_germany <- read_csv("wrangled_data/ts_germany.csv")
# set the cutoff day
cutoff <- 443
# get the test ids
test <- seq(cutoff, nrow(ts_germany))
# get the test values
test_value <- ts_germany$new_cases[test]
# set to NA in dataset
ts_germany$new_cases[test] <- NA
# create the link function
link <- rep(NA, nrow(ts_germany))
link[which(is.na(ts_germany$new_cases))] <- 1
# add the season variable
ts_germany$season <- "Winter"
ts_germany[ts_germany$Date >= "2020-03-20", ]$season <- "Spring"
ts_germany[ts_germany$Date >= "2020-06-20", ]$season <- "Summer"
ts_germany[ts_germany$Date >= "2020-09-22", ]$season <- "Fall"
ts_germany[ts_germany$Date >= "2020-12-21", ]$season <- "Winter"
ts_germany[ts_germany$Date >= "2021-03-20", ]$season <- "Spring"
# add the variables for the variant
ts_germany$main_variant <- 1
ts_germany$variant_20e <- 0
ts_germany$variant_20l <- 0
# set the values according to covariants.org
ts_germany[ts_germany$Date >= "2020-05-11", ]$main_variant <- 0.99
ts_germany[ts_germany$Date >= "2020-05-25", ]$main_variant <- 0.97
ts_germany[ts_germany$Date >= "2020-06-08", ]$main_variant <- 1
ts_germany[ts_germany$Date >= "2020-08-03", ]$main_variant <- 0.84
ts_germany[ts_germany$Date >= "2020-08-17", ]$main_variant <- 0.63
ts_germany[ts_germany$Date >= "2020-08-31", ]$main_variant <- 0.77
ts_germany[ts_germany$Date >= "2020-09-14", ]$main_variant <- 0.69
ts_germany[ts_germany$Date >= "2020-09-28", ]$main_variant <- 0.72
ts_germany[ts_germany$Date >= "2020-10-12", ]$main_variant <- 0.51
ts_germany[ts_germany$Date >= "2020-10-26", ]$main_variant <- 0.61
ts_germany[ts_germany$Date >= "2020-11-09", ]$main_variant <- 0.57
ts_germany[ts_germany$Date >= "2020-11-23", ]$main_variant <- 0.56
ts_germany[ts_germany$Date >= "2020-12-07", ]$main_variant <- 0.43
ts_germany[ts_germany$Date >= "2020-12-21", ]$main_variant <- 0.35
ts_germany[ts_germany$Date >= "2020-12-28", ]$main_variant <- 0.34
ts_germany[ts_germany$Date >= "2021-01-11", ]$main_variant <- 0.27
ts_germany[ts_germany$Date >= "2021-01-25", ]$main_variant <- 0.26
ts_germany[ts_germany$Date >= "2021-02-08", ]$main_variant <- 0.20
ts_germany[ts_germany$Date >= "2021-02-22", ]$main_variant <- 0.18
ts_germany[ts_germany$Date >= "2021-03-08", ]$main_variant <- 0.09
ts_germany[ts_germany$Date >= "2021-03-22", ]$main_variant <- 0.05
ts_germany[ts_germany$Date >= "2021-04-05", ]$main_variant <- 0.03
ts_germany[ts_germany$Date >= "2020-05-25", ]$variant_20e <- 0.03
ts_germany[ts_germany$Date >= "2020-06-08", ]$variant_20e <- 0
ts_germany[ts_germany$Date >= "2020-08-03", ]$variant_20e <- 0.04
ts_germany[ts_germany$Date >= "2020-08-17", ]$variant_20e <- 0.09
ts_germany[ts_germany$Date >= "2020-08-31", ]$variant_20e <- 0.06
ts_germany[ts_germany$Date >= "2020-09-14", ]$variant_20e <- 0.17
ts_germany[ts_germany$Date >= "2020-10-26", ]$variant_20e <- 0.13
ts_germany[ts_germany$Date >= "2020-11-09", ]$variant_20e <- 0.19
ts_germany[ts_germany$Date >= "2020-11-23", ]$variant_20e <- 0.22
ts_germany[ts_germany$Date >= "2020-12-07", ]$variant_20e <- 0.25
ts_germany[ts_germany$Date >= "2020-12-21", ]$variant_20e <- 0.31
ts_germany[ts_germany$Date >= "2020-12-28", ]$variant_20e <- 0.41
ts_germany[ts_germany$Date >= "2021-01-11", ]$variant_20e <- 0.30
ts_germany[ts_germany$Date >= "2021-01-25", ]$variant_20e <- 0.29
ts_germany[ts_germany$Date >= "2021-02-08", ]$variant_20e <- 0.20
ts_germany[ts_germany$Date >= "2021-02-22", ]$variant_20e <- 0.14
ts_germany[ts_germany$Date >= "2021-03-08", ]$variant_20e <- 0.06
ts_germany[ts_germany$Date >= "2021-03-22", ]$variant_20e <- 0.02
ts_germany[ts_germany$Date >= "2021-04-05", ]$variant_20e <- 0.01
ts_germany[ts_germany$Date >= "2021-04-19", ]$variant_20e <- 0
ts_germany[ts_germany$Date >= "2020-12-07", ]$variant_20l <- 0.01
ts_germany[ts_germany$Date >= "2020-12-21", ]$variant_20l <- 0.04
ts_germany[ts_germany$Date >= "2020-12-28", ]$variant_20l <- 0.01
ts_germany[ts_germany$Date >= "2021-01-11", ]$variant_20l <- 0.09
ts_germany[ts_germany$Date >= "2021-01-25", ]$variant_20l <- 0.18
ts_germany[ts_germany$Date >= "2021-02-08", ]$variant_20l <- 0.44
ts_germany[ts_germany$Date >= "2021-02-22", ]$variant_20l <- 0.55
ts_germany[ts_germany$Date >= "2021-03-08", ]$variant_20l <- 0.77
ts_germany[ts_germany$Date >= "2021-03-22", ]$variant_20l <- 0.87
ts_germany[ts_germany$Date >= "2021-04-05", ]$variant_20l <- 0.92
ts_germany[ts_germany$Date >= "2021-04-19", ]$variant_20l <- 0.95
ts_germany[ts_germany$Date >= "2021-05-03", ]$variant_20l <- 0.94
# scale the variant variables
ts_germany$main_variant <- scale(ts_germany$main_variant)[, 1]
ts_germany$variant_20e <- scale(ts_germany$variant_20e)[, 1]
ts_germany$variant_20l <- scale(ts_germany$variant_20l)[, 1]
# remove some variables
ts_germany$people_fully_vaccinated_per_hundred <- NULL
ts_germany$people_vaccinated_per_hundred <- NULL
ts_germany$school_closures <- NULL
set.seed(325234)
# specify penalized prior
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
# create lists for saving everything
models <- list()
gof <- list()
mae <- list()
formulas <- list()
# create the linear combination
lcs <- inla.make.lincombs(
  id_date_1 = diag(nrow(ts_germany)),
  id_date_2 = diag(nrow(ts_germany))
)
# define the formula
formula_1 <- new_cases ~
1 + Date
# run the model
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_2 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1)
# run the model
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_3 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1) +
  f(id_date_2, model = "iid")
# run the model
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:26], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:26], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[c(12, 14, 17, 19, 23, 25)], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# next, do some variable selection by removing variables with a VIF of above 5
# create this frame with all the variables of interest
b <- ts_germany[, 6:26]
# create a backup of the original frame
backup <- ts_germany
# remove the geometry column
b$geomety <- NULL
# add the original case numbers again
b$new_cases[is.na(b$new_cases)] <- test_value
sign <- TRUE
# remove these variables due to high correlation
b$international_travel_controls <- NULL
b$cancel_public_events <- NULL
i <- 1
while (sign) {
  # calculate a glm
  mod <- glm.nb(
    new_cases ~ .,
    data = b
  )
  # if the are variables with a VIF > 5 they will be removed, else we are done
  if (!any(VIF(mod)[, 1] > 5)) {
    sign <- FALSE
  } else {
    b[, names(VIF(mod)[, 1][VIF(mod)[, 1] == max(VIF(mod)[, 1])])] <- NULL
  }
}
# remove the variables from the original frame
ts_germany[, 7:26] <- NULL
# add only the relevant variables
ts_germany <- cbind(ts_germany[, 1:6], b[, 2:ncol(b)], ts_germany[, 7:17])
# define the formula
formula_7 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ Date"
  )
)
# run the model
res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_8 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_9 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:12], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_10 <- new_cases ~ 1 +
  f(id_date_1, model = "ar1")
# run the model
res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# now the VIF procedure again, this time using select variables
ts_germany <- backup
b_2 <- ts_germany[, c(6, 12, 14, 17, 19, 23, 25)]
b_2$geomety <- NULL
b_2$new_cases[is.na(b_2$new_cases)] <- test_value
sign <- TRUE
i <- 1
while (sign) {
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
ts_germany[, 7:26] <- NULL
ts_germany <- cbind(ts_germany[, 1:6], b_2[, 2:ncol(b_2)], ts_germany[, 7:17])
# define the formula
formula_11 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ Date"
  )
)
# run the model
res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_12 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# define the formula
formula_13 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_germany)[7:11], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = ts_germany,
  E = expected,
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  Ntrials = ts_germany$population,
  lincomb = lcs,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
# get all the GOF measures
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
# calculate the mae for test and train
mae <- c(mae, list(
  mean(abs(res_1$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_2$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_3$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_4$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_5$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_6$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_7$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_8$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_9$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_10$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_11$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_12$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_13$summary.fitted.values$mean[test] * ts_germany$expected[test] - test_value)),
  mean(abs(res_1$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_2$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_3$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_4$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_5$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_6$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_7$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_8$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_9$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_10$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_11$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_12$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test])),
  mean(abs(res_13$summary.fitted.values$mean[-test] * ts_germany$expected[-test] - ts_germany$new_cases[-test]))
))
# create a df containing the confidence intervals of the predictions
pred_tibble <- tibble(
  q025 = c(
    res_1$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_2$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_3$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_4$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_5$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_6$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_7$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_8$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_9$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_10$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_11$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_12$summary.fitted.values$`0.025quant` * ts_germany$expected,
    res_13$summary.fitted.values$`0.025quant` * ts_germany$expected
  ),
  mean = c(
    res_1$summary.fitted.values$mean * ts_germany$expected,
    res_2$summary.fitted.values$mean * ts_germany$expected,
    res_3$summary.fitted.values$mean * ts_germany$expected,
    res_4$summary.fitted.values$mean * ts_germany$expected,
    res_5$summary.fitted.values$mean * ts_germany$expected,
    res_6$summary.fitted.values$mean * ts_germany$expected,
    res_7$summary.fitted.values$mean * ts_germany$expected,
    res_8$summary.fitted.values$mean * ts_germany$expected,
    res_9$summary.fitted.values$mean * ts_germany$expected,
    res_10$summary.fitted.values$mean * ts_germany$expected,
    res_11$summary.fitted.values$mean * ts_germany$expected,
    res_12$summary.fitted.values$mean * ts_germany$expected,
    res_13$summary.fitted.values$mean * ts_germany$expected
  ),
  q975 = c(
    res_1$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_2$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_3$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_4$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_5$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_6$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_7$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_8$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_9$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_10$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_11$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_12$summary.fitted.values$`0.975quant` * ts_germany$expected,
    res_13$summary.fitted.values$`0.975quant` * ts_germany$expected
  ),
  model = c(
    rep(1, nrow(ts_germany)),
    rep(2, nrow(ts_germany)),
    rep(3, nrow(ts_germany)),
    rep(4, nrow(ts_germany)),
    rep(5, nrow(ts_germany)),
    rep(6, nrow(ts_germany)),
    rep(7, nrow(ts_germany)),
    rep(8, nrow(ts_germany)),
    rep(9, nrow(ts_germany)),
    rep(10, nrow(ts_germany)),
    rep(11, nrow(ts_germany)),
    rep(12, nrow(ts_germany)),
    rep(13, nrow(ts_germany))
  ),
  Date = rep(ts_germany$Date, 13),
  actual = rep(c(ts_germany$new_cases[-test], test_value), 13)
)
# get all the models
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
# get all the formulas
formulas <- c(formulas, list(
  formula_1,
  formula_2,
  formula_3,
  formula_4,
  formula_5,
  formula_6,
  formula_7,
  formula_8,
  formula_9,
  formula_10,
  formula_11,
  formula_12,
  formula_13
))
# add everything to a list
models_final <- list(models, gof, mae, pred_tibble, formulas)
# save the models
save(models_final, file = "models/temporal_germany_3.Rda")
rm(list = ls())
