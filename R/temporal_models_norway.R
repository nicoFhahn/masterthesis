# this script is used for calculating the temporal models for norway
library(INLA)
library(MASS)
library(readr)
library(regclass)
library(tibble)
# load the timeseries
ts_norway <- read_csv("wrangled_data/ts_norway.csv")
# set the cutoff day
cutoff <- 454
# get the test ids
test <- seq(cutoff, nrow(ts_norway))
# get the test values
test_value <- ts_norway$new_cases[test]
# set to NA in dataset
ts_norway$new_cases[test] <- NA
# create the link function
link <- rep(NA, nrow(ts_norway))
link[which(is.na(ts_norway$new_cases))] <- 1
# remove some variables
ts_norway$public_information_campaigns <- NULL
ts_norway$testing_policy <- NULL
ts_norway$people_fully_vaccinated_per_hundred <- NULL
ts_norway$people_vaccinated_per_hundred <- NULL
# add the season variable
ts_norway$season <- "Winter"
ts_norway[ts_norway$Date >= "2020-03-20", ]$season <- "Spring"
ts_norway[ts_norway$Date >= "2020-06-20", ]$season <- "Summer"
ts_norway[ts_norway$Date >= "2020-09-22", ]$season <- "Fall"
ts_norway[ts_norway$Date >= "2020-12-21", ]$season <- "Winter"
ts_norway[ts_norway$Date >= "2021-03-20", ]$season <- "Spring"
# add the variables for the variant
ts_norway$main_variant <- 1
ts_norway$variant_20a_eu2 <- 0
ts_norway$variant_20a_s439 <- 0
ts_norway$variant_20b <- 0
ts_norway$variant_20c <- 0
ts_norway$variant_20e <- 0
ts_norway$variant_20l <- 0
# set the values according to covariants.org
ts_norway[ts_norway$Date >= "2020-06-22", ]$main_variant <- 0.97
ts_norway[ts_norway$Date >= "2020-07-06", ]$main_variant <- 1
ts_norway[ts_norway$Date >= "2020-07-20", ]$main_variant <- 0.87
ts_norway[ts_norway$Date >= "2020-08-03", ]$main_variant <- 0.29
ts_norway[ts_norway$Date >= "2020-08-17", ]$main_variant <- 0.78
ts_norway[ts_norway$Date >= "2020-08-31", ]$main_variant <- 0.81
ts_norway[ts_norway$Date >= "2020-09-14", ]$main_variant <- 0.21
ts_norway[ts_norway$Date >= "2020-09-28", ]$main_variant <- 0.26
ts_norway[ts_norway$Date >= "2020-10-12", ]$main_variant <- 0.34
ts_norway[ts_norway$Date >= "2020-10-26", ]$main_variant <- 0.49
ts_norway[ts_norway$Date >= "2020-11-09", ]$main_variant <- 0.53
ts_norway[ts_norway$Date >= "2020-11-23", ]$main_variant <- 0.69
ts_norway[ts_norway$Date >= "2020-12-07", ]$main_variant <- 0.77
ts_norway[ts_norway$Date >= "2020-12-21", ]$main_variant <- 0.65
ts_norway[ts_norway$Date >= "2020-12-28", ]$main_variant <- 0.47
ts_norway[ts_norway$Date >= "2021-01-11", ]$main_variant <- 0.57
ts_norway[ts_norway$Date >= "2021-01-25", ]$main_variant <- 0.52
ts_norway[ts_norway$Date >= "2021-02-08", ]$main_variant <- 0.41
ts_norway[ts_norway$Date >= "2021-02-22", ]$main_variant <- 0.24
ts_norway[ts_norway$Date >= "2021-03-08", ]$main_variant <- 0.13
ts_norway[ts_norway$Date >= "2021-03-22", ]$main_variant <- 0.07
ts_norway[ts_norway$Date >= "2021-04-05", ]$main_variant <- 0.04
ts_norway[ts_norway$Date >= "2021-04-19", ]$main_variant <- 0.04
ts_norway[ts_norway$Date >= "2021-05-03", ]$main_variant <- 0.10
ts_norway[ts_norway$Date >= "2020-09-14", ]$variant_20a_eu2 <- 0.46
ts_norway[ts_norway$Date >= "2020-09-28", ]$variant_20a_eu2 <- 0.15
ts_norway[ts_norway$Date >= "2020-10-12", ]$variant_20a_eu2 <- 0.07
ts_norway[ts_norway$Date >= "2020-10-26", ]$variant_20a_eu2 <- 0.10
ts_norway[ts_norway$Date >= "2020-11-09", ]$variant_20a_eu2 <- 0.02
ts_norway[ts_norway$Date >= "2020-11-23", ]$variant_20a_eu2 <- 0.02
ts_norway[ts_norway$Date >= "2020-12-07", ]$variant_20a_eu2 <- 0
ts_norway[ts_norway$Date >= "2020-12-21", ]$variant_20a_eu2 <- 0.01
ts_norway[ts_norway$Date >= "2020-12-28", ]$variant_20a_eu2 <- 0.04
ts_norway[ts_norway$Date >= "2021-01-11", ]$variant_20a_eu2 <- 0.04
ts_norway[ts_norway$Date >= "2021-01-25", ]$variant_20a_eu2 <- 0.01
ts_norway[ts_norway$Date >= "2021-02-08", ]$variant_20a_eu2 <- 0.01
ts_norway[ts_norway$Date >= "2021-02-22", ]$variant_20a_eu2 <- 0
ts_norway[ts_norway$Date >= "2020-06-22", ]$main_variant <- 0.97
ts_norway[ts_norway$Date >= "2020-07-06", ]$variant_20a_s439 <- 1
ts_norway[ts_norway$Date >= "2020-07-20", ]$variant_20a_s439 <- 0.87
ts_norway[ts_norway$Date >= "2020-08-03", ]$variant_20a_s439 <- 0.29
ts_norway[ts_norway$Date >= "2020-08-17", ]$variant_20a_s439 <- 0.78
ts_norway[ts_norway$Date >= "2020-08-31", ]$variant_20a_s439 <- 0.81
ts_norway[ts_norway$Date >= "2020-09-14", ]$variant_20a_s439 <- 0.21
ts_norway[ts_norway$Date >= "2020-09-28", ]$variant_20a_s439 <- 0.26
ts_norway[ts_norway$Date >= "2020-10-12", ]$variant_20a_s439 <- 0.34
ts_norway[ts_norway$Date >= "2020-10-26", ]$variant_20a_s439 <- 0.49
ts_norway[ts_norway$Date >= "2020-11-09", ]$variant_20a_s439 <- 0.53
ts_norway[ts_norway$Date >= "2020-11-23", ]$variant_20a_s439 <- 0.69
ts_norway[ts_norway$Date >= "2020-12-07", ]$variant_20a_s439 <- 0.77
ts_norway[ts_norway$Date >= "2020-12-21", ]$variant_20a_s439 <- 0.65
ts_norway[ts_norway$Date >= "2020-12-28", ]$variant_20a_s439 <- 0.47
ts_norway[ts_norway$Date >= "2021-01-11", ]$variant_20a_s439 <- 0.57
ts_norway[ts_norway$Date >= "2021-01-25", ]$variant_20a_s439 <- 0.52
ts_norway[ts_norway$Date >= "2021-02-08", ]$variant_20a_s439 <- 0.41
ts_norway[ts_norway$Date >= "2021-02-22", ]$variant_20a_s439 <- 0.24
ts_norway[ts_norway$Date >= "2021-03-08", ]$variant_20a_s439 <- 0.13
ts_norway[ts_norway$Date >= "2021-03-22", ]$variant_20a_s439 <- 0.07
ts_norway[ts_norway$Date >= "2021-04-05", ]$variant_20a_s439 <- 0.04
ts_norway[ts_norway$Date >= "2021-04-19", ]$variant_20a_s439 <- 0.04
ts_norway[ts_norway$Date >= "2021-05-03", ]$variant_20a_s439 <- 0.10
ts_norway[ts_norway$Date >= "2020-08-03", ]$variant_20b <- 0.04
ts_norway[ts_norway$Date >= "2020-08-17", ]$variant_20b <- 0.07
ts_norway[ts_norway$Date >= "2020-08-31", ]$variant_20b <- 0.05
ts_norway[ts_norway$Date >= "2020-09-14", ]$variant_20b <- 0.16
ts_norway[ts_norway$Date >= "2020-09-28", ]$variant_20b <- 0.36
ts_norway[ts_norway$Date >= "2020-10-12", ]$variant_20b <- 0.23
ts_norway[ts_norway$Date >= "2020-10-26", ]$variant_20b <- 0.11
ts_norway[ts_norway$Date >= "2020-11-09", ]$variant_20b <- 0.06
ts_norway[ts_norway$Date >= "2020-11-23", ]$variant_20b <- 0.02
ts_norway[ts_norway$Date >= "2020-12-07", ]$variant_20b <- 0.02
ts_norway[ts_norway$Date >= "2020-12-21", ]$variant_20b <- 0.02
ts_norway[ts_norway$Date >= "2020-12-28", ]$variant_20b <- 0
ts_norway[ts_norway$Date >= "2021-01-11", ]$variant_20b <- 0
ts_norway[ts_norway$Date >= "2021-01-25", ]$variant_20b <- 0.01
ts_norway[ts_norway$Date >= "2020-08-03", ]$variant_20c <- 0.15
ts_norway[ts_norway$Date >= "2020-08-17", ]$variant_20c <- 0.04
ts_norway[ts_norway$Date >= "2020-08-31", ]$variant_20c <- 0.13
ts_norway[ts_norway$Date >= "2020-09-14", ]$variant_20c <- 0.13
ts_norway[ts_norway$Date >= "2020-09-28", ]$variant_20c <- 0.07
ts_norway[ts_norway$Date >= "2020-10-12", ]$variant_20c <- 0.06
ts_norway[ts_norway$Date >= "2020-10-26", ]$variant_20c <- 0.02
ts_norway[ts_norway$Date >= "2020-11-09", ]$variant_20c <- 0
ts_norway[ts_norway$Date >= "2020-07-20", ]$variant_20e <- 0.13
ts_norway[ts_norway$Date >= "2020-08-03", ]$variant_20e <- 0.52
ts_norway[ts_norway$Date >= "2020-08-17", ]$variant_20e <- 0.07
ts_norway[ts_norway$Date >= "2020-08-31", ]$variant_20e <- 0
ts_norway[ts_norway$Date >= "2020-09-14", ]$variant_20e <- 0.01
ts_norway[ts_norway$Date >= "2020-09-28", ]$variant_20e <- 0.07
ts_norway[ts_norway$Date >= "2020-10-12", ]$variant_20e <- 0.21
ts_norway[ts_norway$Date >= "2020-10-26", ]$variant_20e <- 0.13
ts_norway[ts_norway$Date >= "2020-11-09", ]$variant_20e <- 0.27
ts_norway[ts_norway$Date >= "2020-11-23", ]$variant_20e <- 0.15
ts_norway[ts_norway$Date >= "2020-12-07", ]$variant_20e <- 0.11
ts_norway[ts_norway$Date >= "2020-12-21", ]$variant_20e <- 0.20
ts_norway[ts_norway$Date >= "2020-12-28", ]$variant_20e <- 0.36
ts_norway[ts_norway$Date >= "2021-01-11", ]$variant_20e <- 0.15
ts_norway[ts_norway$Date >= "2021-01-25", ]$variant_20e <- 0.12
ts_norway[ts_norway$Date >= "2021-02-08", ]$variant_20e <- 0.07
ts_norway[ts_norway$Date >= "2021-02-22", ]$variant_20e <- 0.03
ts_norway[ts_norway$Date >= "2021-03-08", ]$variant_20e <- 0.01
ts_norway[ts_norway$Date >= "2021-03-22", ]$variant_20e <- 0
ts_norway[ts_norway$Date >= "2020-12-07", ]$variant_20l <- 0.06
ts_norway[ts_norway$Date >= "2020-12-21", ]$variant_20l <- 0.10
ts_norway[ts_norway$Date >= "2020-12-28", ]$variant_20l <- 0.10
ts_norway[ts_norway$Date >= "2021-01-11", ]$variant_20l <- 0.12
ts_norway[ts_norway$Date >= "2021-01-25", ]$variant_20l <- 0.27
ts_norway[ts_norway$Date >= "2021-02-08", ]$variant_20l <- 0.51
ts_norway[ts_norway$Date >= "2021-02-22", ]$variant_20l <- 0.73
ts_norway[ts_norway$Date >= "2021-03-08", ]$variant_20l <- 0.85
ts_norway[ts_norway$Date >= "2021-03-22", ]$variant_20l <- 0.93
ts_norway[ts_norway$Date >= "2021-04-05", ]$variant_20l <- 0.95
ts_norway[ts_norway$Date >= "2021-04-19", ]$variant_20l <- 0.95
ts_norway[ts_norway$Date >= "2021-05-03", ]$variant_20l <- 0.90
# scale the variant variables
ts_norway$main_variant <- scale(ts_norway$main_variant)[, 1]
ts_norway$variant_20a_eu2 <- scale(ts_norway$variant_20a_eu2)[, 1]
ts_norway$variant_20a_s439 <- scale(ts_norway$variant_20a_s439)[, 1]
ts_norway$variant_20b <- scale(ts_norway$variant_20b)[, 1]
ts_norway$variant_20c <- scale(ts_norway$variant_20c)[, 1]
ts_norway$variant_20e <- scale(ts_norway$variant_20e)[, 1]
ts_norway$variant_20l <- scale(ts_norway$variant_20l)[, 1]
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
  id_date_1 = diag(nrow(ts_norway)),
  id_date_2 = diag(nrow(ts_norway))
)
#####################################################
# define the formula
formula_1 <- new_cases ~
1 + Date
# run the model
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
# define the formula
formula_2 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1)
# run the model
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
# define the formula
formula_3 <- new_cases ~
f(id_date_1, model = "rw2", hyper = prior_1) +
  f(id_date_2, model = "iid")
# run the model
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
# define the formula
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:25], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
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
# define the formula
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:25], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"    
  )
)
# run the model
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
# define the formula
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[c(8, 12, 18, 24, 25)], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
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
# next, do some variable selection by removing variables with a VIF of above 5
# create this frame with all the variables of interest
b_1 <- ts_norway[, 6:25]
# create a backup of the original frame
backup <- ts_norway
# remove the geometry column
b_1$geomety <- NULL
# add the original cases numbers again
b_1$new_cases[is.na(b_1$new_cases)] <- test_value
sign <- TRUE
while (sign) {
  # calculate a glm
  mod <- glm.nb(
    new_cases ~ .,
    data = b_1
  )
  # if the are variables with a VIF > 5 they will be removed, else we are done
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
# remove the variables from the original frame
ts_norway[, 7:25] <- NULL
# add only the relevant variables
ts_norway <- cbind(ts_norway[, 1:6], b_1[, 2:ncol(b_1)], ts_norway[, 7:21])
# define the formula
formula_7 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:13], collapse = " + "),
    "+ Date"
  )
)
# run the model
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
# define the formula
formula_8 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:13], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
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
# define the formula
formula_9 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:13], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
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
# define the formula
formula_10 <- new_cases ~ 1 +
  f(id_date_1, model = "ar1")
# run the model
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
# now the VIF procedure again, this time using select variables
ts_norway <- backup
b_2 <- ts_norway[, c(6, 8, 12, 18, 24, 25)]
b_2$geomety <- NULL
b_2$new_cases[is.na(b_2$new_cases)] <- test_value
sign <- TRUE
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
ts_norway[, 7:25] <- NULL
ts_norway <- cbind(ts_norway[, 1:6], b_2[, 2:ncol(b_2)], ts_norway[, 7:21])
# define the formula
formula_11 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:10], collapse = " + "),
    "+ Date"
  )
)
# run the model
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
# define the formula
formula_12 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:10], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1) + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
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
# define the formula
formula_13 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_norway)[7:10], collapse = " + "),
    "+ f(id_date_1, model = 'rw2', hyper = prior_1)",
    "+ f(id_date_2, model = 'iid') + season",
    "+ main_variant + variant_20e + variant_20l"
  )
)
# run the model
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
# create a df containing the confidence intervals of the predictions
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
  actual = rep(c(ts_norway$new_cases[-test], test_value), 13)
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
# and save it
save(models_final, file = "models/temporal_norway.Rda")
rm(list = ls())
