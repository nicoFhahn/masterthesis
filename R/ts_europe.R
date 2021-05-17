library(INLA)
library(SpatialEpi)
library(eurostat)
library(ISOcodes)
library(spdep)
library(stringr)
library(dplyr)
library(MASS)
source("R/preprocess_timeseries.R")
set.seed(2354324)
backup <- ts_europe
test <- ts_europe[ts_europe$id_date_1 >= 450, ]
test_value <- test$new_cases
ts_europe[ts_europe$id_date_1 >= 450, ]$new_cases <- NA
link <- rep(NA, nrow(ts_europe))
link[which(is.na(ts_europe$new_cases))] <- 1
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
nb <- poly2nb(ts_europe[!duplicated(ts_europe$CNTR_CODE), ])
if (length(unique((ts_europe$Country))) == 1) nb <- poly2nb(ts_europe)
nb2INLA("maps/map_3.adj", nb)
g <- inla.read.graph(filename = "maps/map_3.adj")
formula_1 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[7:41], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_country_2, model = 'iid')",
    "+ Date"
  )
)
res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  E = expected,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  verbose = TRUE
)
formula_2 <- new_cases ~ 1 +
  f(id_country_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1) +
  Date
formula_2 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[7:41], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')"
  )
)
lcs <- inla.make.lincombs(id_date_1 = diag(length(unique(ts_europe$Date))), id_date_2 = diag(length(unique(ts_europe$Date))))
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  # lincomb = lcs,
  verbose = TRUE
)
formula_3 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[7:41], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')",
    "+ f(id_date_area, model = 'iid')"
  )
)
res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  verbose = TRUE
)
b <- ts_europe[, c(6:41)]
b$geometry <- NULL
sign <- TRUE
while (sign) {
  mod <- try(glm.nb(
    new_cases ~ .,
    data = b
  ), silent = TRUE)
  if (class(mod) == "try-error") {
    mod <- lm(
      new_cases ~ .,
      data = b
    )
  }
  if (!any(VIF(mod) > 5)) {
    sign <- FALSE
  } else {
    b[, names(VIF(mod)[, 1])[VIF(mod)[, 1] == max(VIF(mod)[, 1])]] <- NULL
  }
}
ts_europe[, c(6:41)] <- NULL
ts_europe <- st_as_sf(cbind(ts_europe, b))
formula_4 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[14:35], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_country_2, model = 'iid')",
    "+ Date"
  )
)
res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  E = expected,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  verbose = TRUE
)
formula_5 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[14:22], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')"
  )
)
lcs <- inla.make.lincombs(id_date_1 = diag(length(unique(ts_europe$Date))), id_date_2 = diag(length(unique(ts_europe$Date))))
res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  lincomb = lcs,
  verbose = TRUE
)
formula_6 <- as.formula(
  paste(
    "new_cases ~",
    paste(colnames(ts_europe)[14:35], collapse = " + "),
    "+ f(id_country_1, model = 'bym2', graph = g, scale.model = TRUE, hyper = prior_1)",
    "+ f(id_date_1, model = 'rw2')",
    "+ f(id_date_2, model = 'iid')",
    "+ f(id_date_area, model = 'iid')"
  )
)
res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = ts_europe,
  Ntrials = ts_europe$population,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(
    compute = TRUE,
    link = link
  ),
  E = expected,
  verbose = TRUE
)
predicted_1 <- c()
predicted_2 <- c()
predicted_3 <- c()
predicted_4 <- c()
predicted_5 <- c()
predicted_6 <- c()
# make predictions
for (i in seq_len(nrow(ts_europe))) {
  # predicted_1[i] <- inla.emarginal(
  #   function(x) x * ts_europe$population[i],
  #   res_1$marginals.fitted.values[[i]]
  # )
  predicted_2[i] <- inla.emarginal(
    function(x) x * ts_europe$population[i],
    res_2$marginals.fitted.values[[i]]
  )
  # predicted_3[i] <- inla.emarginal(
  #   function(x) x * ts_europe$population[i],
  #   res_3$marginals.fitted.values[[i]]
  # )
  # predicted_4[i] <- inla.emarginal(
  #   function(x) x * ts_europe$population[i],
  #   res_4$marginals.fitted.values[[i]]
  # )
  # predicted_5[i] <- inla.emarginal(
  #   function(x) x * ts_europe$population[i],
  #   res_5$marginals.fitted.values[[i]]
  # )
  # predicted_6[i] <- inla.emarginal(
  #   function(x) x * ts_europe$population[i],
  #   res_6$marginals.fitted.values[[i]]
  # )
}
predicted_1
ts_europe$pred_1 <- predicted_1
ts_europe$pred_2 <- predicted_2
ts_europe$pred_3 <- predicted_3
ts_europe$pred_4 <- predicted_4
ts_europe$pred_5 <- predicted_5
ts_europe$pred_6 <- predicted_6
mean(abs(ts_europe[ts_europe$id_date_area >= 450, ]$new_cases - ts_europe[ts_europe$id_date_area >= 450, ]$pred_1), na.rm = TRUE)
mean(abs(ts_europe[ts_europe$id_date_area >= 450, ]$new_cases - ts_europe[ts_europe$id_date_area >= 450, ]$pred_2), na.rm = TRUE)
mean(abs(ts_europe[ts_europe$id_date_area >= 450, ]$new_cases - ts_europe[ts_europe$id_date_area >= 450, ]$pred_3), na.rm = TRUE)
mean(abs(ts_europe[ts_europe$id_date_area >= 450, ]$new_cases - ts_europe[ts_europe$id_date_area >= 450, ]$pred_4), na.rm = TRUE)
mean(abs(ts_europe[ts_europe$id_date_area >= 450, ]$new_cases - ts_europe[ts_europe$id_date_area >= 450, ]$pred_5), na.rm = TRUE)
mean(abs(ts_europe[ts_europe$id_date_area >= 450, ]$new_cases - ts_europe[ts_europe$id_date_area >= 450, ]$pred_6), na.rm = TRUE)
