library(ggplot2)
library(htmlwidgets)
library(INLA)
library(INLAutils)
library(leaflet)
library(leaflet.mapboxgl)
library(mlr)
library(randomForestSRC)
library(spdep)
source("R/preprocess_norge.R")
set.seed(420)
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
#
# create the neighbordhood matrix
nb <- poly2nb(newest_numbers)
# save the matrix
nb2INLA("maps/map_1.adj", nb)
g <- inla.read.graph(filename = "maps/map_1.adj")
# specify the model formula
# we will start with demographic variables and pop/urban density
formula_1 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_2 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)

res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
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
    sum(log(res_1$cpo$cpo)), sum(log(res_2$cpo$cpo))
  )
)
results <- c(results, list(res_1 = perf))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
# now models with the mobility variables
formula_3 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_4 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex +  + unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
# now models with the mobility variables
formula_5 <- value ~
  # add the demographic vars and pop density
  unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_6 <- value ~
  # add the demographic vars and pop density
  unemp_tot + unemp_immg +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)


res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
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
    sum(log(res_3$cpo$cpo)), sum(log(res_4$cpo$cpo)),
    sum(log(res_5$cpo$cpo)), sum(log(res_6$cpo$cpo))
  )
)
results <- c(results, list(res_2 = perf))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
# now models with the infrastructure variables
formula_7 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_8 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_9 <- value ~
  immigrants_total + immigrants_norge + immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_10 <- value ~
  immigrants_total + immigrants_norge + immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)

res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
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
    sum(log(res_7$cpo$cpo)), sum(log(res_8$cpo$cpo)),
    sum(log(res_9$cpo$cpo)), sum(log(res_10$cpo$cpo))
  )
)
results <- c(results, list(res_3 = perf))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
# now models with all the variables
formula_11 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_12 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_13 <- value ~
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_14 <- value ~
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_14 <- inla(
  formula_14,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
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
    sum(log(res_11$cpo$cpo)), sum(log(res_12$cpo$cpo)),
    sum(log(res_13$cpo$cpo)), sum(log(res_14$cpo$cpo))
  )
)
results <- c(results, list(res_4 = perf))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
########################################################
# Now with variable selection
formula_15 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_16 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_17 <- value ~
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_18 <- value ~
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)

res_15 <- inla(
  formula_15,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_16 <- inla(
  formula_16,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_17 <- inla(
  formula_17,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_18 <- inla(
  formula_18,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
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
    sum(log(res_15$cpo$cpo)), sum(log(res_16$cpo$cpo)),
    sum(log(res_17$cpo$cpo)), sum(log(res_18$cpo$cpo))
  )
)
results <- c(results, list(res_5 = perf))
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
# now models with all the variables
formula_19 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_20 <- value ~
  pop_dens + urb_dens + sex +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)


res_19 <- inla(
  formula_19,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_20 <- inla(
  formula_20,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


models <- c(models, list(res_19, res_20))

perf <- list(
  dic = c(
    res_19$dic$dic, res_20$dic$dic
  ),
  waic = c(
    res_19$waic$waic, res_20$waic$waic
  ),
  cpo = c(
    sum(log(res_19$cpo$cpo)), sum(log(res_20$cpo$cpo))
  )
)
results <- c(results, list(res_6 = perf))

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))

# now models with all the variables
formula_21 <- value ~
  pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  toilet + hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  banks + kindergarten + bakeries + gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_22 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + marketplace + entertainment + sport + clinic +
  toilet + hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  banks + kindergarten + bakeries + gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)
formula_23 <- value ~
  marketplace + entertainment + sport + clinic +
  toilet + hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  banks + kindergarten + bakeries + gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_24 <- value ~
  # add the demographic vars and pop density
  marketplace + entertainment + sport + clinic +
  toilet + hairdresser + shops + place_of_worship + retail + nursing_home +
  restaurant + aerodrome + office + platform + schools + higher_education +
  banks + kindergarten + bakeries + gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)


res_21 <- inla(
  formula_21,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_22 <- inla(
  formula_22,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_23 <- inla(
  formula_23,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


res_24 <- inla(
  formula_24,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


models <- c(models, list(res_21, res_22, res_23, res_24))

perf <- list(
  dic = c(
    res_21$dic$dic, res_22$dic$dic,
    res_23$dic$dic, res_24$dic$dic
  ),
  waic = c(
    res_21$waic$waic, res_22$waic$waic,
    res_23$waic$waic, res_24$waic$waic
  ),
  cpo = c(
    sum(log(res_21$cpo$cpo)), sum(log(res_22$cpo$cpo)),
    sum(log(res_23$cpo$cpo)), sum(log(res_24$cpo$cpo))
  )
)
results <- c(results, list(res_7 = perf))

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))

# now models with all the variables
# now models with all the variables
formula_25 <- value ~
  marketplace + entertainment + clinic + toilet + hairdresser +
  place_of_worship + retail + nursing_home + restaurant + terminal +
  platform + kindergarten + schools + bakeries + gas + banks + atm + pop_dens +
  higher_education +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_26 <- value ~
  # add the demographic vars and pop density
  marketplace + entertainment + clinic + toilet + hairdresser +
  place_of_worship + retail + nursing_home + restaurant + terminal +
  platform + kindergarten + schools + bakeries + gas + banks + atm + pop_dens +
  higher_education +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)

res_25 <- inla(
  formula_25,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_26 <- inla(
  formula_26,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


models <- c(models, list(res_25, res_26))

perf <- list(
  dic = c(
    res_25$dic$dic, res_26$dic$dic
  ),
  waic = c(
    res_25$waic$waic, res_26$waic$waic
  ),
  cpo = c(
    sum(log(res_25$cpo$cpo)), sum(log(res_26$cpo$cpo))
  )
)
results <- c(results, list(res_8 = perf))

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
formula_27 <- value ~
  pop_dens + urb_dens + sex + median_age +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure + entertainment + sport + clinic + toilet +
  hairdresser + shops + place_of_worship + nursing_home + aerodrome + platform +
  kindergarten + schools + bakeries + gas + banks + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_1)
formula_28 <- value ~
  pop_dens + urb_dens + sex + median_age +
  # add the demographic vars and pop density
  workers_ft_com + workers_pt_com + mining_ft_com + mining_pt_com +
  construction_ft_com + construction_pt_com +
  median_age + unemp_tot + unemp_immg + immigrants_total + immigrants_norge +
  immigrants_pure + entertainment + sport + clinic + toilet +
  hairdresser + shops + place_of_worship + nursing_home + aerodrome + platform +
  kindergarten + schools + bakeries + gas + banks + 
# specify the model with neighborhood matrix
f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior_2)

res_27 <- inla(
  formula_27,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_28 <- inla(
  formula_28,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)


models <- c(models, list(res_27, res_28))

perf <- list(
  dic = c(
    res_27$dic$dic, res_28$dic$dic
  ),
  waic = c(
    res_27$waic$waic, res_28$waic$waic
  ),
  cpo = c(
    sum(log(res_27$cpo$cpo)), sum(log(res_28$cpo$cpo))
  )
)
results <- c(results, list(res_9 = perf))

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "results")))
# now models with all the variables
models_final <- list(models, results)
save(models_final, file = "models/bym2_norway.Rda")

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
