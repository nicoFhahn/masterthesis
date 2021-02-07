library(ggplot2)
library(ggregplot)
library(INLA)
library(leaflet)
library(spdep)
source("R/preprocess_norge.R")
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
)#
# create the neighbordhood matrix
nb <- poly2nb(newest_numbers)
# save the matrix
nb2INLA("maps/map_1.adj", nb)
g <- inla.read.graph(filename = "maps/map_1.adj")
# specify the model formula
# we will start with demographic variables and pop/urban density
formula_1 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) +
  # random effects term
  f(idarea_2, model = "iid")
formula_2 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2) +
  # random effects term
  f(idarea_2, model = "iid")


res_1 <- inla(
  formula_1,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_1$dic$dic
res_2$dic$dic
res_2$summary.fixed
head(res_2$summary.fitted.values[order(res_2$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(1, 265, 117, 115, 96, 95), ]
results_frame <- newest_numbers
results_frame$rr <- res_2$summary.fitted.values$mean
results_frame$q025 <- res_2$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_2$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_2$summary.fitted.values$`0.975quant`
range(results_frame$rr)
rc1 <- colorRampPalette(
  c(
    "#86e7b8",
    "#93ff96",
    "#b2ffa8",
    "#d0ffb7",
    "#f2f5de",
    "white"
  ),
  space = "Lab"
)(10)
rc2 <- colorRampPalette(
  c(
    "white",
    "#fae0e4",
    "#f7cad0",
    "#f9bec7",
    "#fbb1bd",
    "#ff99ac",
    "#ff85a1",
    "#ff7096",
    "#ff5c8a",
    "#ff477e",
    "#ff0a54"
  ),
  space = "Lab"
)(30)
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal,
    values = ~rr,
    title = "RR",
    group = "RR"
  )


rm(list=setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with the mobility variables
formula_3 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + groc_pha + parks + resident +
  ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) +
  # random effects term
  f(idarea_2, model = "iid")
formula_4 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + groc_pha + parks + resident +
  ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2) +
  # random effects term
  f(idarea_2, model = "iid")

res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
res_3$dic$dic
res_4$dic$dic
head(res_4$summary.fitted.values[order(res_4$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(265, 174, 117, 1, 95, 94), ]
results_frame <- newest_numbers
results_frame$rr <- res_4$summary.fitted.values$mean
results_frame$q025 <- res_4$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_4$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_4$summary.fitted.values$`0.975quant`
range(results_frame$rr)
rc1 <- colorRampPalette(
  c(
    "#86e7b8",
    "#93ff96",
    "#b2ffa8",
    "#d0ffb7",
    "#f2f5de",
    "white"
  ),
  space = "Lab"
)(10)
rc2 <- colorRampPalette(
  c(
    "white",
    "#fae0e4",
    "#f7cad0",
    "#f9bec7",
    "#fbb1bd",
    "#ff99ac",
    "#ff85a1",
    "#ff7096",
    "#ff5c8a",
    "#ff477e",
    "#ff0a54"
  ),
  space = "Lab"
)(42)
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Mobility grocery & pharmacy: ", results_frame$groc_pha, "<br>",
      "Mobility parks: ", results_frame$parks, "<br>",
      "Mobility residential: ", results_frame$resident, "<br>",
      "Mobility retail & recreation: ", results_frame$ret_recr, "<br>",
      "Mobility transit: ", results_frame$transit, "<br>",
      "Mobility workplace: ", results_frame$workplace, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal,
    values = ~rr,
    title = "RR",
    group = "RR"
  )

rm(list=setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with the infrastructure variables
formula_5 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + shops + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + office + platform +
  higher_educ + kindergarten + schools + bakeries + gas + banks + atm +
  marketplace + entertainment + sport + clinic + toilet + hairdresser +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) +
  # random effects term
  f(idarea_2, model = "iid")
formula_6 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + shops + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + office + platform +
  higher_educ + kindergarten + schools + bakeries + gas + banks + atm +
  marketplace + entertainment + sport + clinic + toilet + hairdresser +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2) +
  # random effects term
  f(idarea_2, model = "iid")

res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_6 <- inla(
  formula_6,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
res_5$dic$dic
res_6$dic$dic
head(res_6$summary.fitted.values[order(res_6$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(265, 174, 117, 1, 95, 94), ]
results_frame <- newest_numbers
results_frame$rr <- res_6$summary.fitted.values$mean
results_frame$q025 <- res_6$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_6$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_6$summary.fitted.values$`0.975quant`
range(results_frame$rr)
rc1 <- colorRampPalette(
  c(
    "#86e7b8",
    "#93ff96",
    "#b2ffa8",
    "#d0ffb7",
    "#f2f5de",
    "white"
  ),
  space = "Lab"
)(10)
rc2 <- colorRampPalette(
  c(
    "white",
    "#fae0e4",
    "#f7cad0",
    "#f9bec7",
    "#fbb1bd",
    "#ff99ac",
    "#ff85a1",
    "#ff7096",
    "#ff5c8a",
    "#ff477e",
    "#ff0a54"
  ),
  space = "Lab"
)(42)
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of aerodromes: ", results_frame$aerodrome, "<br>",
      "Number of gas stations: ", results_frame$gas, "<br>",
      "Number of bakeries: ", results_frame$bakeries, "<br>",
      "Number of higher educational buildings: ", results_frame$higher_educ, "<br>",
      "Number of restaurants: ", results_frame$restaurant, "<br>",
      "Number of retail buildings: ", results_frame$retail, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal,
    values = ~rr,
    title = "RR",
    group = "RR"
  )

rm(list=setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with all the variables
formula_7 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + shops + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + office + platform +
  higher_educ + kindergarten + schools + bakeries + gas + banks + atm +
  marketplace + entertainment + sport + clinic + toilet + hairdresser +
  groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) +
  # random effects term
  f(idarea_2, model = "iid")
formula_8 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + shops + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + office + platform +
  higher_educ + kindergarten + schools + bakeries + gas + banks + atm +
  marketplace + entertainment + sport + clinic + toilet + hairdresser +
  groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2) +
  # random effects term
  f(idarea_2, model = "iid")

res_7 <- inla(
  formula_7,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_8 <- inla(
  formula_8,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
res_7$dic$dic
res_8$dic$dic
head(res_8$summary.fitted.values[order(res_8$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(265, 174, 117, 1, 95, 94), ]
results_frame <- newest_numbers
results_frame$rr <- res_8$summary.fitted.values$mean
results_frame$q025 <- res_8$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_8$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_8$summary.fitted.values$`0.975quant`
range(results_frame$rr)
rc1 <- colorRampPalette(
  c(
    "#86e7b8",
    "#93ff96",
    "#b2ffa8",
    "#d0ffb7",
    "#f2f5de",
    "white"
  ),
  space = "Lab"
)(10)
rc2 <- colorRampPalette(
  c(
    "white",
    "#fae0e4",
    "#f7cad0",
    "#f9bec7",
    "#fbb1bd",
    "#ff99ac",
    "#ff85a1",
    "#ff7096",
    "#ff5c8a",
    "#ff477e",
    "#ff0a54"
  ),
  space = "Lab"
)(42)
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of aerodromes: ", results_frame$aerodrome, "<br>",
      "Number of gas stations: ", results_frame$gas, "<br>",
      "Number of bakeries: ", results_frame$bakeries, "<br>",
      "Number of banks: ", results_frame$banks, "<br>",
      "Mobility workplace: ", results_frame$workplace, "<br>",
      "Mobility groceries & pharmacies: ", results_frame$groc_pha, "<br>",
      "Mobility retail & recreation: ", results_frame$ret_recr, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal,
    values = ~rr,
    title = "RR",
    group = "RR"
  )
########################################################
# Now with variable selection
# data(Epil)
# stack <- INLA::inla.stack(data = list(y = Epil$y),
#                           A = list(1),
#                             effects = list(data.frame(Intercept = 1, Epil[3:5])))
covariates <- newest_numbers[, c(9:14, 24:49, 57:59)]
covariates$geometry <- NULL
stack <- inla.stack(
  data = list(
    value = newest_numbers$value
  ),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      covariates
    )
  )
)

result_step <- INLAstep(
  fam1 = "nbinomial", 
  as.data.frame(newest_numbers),
  in_stack = stack,
  invariant = "0 + Intercept",
  direction = 'backwards',
  include = c(9:14, 24:49, 57:59),
  y = 'value', 
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)


autoplot(result$best_model, which = c(1, 5), CI = TRUE)