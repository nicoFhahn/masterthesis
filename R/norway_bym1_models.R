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
)
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
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1)
formula_2 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2)


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
results_frame <- newest_numbers
dics <- c(res_1$dic$dic, res_2$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_1$summary.fitted.values
} else {
  sfv <- res_2$summary.fitted.values
}
results_frame$rr <- sfv$mean
results_frame$q025 <- sfv$`0.025quant`
results_frame$q5 <- sfv$`0.5quant`
results_frame$q975 <- sfv$`0.975quant`
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
)(round(10 * range(results_frame$rr)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
map <- leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(rr),
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
      "Unemployment %: ", results_frame$unemp_tot, "<br>",
      "Immigrants %: ", results_frame$median_age, "<br>",
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
    title = htmltools::HTML(
      paste(
        "RR<br><span style='font-size:0.8em'>DIC:",
        round(min(dics)),
        "</span>"
      )
    ),
    group = "RR"
  )
saveWidget(
  map,
  paste(
    "html_plots/norway_bym1_model_",
    which(dics %in% min(dics)),
    ".html",
    sep = ""
  )
)

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with the mobility variables
formula_3 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total +
  groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1)
formula_4 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total +
  groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2)

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
results_frame <- newest_numbers
dics <- c(res_3$dic$dic, res_4$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_3$summary.fitted.values
} else {
  sfv <- res_4$summary.fitted.values
}
results_frame$rr <- sfv$mean
results_frame$q025 <- sfv$`0.025quant`
results_frame$q5 <- sfv$`0.5quant`
results_frame$q975 <- sfv$`0.975quant`
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
)(round(10 * range(results_frame$rr)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
map <- leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(rr),
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
      "Unemployment %: ", results_frame$unemp_tot, "<br>",
      "Immigrants %: ", results_frame$median_age, "<br>",
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
    title = htmltools::HTML(
      paste(
        "RR<br><span style='font-size:0.8em'>DIC:",
        round(min(dics)),
        "</span>"
      )
    ),
    group = "RR"
  )
saveWidget(
  map,
  paste(
    "html_plots/norway_bym1_model_",
    which(dics %in% min(dics)) + 2,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with the infrastructure variables
formula_5 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total + 
  shops + place_of_worship + retail + nursing_home + restaurant + aerodrome +
  office + platform + higher_educ + kindergarten + schools + bakeries + gas +
  banks + atm + marketplace + entertainment + sport + clinic + toilet +
  hairdresser +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1)
formula_6 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total + 
  shops + place_of_worship + retail + nursing_home + restaurant + aerodrome +
  office + platform + higher_educ + kindergarten + schools + bakeries + gas +
  banks + atm + marketplace + entertainment + sport + clinic + toilet +
  hairdresser +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2)

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
results_frame <- newest_numbers
dics <- c(res_5$dic$dic, res_6$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_5$summary.fitted.values
} else {
  sfv <- res_6$summary.fitted.values
}
results_frame$rr <- sfv$mean
results_frame$q025 <- sfv$`0.025quant`
results_frame$q5 <- sfv$`0.5quant`
results_frame$q975 <- sfv$`0.975quant`
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
)(round(10 * range(results_frame$rr)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
map <- leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(rr),
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
      "Unemployment %: ", results_frame$unemp_tot, "<br>",
      "Immigrants %: ", results_frame$median_age, "<br>",
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
    title = htmltools::HTML(
      paste(
        "RR<br><span style='font-size:0.8em'>DIC:",
        round(min(dics)),
        "</span>"
      )
    ),
    group = "RR"
  )
saveWidget(
  map,
  paste(
    "html_plots/norway_bym1_model_",
    which(dics %in% min(dics)) + 4,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with all the variables
formula_7 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total + 
  shops + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + office + platform +
  higher_educ + kindergarten + schools + bakeries + gas + banks + atm +
  marketplace + entertainment + sport + clinic + toilet + hairdresser +
  groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1)
formula_8 <- value ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total +
  shops + place_of_worship +
  retail + nursing_home + restaurant + aerodrome + office + platform +
  higher_educ + kindergarten + schools + bakeries + gas + banks + atm +
  marketplace + entertainment + sport + clinic + toilet + hairdresser +
  groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2)
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
results_frame <- newest_numbers
dics <- c(res_7$dic$dic, res_8$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_7$summary.fitted.values
} else {
  sfv <- res_8$summary.fitted.values
}
results_frame$rr <- sfv$mean
results_frame$q025 <- sfv$`0.025quant`
results_frame$q5 <- sfv$`0.5quant`
results_frame$q975 <- sfv$`0.975quant`
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
)(round(10 * range(results_frame$rr)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
map <- leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(rr),
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
      "Unemployment %: ", results_frame$unemp_tot, "<br>",
      "Immigrants %: ", results_frame$median_age, "<br>",
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
    title = htmltools::HTML(
      paste(
        "RR<br><span style='font-size:0.8em'>DIC:",
        round(min(dics)),
        "</span>"
      )
    ),
    group = "RR"
  )
saveWidget(
  map,
  paste(
    "html_plots/norway_bym1_model_",
    which(dics %in% min(dics)) + 6,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
########################################################
# Now with variable selection
formula_9 <- value ~
# add the demographic vars and pop density
  groc_pha + parks + resident + ret_recr + transit + workplace + median_age +
  marketplace + sport + clinic + toilet + retail + nursing_home + restaurant +
  aerodrome + office + platform + kindergarten + schools + bakeries +
  gas + banks + atm + 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1)
formula_10 <- value ~
# add the demographic vars and pop density
  groc_pha + parks + resident + ret_recr + transit + workplace + median_age +
  marketplace + sport + clinic + toilet + retail + nursing_home + restaurant +
  aerodrome + office + platform + kindergarten + schools + bakeries +
  gas + banks + atm + 
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + median_age + unemp_tot + immigrants_total + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2)

res_9 <- inla(
  formula_9,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_10 <- inla(
  formula_10,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
results_frame <- newest_numbers
dics <- c(res_9$dic$dic, res_10$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_9$summary.fitted.values
} else {
  sfv <- res_10$summary.fitted.values
}
results_frame$rr <- sfv$mean
results_frame$q025 <- sfv$`0.025quant`
results_frame$q5 <- sfv$`0.5quant`
results_frame$q975 <- sfv$`0.975quant`
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
)(round(10 * range(results_frame$rr)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
map <- leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(rr),
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
      "Unemployment %: ", results_frame$unemp_tot, "<br>",
      "Immigrants %: ", results_frame$median_age, "<br>",
      "Number of marketplace: ", results_frame$marketplace, "<br>",
      "Number of gas stations: ", results_frame$gas, "<br>",
      "Number of bakeries: ", results_frame$bakeries, "<br>",
      "Number of banks: ", results_frame$banks, "<br>",
      "Mobility residential: ", results_frame$resident, "<br>",
      "Mobility workplace: ", results_frame$workplace, "<br>",
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
    title = htmltools::HTML(
      paste(
        "RR<br><span style='font-size:0.8em'>DIC:",
        round(min(dics)),
        "</span>"
      )
    ),
    group = "RR"
  )
saveWidget(
  map,
  paste(
    "html_plots/norway_bym1_model_",
    which(dics %in% min(dics)) + 8,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g")))
# now models with all the variables
formula_11 <- value ~
# add the demographic vars and pop density
shops + retail + clinic + schools + banks + pop_dens + office + atm +
  place_of_worship + restaurant + sport + hairdresser + gas +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) 
formula_12 <- value ~
# add the demographic vars and pop density
shops + retail + clinic + schools + banks + pop_dens + office + atm +
  place_of_worship + restaurant + sport + hairdresser + gas +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2) 
formula_13 <- value ~
# add the demographic vars and pop density
shops + retail + clinic + schools + banks + pop_dens + office + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) 
formula_14 <- value ~
# add the demographic vars and pop density
shops + retail + clinic + schools + banks + pop_dens + office + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2) 
formula_15 <- value ~
# add the demographic vars and pop density
shops + retail + clinic + schools + banks +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_1) 
formula_16 <- value ~
# add the demographic vars and pop density
shops + retail + clinic + schools + banks +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_2)

res_11 <- inla(
  formula_11,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_12 <- inla(
  formula_12,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_13 <- inla(
  formula_13,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_14 <- inla(
  formula_14,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_15 <- inla(
  formula_15,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

res_16 <- inla(
  formula_16,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

results_frame <- newest_numbers
dics <- c(
  res_11$dic$dic, res_12$dic$dic,
  res_14$dic$dic, res_15$dic$dic,
  res_13$dic$dic, res_16$dic$dic
)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_11$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_12$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_13$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_14$summary.fitted.values
} else if (dics[5] == min(dics)) {
  sfv <- res_15$summary.fitted.values
} else if (dics[6] == min(dics)) {
  sfv <- res_16$summary.fitted.values
}
results_frame$rr <- sfv$mean
results_frame$q025 <- sfv$`0.025quant`
results_frame$q5 <- sfv$`0.5quant`
results_frame$q975 <- sfv$`0.975quant`
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
)(round(10 * range(results_frame$rr)[2] - 10))
pal <- colorNumeric(
  c(rc1, rc2),
  domain = results_frame$rr
)
map <- leaflet(results_frame) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(rr),
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
      "Number of gas stations: ", results_frame$gas, "<br>",
      "Number of retail stores: ", results_frame$retail, "<br>",
      "Number of hairdresser: ", results_frame$hairdresser, "<br>",
      "Number of clinic: ", results_frame$clinic, "<br>",
      "Number of shops: ", results_frame$shops, "<br>",
      "Number of places of worship: ", results_frame$place_of_worship, "<br>",
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
    title = htmltools::HTML(
      paste(
        "RR<br><span style='font-size:0.8em'>DIC:",
        round(min(dics)),
        "</span>"
      )
    ),
    group = "RR"
  )
saveWidget(
  map,
  paste(
    "html_plots/norway_bym1_model_",
    which(dics %in% min(dics)) + 10,
    ".html",
    sep = ""
  )
)
