library(ggplot2)
library(htmlwidgets)
library(INLA)
library(INLAutils)
library(leaflet)
library(leaflet.mapboxgl)
library(mlr)
library(randomForestSRC)
library(spdep)
source("R/preprocess_germany.R")
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
models <- list()
#
# create the neighbordhood matrix
nb <- poly2nb(newest_numbers)
Q <- Diagonal(x = sapply(nb, length))
for(i in 2:nrow(newest_numbers)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = nrow(newest_numbers)) - Q
# save the matrix
nb2INLA("maps/map_2.adj", nb)
g <- inla.read.graph(filename = "maps/map_2.adj")
# specify the model formula
# we will start with demographic variables and pop/urban density
formula_1 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_2 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_3 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_4 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")


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
models <- c(models, list(res_1, res_2, res_3, res_4))
results_frame <- newest_numbers
dics <- c(res_1$dic$dic, res_2$dic$dic, res_3$dic$dic, res_4$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_1$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_2$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_3$summary.fitted.values
} else if (dics[4] == min(dics)) {
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)),
    ".html",
    sep = ""
  )
)

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with the mobility variables
formula_5 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_6 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_7 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_8 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")


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
models <- c(models, list(res_5, res_6, res_7, res_8))
results_frame <- newest_numbers
dics <- c(res_5$dic$dic, res_6$dic$dic, res_7$dic$dic, res_8$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_5$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_6$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_7$summary.fitted.values
} else if (dics[4] == min(dics)) {
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Unemployed/1000: ", round(results_frame$arbeitslose_insgesamt, 2), "<br>",
      "Asylum seekers/1000: ", round(results_frame$arbeitslose_insgesamt, 2), "<br>",
      "Protection seekers/1000: ", round(results_frame$arbeitslose_insgesamt, 2), "<br>",
      "Welfare recipients/1000: ", round(results_frame$arbeitslose_insgesamt, 2), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 4,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with the infrastructure variables
formula_9 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Gewerbesteuer + einkuenfte_gesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_10 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Gewerbesteuer + einkuenfte_gesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
# now models with the infrastructure variables
formula_11 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Gewerbesteuer + einkuenfte_gesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_12 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Gewerbesteuer + einkuenfte_gesamt +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")

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
models <- c(models, list(res_9, res_10, res_11, res_12))
results_frame <- newest_numbers
dics <- c(res_9$dic$dic, res_10$dic$dic, res_11$dic$dic, res_12$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_9$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_10$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_11$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_12$summary.fitted.values
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Trade tax: ", results_frame$Gewerbesteuer, "<br>",
      "Total income: ", results_frame$einkuenfte_gesamt, "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 8,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with all the variables
formula_13 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_14 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_15 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) + 
  f(idarea_2, model = "iid")
formula_16 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) + 
  f(idarea_2, model = "iid")
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
models <- c(models, list(res_13, res_14, res_15, res_16))
results_frame <- newest_numbers
dics <- c(res_13$dic$dic, res_14$dic$dic, res_15$dic$dic, res_16$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_13$summary.fitted.values
} else  if (dics[2] == min(dics)) {
  sfv <- res_14$summary.fitted.values
} else  if (dics[3] == min(dics)) {
  sfv <- res_15$summary.fitted.values
} else  if (dics[4] == min(dics)) {
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
      "Voter turnout in %: ", results_frame$Wahlbeteiligung, "<br>",
      "Union voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "SPD voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "Greens voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "FDP voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "Die Linke voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "AfD voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "Others voters / 1000: ", round(newest_numbers$Union, 2), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 12,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
########################################################
# Now with variable selection
formula_17 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_18 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2)
formula_19 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_20 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  pop_dens + urb_dens + sex + empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")

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
models <- c(models, list(res_17, res_18, res_19, res_20))
results_frame <- newest_numbers
dics <- c(res_17$dic$dic, res_18$dic$dic, res_19$dic$dic, res_20$dic$dic)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_17$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_18$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_19$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_20$summary.fitted.values
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 16,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with all the variables
formula_21 <- CumNumberTestedIll ~
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) 
formula_22 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) 
formula_23 <- CumNumberTestedIll ~
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_24 <- CumNumberTestedIll ~
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")

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
results_frame <- newest_numbers
dics <- c(
  res_21$dic$dic, res_22$dic$dic,
  res_23$dic$dic, res_24$dic$dic
)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_21$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_22$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_23$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_24$summary.fitted.values
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of gas stations: ", round(results_frame$gas, 3), "<br>",
      "Number of retail stores: ", round(results_frame$retail, 3), "<br>",
      "Number of hairdresser: ", round(results_frame$hairdresser, 3), "<br>",
      "Number of clinic: ", round(results_frame$clinic, 3), "<br>",
      "Number of shops: ", round(results_frame$shops, 3), "<br>",
      "Number of places of worship: ", round(results_frame$place_of_worship, 3), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 20,
    ".html",
    sep = ""
  )
)

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with all the variables
formula_25 <- CumNumberTestedIll ~
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm + empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) 
formula_26 <- CumNumberTestedIll ~
  # add the demographic vars and pop density
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) 
formula_27 <- CumNumberTestedIll ~
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_28 <- CumNumberTestedIll ~
  marketplace + entertainment + sport + clinic + toilet + hairdresser + shops +
  place_of_worship + retail + nursing_home + restaurant + aerodrome + office + 
  university + platform + schools + college + banks + kindergarten + bakeries +
  gas + atm +empfaenger_asylbewerber + Gewerbesteuer +
  einkuenfte_gesamt + Wahlbeteiligung + Union + SPD + Gruene + FDP +
  die_linke + afd + sonstige + schutzsuchende + sozialhilfe_empfaenger + 
  arbeitslose_insgesamt + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")

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
models <- c(models, list(res_25, res_26, res_27, res_28))
results_frame <- newest_numbers
dics <- c(
  res_25$dic$dic, res_26$dic$dic,
  res_27$dic$dic, res_28$dic$dic
)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_25$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_26$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_27$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_28$summary.fitted.values
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of gas stations: ", round(results_frame$gas, 3), "<br>",
      "Number of retail stores: ", round(results_frame$retail, 3), "<br>",
      "Number of hairdresser: ", round(results_frame$hairdresser, 3), "<br>",
      "Number of clinic: ", round(results_frame$clinic, 3), "<br>",
      "Number of shops: ", round(results_frame$shops, 3), "<br>",
      "Number of places of worship: ", round(results_frame$place_of_worship, 3), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 24,
    ".html",
    sep = ""
  )
)

rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with all the variables
formula_29 <- CumNumberTestedIll ~
  Gewerbesteuer + einkuenfte_gesamt + SPD + die_linke + afd + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt + restaurant + banks +
  urb_dens + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_30 <- CumNumberTestedIll ~
  Gewerbesteuer + einkuenfte_gesamt + SPD + die_linke + afd + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt + restaurant + banks +
  urb_dens +  
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) 
formula_31 <- CumNumberTestedIll ~
  Gewerbesteuer + einkuenfte_gesamt + SPD + die_linke + afd + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt + restaurant + banks +
  urb_dens + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_32 <- CumNumberTestedIll ~
  Gewerbesteuer + einkuenfte_gesamt + SPD + die_linke + afd + schutzsuchende +
  sozialhilfe_empfaenger + arbeitslose_insgesamt + restaurant + banks +
  urb_dens + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")

res_29 <- inla(
  formula_29,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_30 <- inla(
  formula_30,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_31 <- inla(
  formula_31,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_32 <- inla(
  formula_32,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
models <- c(models, list(res_29, res_30, res_31, res_32))
results_frame <- newest_numbers
dics <- c(
  res_29$dic$dic, res_30$dic$dic,
  res_31$dic$dic, res_32$dic$dic
)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_29$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_30$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_31$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_32$summary.fitted.values
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of gas stations: ", round(results_frame$gas, 3), "<br>",
      "Number of retail stores: ", round(results_frame$retail, 3), "<br>",
      "Number of hairdresser: ", round(results_frame$hairdresser, 3), "<br>",
      "Number of clinic: ", round(results_frame$clinic, 3), "<br>",
      "Number of shops: ", round(results_frame$shops, 3), "<br>",
      "Number of places of worship: ", round(results_frame$place_of_worship, 3), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 28,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
# now models with all the variables
formula_33 <- CumNumberTestedIll ~
  empfaenger_asylbewerber + Gewerbesteuer + einkuenfte_gesamt + SPD + FDP +
  afd + schutzsuchende + sozialhilfe_empfaenger + arbeitslose_insgesamt +
  arbeitslose_auslaender + restaurant + urb_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1)
formula_34 <- CumNumberTestedIll ~
  empfaenger_asylbewerber + Gewerbesteuer + einkuenfte_gesamt + SPD + FDP +
  afd + schutzsuchende + sozialhilfe_empfaenger + arbeitslose_insgesamt +
  arbeitslose_auslaender + restaurant + urb_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) 
formula_35 <- CumNumberTestedIll ~
  empfaenger_asylbewerber + Gewerbesteuer + einkuenfte_gesamt + SPD + FDP +
  afd + schutzsuchende + sozialhilfe_empfaenger + arbeitslose_insgesamt +
  arbeitslose_auslaender + restaurant + urb_dens +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_1) +
  f(idarea_2, model = "iid")
formula_36 <- CumNumberTestedIll ~
  empfaenger_asylbewerber + Gewerbesteuer + einkuenfte_gesamt + SPD + FDP +
  afd + schutzsuchende + sozialhilfe_empfaenger + arbeitslose_insgesamt +
  arbeitslose_auslaender + restaurant + urb_dens + 
  # specify the model with neighborhood matrix
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior_2) +
  f(idarea_2, model = "iid")

res_33 <- inla(
  formula_33,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_34 <- inla(
  formula_34,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_35 <- inla(
  formula_35,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

res_36 <- inla(
  formula_36,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
models <- c(models, list(res_33, res_34, res_35, res_36))
results_frame <- newest_numbers
dics <- c(
  res_33$dic$dic, res_34$dic$dic,
  res_35$dic$dic, res_36$dic$dic
)
dics[is.nan(dics)] <- 100000
if (dics[1] == min(dics)) {
  sfv <- res_33$summary.fitted.values
} else if (dics[2] == min(dics)) {
  sfv <- res_34$summary.fitted.values
} else if (dics[3] == min(dics)) {
  sfv <- res_35$summary.fitted.values
} else if (dics[4] == min(dics)) {
  sfv <- res_36$summary.fitted.values
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
      "Kommune: ", results_frame$Landkreis, "<br>",
      "Population: ", results_frame$PopulationTotal, "<br>",
      "Population density: ", round(results_frame$pop_dens), "<br>",
      "Urban density: ", round(results_frame$urb_dens, 3), "<br>",
      "Proportion of females: ", round(results_frame$sex, 3), "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of gas stations: ", round(results_frame$gas, 3), "<br>",
      "Number of retail stores: ", round(results_frame$retail, 3), "<br>",
      "Number of hairdresser: ", round(results_frame$hairdresser, 3), "<br>",
      "Number of clinic: ", round(results_frame$clinic, 3), "<br>",
      "Number of shops: ", round(results_frame$shops, 3), "<br>",
      "Number of places of worship: ", round(results_frame$place_of_worship, 3), "<br>",
      "Number of infections: ", results_frame$CumNumberTestedIll, "<br>",
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
    "html_plots/germany_leroux_model_",
    which(dics %in% min(dics)) + 32,
    ".html",
    sep = ""
  )
)
rm(list = setdiff(ls(), c("newest_numbers", "prior_1", "prior_2", "g", "models", "C")))
save(models, file = "models/leroux_germany.Rda")
