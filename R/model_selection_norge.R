library(ggplot2)
library(ggregplot)
library(htmlwidgets)
library(INLA)
library(INLAutils)
library(leaflet)
library(leaflet.mapboxgl)
library(mlr)
library(randomForestSRC)
library(spdep)
source("R/preprocess_norge.R")
parallelMap::parallelStartSocket(7)
newest_numbers$geometry <- NULL
stack_all <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(7:32, 35:39, 41, 48:50)]
    )
  )
)
stack_demo <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(7:18, 48:50)]
    )
  )
)
stack_infra <- inla.stack(
  data = list(value = newest_numbers$value),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(19:32, 35:39, 48, 49)]
    )
  )
)
result_all <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "0 + Intercept",
  direction = "backwards",
  include = c(7:32, 35:39, 41, 48:50),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2,
  num.threads = 7
)
result_demo <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_demo,
  invariant = "0 + Intercept",
  direction = "backwards",
  include = c(7:18, 48:50),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2
)
result_infra <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_infra,
  invariant = "0 + Intercept",
  direction = "backwards",
  include = c(19:32, 35:39, 48, 49),
  y = "value",
  y2 = "value",
  powerl = 1,
  inter = 1,
  thresh = 2
)
set.seed(420)
sel_all <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(7:32, 35:39, 41, 48:50)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
sel_demo <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(7:18, 48:50)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
sel_infra <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(19:32, 35:39, 48, 49)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
