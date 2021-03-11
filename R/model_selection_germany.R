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
start <- Sys.time()
source("R/preprocess_germany.R")
start <- Sys.time()                                 
parallelMap::parallelStartSocket(7)
newest_numbers$geometry <- NULL
stack_all <- inla.stack(
  data = list(CumNumberTestedIll = newest_numbers$CumNumberTestedIll),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(2:5, 8:18, 26:40, 43:48, 56:59)]
    )
  )
)
stack_demo <- inla.stack(
  data = list(CumNumberTestedIll = newest_numbers$CumNumberTestedIll),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(2:5, 8:18, 56:58)]
    )
  )
)
stack_infra <- inla.stack(
  data = list(CumNumberTestedIll = newest_numbers$CumNumberTestedIll),
  A = list(1),
  effects = list(
    data.frame(
      Intercept = 1,
      newest_numbers[, c(26:40, 43:48, 56:57, 59)]
    )
  )
)
result_all <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_all,
  invariant = "0 + Intercept",
  direction = "backwards",
  include = c(2:5, 8:18, 26:40, 43:48, 56:58, 60),
  y = "CumNumberTestedIll",
  y2 = "CumNumberTestedIll",
  powerl = 1,
  inter = 1,
  thresh = 2
)
result_demo <- INLAstep(
  fam1 = "nbinomial",
  newest_numbers,
  in_stack = stack_demo,
  invariant = "0 + Intercept",
  direction = "backwards",
  include = c(2:5, 8:18, 56:58),
  y = "CumNumberTestedIll",
  y2 = "CumNumberTestedIll",
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
  include = c(26:40, 43:48, 56:57, 60),
  y = "CumNumberTestedIll",
  y2 = "CumNumberTestedIll",
  powerl = 1,
  inter = 1,
  thresh = 2
)
set.seed(420)
sel_all <- INLAModelSel(
  "CumNumberTestedIll",
  colnames(newest_numbers)[c(2:5, 8:18, 26:40, 43:48, 56:58, 60)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
sel_demo <- INLAModelSel(
  "CumNumberTestedIll",
  colnames(newest_numbers)[c(2:5, 8:18, 56:58)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
sel_infra <- INLAModelSel(
  "CumNumberTestedIll",
  colnames(newest_numbers)[c(26:40, 43:48, 56:57, 60)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
parallelMap::parallelStop()
Sys.time() - start
sel$AllModels <- NULL
sel$FormulaList <- NULL
save(sel, file = "sel_germany.Rda")
