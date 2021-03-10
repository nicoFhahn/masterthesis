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
