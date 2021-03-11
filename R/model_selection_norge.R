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
parallelMap::parallelStartSocket(6)
set.seed(420)
sel_all <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(7:51, 53, 61:63)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
sel_demo <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(7:30, 61:63)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
sel_infra <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(37:51, 53, 61, 62)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)

