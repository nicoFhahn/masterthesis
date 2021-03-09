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
source("R/preprocess_germany.R")
parallelMap::parallelStartSocket(6)
set.seed(420)
sel <- INLAModelSel(
  "CumNumberTestedIll",
  colnames(newest_numbers)[c(2:5, 8:18, 26:40, 43:48, 56:58, 60)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
