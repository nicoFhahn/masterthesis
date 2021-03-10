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
sel <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(17:37, 39:56, 59:64, 66, 74:76)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)

