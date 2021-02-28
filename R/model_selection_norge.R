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
sel <- INLAModelSel(
  "value",
  colnames(newest_numbers)[c(9:15, 23, 24:43, 45:62, 65:70, 72, 80:82)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
