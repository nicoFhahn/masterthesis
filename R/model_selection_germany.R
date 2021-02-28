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
sel <- INLAModelSel(
  "CumNumberTestedIll",
  colnames(newest_numbers)[c(2, 5, 6, 8:17, 22, 23, 37, 42:56, 59:66, 73:76)],
  "idarea_1",
  "iid",
  "nbinomial",
  newest_numbers
)
