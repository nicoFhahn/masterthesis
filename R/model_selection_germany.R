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
sel <- INLAModelSel(
  "CumNumberTestedIll",
  colnames(newest_numbers)[c(2, 5, 6, 8:17, 22, 23, 42:56, 59:64, 73:76)],
  Family = "nbinomial",
  Data = newest_numbers
)
parallelMap::parallelStop()
Sys.time() - start
sel$AllModels <- NULL
sel$FormulaList <- NULL
save(sel, file = "sel_germany.Rda")
