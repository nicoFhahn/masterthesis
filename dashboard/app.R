library(covid19germany)
library(covidregionaldata)
library(data.table)
library(dashboardthemes)
library(dplyr)
library(DT)
library(eurostat)
library(highcharter)
library(INLA)
library(ISOcodes)
library(LaCroixColoR)
library(mapdeck)
library(readr)
library(reshape2)
library(sass)
library(sf)
library(shiny)
library(shinybusy)
library(shinydashboard)
library(shinyWidgets)
library(SpatialEpi)
library(spdep)
library(stringr)
library(tibble)
library(waiter)
local <- TRUE
if (!local) {
  source(file.path("server", "scripts/pp_norway.R"), local = TRUE)$value
} else {
  norway_munc_conf_long <- read_csv("server/data/norway_munc_conf_long.csv")
}
css <- sass(sass_file("www/styles.scss"))
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  if (!local) {
    source(file.path("server", "scripts/pp_germany.R"), local = TRUE)$value
    source(file.path("server", "scripts/pp_europe.R"), local = TRUE)$value
  } else {
    load("local_files.RData")
    ts_europe <- ts_europe_unscaled
    geom <- ts_europe$geometry
    ts_europe$geometry <- NULL
    ts_europe[, c(7:24, 27, 38, 39)] <- scale(ts_europe[, c(7:24, 27, 38, 39)])
    ts_europe$geometry <- geom
    rm(geom)
    ts_europe <- st_as_sf(ts_europe)
  }
  pois_norway <- read_csv("server/data/norge_all.csv")
  pois_germany <- read_csv("server/data/germany_all.csv")
  token <- "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  norway_sf <- read_sf("server/data/shapes_norge.shp")
  germany_sf <- read_sf("server/data/shapes_germany.shp")
  nb <- poly2nb(newest_numbers_norway)
  nb2INLA("server/data/map_1.adj", nb)
  g_norway <- inla.read.graph(filename = "server/data/map_1.adj")
  nb <- poly2nb(newest_numbers_germany)
  nb2INLA("server/data/map_2.adj", nb)
  g_germany <- inla.read.graph(filename = "server/data/map_2.adj")
  source(file.path("server", "scripts/reactive_values.R"), local = TRUE)$value
  source(file.path("server", "scripts/observer.R"), local = TRUE)$value
  source(file.path("server", "scripts/ui_outputs.R"), local = TRUE)$value
  source(file.path("server", "scripts/maps.R"), local = TRUE)$value
  source(file.path("server", "scripts/highcharts.R"), local = TRUE)$value
  source(file.path("server", "scripts/datatables.R"), local = TRUE)$value
  waiter_hide() # hide the waiter
}

# Run the application
shinyApp(
  ui = ui,
  server = server
)
