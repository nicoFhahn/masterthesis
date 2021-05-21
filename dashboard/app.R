library(shiny)
library(sass)
library(headspace)
library(dashboardthemes)
library(shinydashboard)
library(mapdeck)
library(readr)
library(data.table)
library(sf)
library(SpatialEpi)
library(shinyWidgets)
library(LaCroixColoR)
library(highcharter)
library(stringr)
library(INLA)
source(file.path("server", "scripts/pp_norway.R"), local = TRUE)$value
source(file.path("server", "scripts/pp_germany.R"), local = TRUE)$value
pois_norway <- read_csv("server/data/norge_all.csv")
pois_germany <- read_csv("server/data/germany_all.csv")
token <- "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
norway_sf <- read_sf("server/data/shapes_norge.shp")
germany_sf <- read_sf("server/data/shapes_germany.shp")
nb <- poly2nb(newest_numbers_norway)
nb2INLA("server/data/map_1.adj", nb)
g_norway <- inla.read.graph(filename = "server/data/map_1.adj")
css <- sass(sass_file("www/styles.scss"))
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  source(file.path("server", "scripts/reactive_values.R"), local = TRUE)$value
  source(file.path("server", "scripts/observer.R"), local = TRUE)$value
  source(file.path("server", "scripts/ui_outputs.R"), local = TRUE)$value
  source(file.path("server", "scripts/maps.R"), local = TRUE)$value
  source(file.path("server", "scripts/highcharts.R"), local = TRUE)$value
  source(file.path("server", "scripts/datatables.R"), local = TRUE)$value
  # give_headspace()
}

# Run the application
shinyApp(
  ui = ui,
  server = server
)
