ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Data Explorer",
        tabName = "data_explorer",
        startExpanded = TRUE,
        menuSubItem(
          "Norway",
          tabName = "de_norway"
        ),
        menuSubItem(
          "Germany",
          tabName = "de_germany"
        )
      ),
      menuItem(
        text = "Spatial Modelling",
        tabName = "modelling",
        startExpanded = TRUE,
        menuSubItem(
          "Norway",
          tabName = "sm_norway"
        ),
        menuSubItem(
          "Germany",
          tabName = "sm_germany"
        )
      )
    )
  ),
  body = dashboardBody(
    shinyDashboardThemes(
      "grey_dark"
    ),
    tags$head(
      tags$style(
        css
      )
    ),
    tabItems(
      tabItem(
        tabName = "de_norway",
        column(
          width = 9,
          mapdeckOutput(outputId = "map_norway", height = "60vh"),
          br(),
          column(
            width = 5,
            highchartOutput(outputId = "highchart_norway_1", height = "30vh")
          ),
          column(
            width = 7,
            highchartOutput(outputId = "highchart_norway_2", height = "30vh")
          )
        ),
        column(
          width = 3,
          uiOutput("dropdown_ui_norway"),
          pickerInput(
            inputId = "map_type_norway",
            label = "Select map type",
            choices = c("Hexagon Map", "Heatmap", "Choropleth")
          ),
          pickerInput(
            inputId = "map_style_norway",
            label = "Select base map",
            choices = c(
              "dark", "light", "outdoors", "satellite"
            )
          ),
          pickerInput(
            inputId = "inf_numbers_norway",
            label = "Select information",
            choices = c(
              "Daily number of infections",
              "Daily number of infections per 100k",
              "Total number of infections",
              "Total number of infections per 100k",
              "Seven day incidence"
            ),
            selected = "Seven day incidence"
          ),
          pickerInput(
            inputId = "municipality_norway",
            label = "Select municipality",
            choices = sort(unique(norway_munc_conf_long$kommune_name)),
            selected = "Trondheim"
          )
        )
      ),
      tabItem(
        tabName = "de_germany",
        column(
          width = 9,
          mapdeckOutput(outputId = "map_germany", height = "60vh"),
          br(),
          column(
            width = 5,
            highchartOutput(outputId = "highchart_germany_1", height = "30vh")
          ),
          column(
            width = 7,
            highchartOutput(outputId = "highchart_germany_2", height = "30vh")
          )
        ),
        column(
          width = 3,
          uiOutput("dropdown_ui_germany"),
          pickerInput(
            inputId = "map_type_germany",
            label = "Select map type",
            choices = c("Hexagon Map", "Heatmap", "Choropleth")
          ),
          pickerInput(
            inputId = "map_style_germany",
            label = "Select base map",
            choices = c(
              "dark", "light", "outdoors", "satellite"
            )
          ),
          pickerInput(
            inputId = "inf_numbers_germany",
            label = "Select information",
            choices = c(
              "Daily number of infections",
              "Daily number of infections per 100k",
              "Total number of infections",
              "Total number of infections per 100k",
              "Seven day incidence"
            ),
            selected = "Seven day incidence"
          ),
          pickerInput(
            inputId = "municipality_germany",
            label = "Select municipality",
            choices = sort(unique(germany_munc_long$Landkreis)),
            selected = "LK Rosenheim"
          )
        )
      ),
      tabItem(
        tabName = "sm_norway",
        multiInput(
          inputId = "multi_norway",
          label = "Select covariates",
          choices = c(
            "Median age",
            "Total unemployment",
            "Unemployed immigrants",
            "Full-time workers",
            "Part-time workers",
            "Total number of immigrants",
            "Marketplaces",
            "Entertainment venues",
            "Sports facilities",
            "Clinics",
            "Hairdressers",
            "Shops",
            "Places of worship",
            "Retail stores",
            "Nursing homes",
            "Restaurants",
            "Aerodromes",
            "Offices",
            "Public transport platforms",
            "Kindergartens",
            "Schools",
            "Bakeries",
            "Higher education",
            "Population density",
            "Urban density",
            "Female to male ratio"
          )
        )
      )
    )
  ),
  title = "Covid or sth.",
  skin = "black"
)
