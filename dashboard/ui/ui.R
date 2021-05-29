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
        ),
        menuSubItem(
          "Europe",
          tabName = "de_europe"
        )
      ),
      menuItem(
        text = "SIR",
        tabName = "sir",
        startExpanded = TRUE,
        menuSubItem(
          "Norway",
          tabName = "sir_norway"
        ),
        menuSubItem(
          "Germany",
          tabName = "sir_germany"
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
      ),
      menuItem(
        text = "Temporal Modelling",
        tabName = "modelling_temporal"
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
        tabName = "de_europe",
        column(
          width = 9,
          mapdeckOutput(outputId = "map_europe", height = "60vh"),
          br(),
          column(
            width = 12,
            highchartOutput(outputId = "highchart_europe", height = "30vh")
          )
        ),
        column(
          width = 3,
          uiOutput("dropdown_ui_europe"),
          dateInput(
            "date_europe",
            label = "Select date",
            min = min(ts_europe$Date),
            max = max(ts_europe$Date),
            value = max(ts_europe$Date)
          ),
          pickerInput(
            inputId = "map_style_europe",
            label = "Select base map",
            choices = c(
              "dark", "light", "outdoors", "satellite"
            )
          ),
          pickerInput(
            inputId = "inf_numbers_europe",
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
            inputId = "country_europe",
            label = "Select country",
            choices = sort(unique(ts_europe$Country)),
            selected = "Germany"
          )
        )
      ),
      tabItem(
        tabName = "sir_norway",
        column(
          width = 2,
          pickerInput(
            inputId = "map_style_norway_3",
            label = "Select base map",
            choices = c(
              "dark", "light", "outdoors", "satellite"
            )
          )
        ),
        column(
          width = 10,
          mapdeckOutput(
            "sir_map_norway",
            height = "80vh"
          )
        )
      ),
      tabItem(
        tabName = "sir_germany",
        column(
          width = 2,
          pickerInput(
            inputId = "map_style_germany_3",
            label = "Select base map",
            choices = c(
              "dark", "light", "outdoors", "satellite"
            )
          )
        ),
        column(
          width = 10,
          mapdeckOutput(
            "sir_map_germany",
            height = "80vh"
          )
        )
      ),
      tabItem(
        tabName = "sm_norway",
        column(
          width = 4,
          fluidRow(
            multiInput(
              inputId = "multi_norway_demo",
              label = "Select demographic variables",
              choices = colnames_norway_nice[
                which(colnames_norway_actual %in% colnames(newest_numbers_norway)[c(3, 5:30, 31:37, 39)][c(1:7, 29:31)])
              ]
            )
          ),
          fluidRow(
            multiInput(
              inputId = "multi_norway_infra",
              label = "Select infrastructure variables",
              choices = colnames_norway_nice[
                which(colnames_norway_actual %in% colnames(newest_numbers_norway)[c(3, 5:30, 31:37, 39)][c(8:25)])
              ]
            ),
          ),
          fluidRow(
            column(
              width = 6,
              textInput(
                "sigma_0_norway",
                "Enter sigma_0",
                1
              )
            ),
            column(
              width = 6,
              textInput(
                "alpha_norway",
                "Enter alpha",
                0.01
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              actionBttn(
                inputId = "start_norway",
                label = "Calculate BYM2 model",
                style = "material-flat",
                color = "danger"
              )
            )
          )
        ),
        column(
          width = 8,
          fluidRow(
            mapdeckOutput(
              "model_map_norway"
            )
          ),
          fluidRow(
            column(
              width = 4,
              pickerInput(
                "picker_col_var_norway",
                label = "Select variable",
                choices = c(
                  "Relative risk",
                  "Posterior mean of the random effects",
                  "Exceedance probability",
                  "Spatial field unstructured component",
                  "Spatial field structured component"
                ),
                selected = "Relative risk"
              )
            ),
            column(
              width = 4,
              pickerInput(
                inputId = "map_style_norway_2",
                label = "Select base map",
                choices = c(
                  "dark", "light", "outdoors", "satellite"
                )
              )
            )
          ),
          br(),
          fluidRow(
            DTOutput(
              "datatable_1_norway"
            )
          ),
          br(),
          br(),
          fluidRow(
            DTOutput(
              "datatable_2_norway"
            )
          )
        )
      ),
      tabItem(
        tabName = "sm_germany",
        column(
          width = 4,
          fluidRow(
            multiInput(
              inputId = "multi_germany_demo",
              label = "Select demographic variables",
              choices = colnames_germany_nice[
                which(colnames_germany_actual %in% colnames(newest_numbers_germany)[c(2:15, 17:34, 37:41, 44:46, 48)][c(1:15, 34:36)])
              ]
            )
          ),
          fluidRow(
            multiInput(
              inputId = "multi_germany_infra",
              label = "Select infrastructure variables",
              choices = colnames_germany_nice[
                which(colnames_germany_actual %in% colnames(newest_numbers_germany)[c(3, 5:30, 31:37, 39)][c(16:32, 37)])
              ]
            ),
          ),
          fluidRow(
            column(
              width = 6,
              textInput(
                "sigma_0_germany",
                "Enter sigma_0",
                1
              )
            ),
            column(
              width = 6,
              textInput(
                "alpha_germany",
                "Enter alpha",
                0.01
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              actionBttn(
                inputId = "start_germany",
                label = "Calculate BYM2 model",
                style = "material-flat",
                color = "danger"
              )
            )
          )
        ),
        column(
          width = 8,
          fluidRow(
            mapdeckOutput(
              "model_map_germany"
            )
          ),
          fluidRow(
            column(
              width = 4,
              pickerInput(
                "picker_col_var_germany",
                label = "Select variable",
                choices = c(
                  "Relative risk",
                  "Posterior mean of the random effects",
                  "Exceedance probability",
                  "Spatial field unstructured component",
                  "Spatial field structured component"
                ),
                selected = "Relative risk"
              )
            ),
            column(
              width = 4,
              pickerInput(
                inputId = "map_style_germany_2",
                label = "Select base map",
                choices = c(
                  "dark", "light", "outdoors", "satellite"
                )
              )
            )
          ),
          br(),
          fluidRow(
            DTOutput(
              "datatable_1_germany"
            )
          ),
          br(),
          br(),
          fluidRow(
            DTOutput(
              "datatable_2_germany"
            )
          )
        )
      ),
      tabItem(
        tabName = "modelling_temporal",
        column(
          width = 3,
          fluidRow(
            multiInput(
              inputId = "multi_europe_mobility",
              label = "Select demographic variables",
              choices = sort(colnames_europe_nice[which(colnames_europe_actual %in% colnames(ts_europe)[5:43][c(15:20)])]),
              width = "100%"
            )
          ),
          fluidRow(
            multiInput(
              inputId = "multi_europe_government",
              label = "Select government measure variables",
              choices = sort(colnames_europe_nice[which(colnames_europe_actual %in% colnames(ts_europe)[5:43][c(21:33, 36, 37)])]),
              width = "100%"
            ),
          ),
          fluidRow(
            multiInput(
              inputId = "multi_europe_health",
              label = "Select vaccination variables",
              choices = sort(colnames_europe_nice[which(colnames_europe_actual %in% colnames(ts_europe)[5:43][c(34:35)])]),
              width = "100%"
            ),
          )
        ),
        column(
          width = 3,
          fluidRow(
            column(
              width = 6,
              textInput(
                "sigma_0_europe",
                "Enter sigma_0",
                1
              )
            ),
            column(
              width = 6,
              textInput(
                "alpha_europe",
                "Enter alpha",
                0.01
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              pickerInput(
                inputId = "temporal_term",
                label = "Select type of temporal term",
                choices = c(
                  "iid",
                  "rw1",
                  "rw2",
                  "ar1",
                  "ou"
                ),
                width = "100%"
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              pickerInput(
                inputId = "temporal_country",
                label = "Select country",
                choices = sort(unique(ts_europe$Country)),
                selected = "Norway",
                width = "100%"
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              textInput(
                inputId = "test_size_temporal",
                label = "Select test size",
                value = 21,
                width = "100%"
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              actionBttn(
                inputId = "start_europe",
                label = "Calculate temporal model",
                style = "material-flat",
                color = "danger"
              )
            )
          )
        ),
        column(
          width = 6,
          fluidRow(
            column(
              width = 12,
              highchartOutput(
                "highchart_europe_1"
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              highchartOutput(
                "highchart_europe_2"
              )
            )
          ),
          fluidRow(
            DTOutput(
              "datatable_1_europe"
            )
          ),
          br(),
          br(),
          fluidRow(
            DTOutput(
              "datatable_2_europe"
            )
          )
        )
      )
    ),
    add_busy_spinner(color = "#F95738")
  ),
  title = "Covid-19 Dashboard",
  skin = "black"
)
