output$dropdown_ui_norway <- renderUI({
  cols <- colnames(newest_numbers_norway)[c(3, 5:30, 31:37, 39)]
  pickerInput(
    inputId = "picker_norway",
    label = "Select variable",
    choices = list(
      Infrastructure = colnames_norway_nice[which(colnames_norway_actual %in% cols[8:25])],
      Health = colnames_norway_nice[which(colnames_norway_actual %in% cols[32:35])],
      Others = colnames_norway_nice[which(colnames_norway_actual %in% cols[c(1:7, 29:31)])]
    ),
    selected = "Offices"
  )
})

output$dropdown_ui_germany <- renderUI({
  cols <- colnames(newest_numbers_germany)[c(2:15, 17:34, 37:41, 44:46, 48)]
  pickerInput(
    inputId = "picker_germany",
    label = "Select variable",
    choices = list(
      Infrastructure = colnames_germany_nice[which(colnames_germany_actual %in% cols[c(16:32, 37)])],
      Health = colnames_germany_nice[which(colnames_germany_actual %in% cols[38:41])],
      Others = colnames_germany_nice[which(colnames_germany_actual %in% cols[c(1:15, 33:36)])]
    ),
    selected = "Offices"
  )
})

output$dropdown_ui_europe <- renderUI({
  cols <- colnames(ts_europe)[c(5:43)]
  pickerInput(
    inputId = "picker_europe",
    label = "Select variable",
    choices = list(
      "General key figures" = sort(colnames_europe_nice[which(colnames_europe_actual %in% cols[c(3:14, 38)])]),
      Mobility = sort(colnames_europe_nice[which(colnames_europe_actual %in% cols[c(15:20)])]),
      "Government measures" = sort(colnames_europe_nice[which(colnames_europe_actual %in% cols[c(21:33, 36, 37)])]),
      Health = sort(colnames_europe_nice[which(colnames_europe_actual %in% cols[c(1, 2, 34, 35, 39)])])
    ),
    selected = "Life expectancy"
  )
})

output$municipality_germany_ui <- renderUI({
  pickerInput(
    inputId = "municipality_germany",
    label = "Select municipality",
    choices = sort(unique(germany_munc_long$Landkreis)),
    selected = "LK Rosenheim"
  )
})

output$date_europe_ui <- renderUI({
  dateInput(
    "date_europe",
    label = "Select date",
    min = as.Date("2020-01-03"),
    max = max(ts_europe$Date),
    value = max(ts_europe$Date)
  )
})

output$country_europe_ui <- renderUI({
  pickerInput(
    inputId = "country_europe",
    label = "Select country",
    choices = sort(unique(ts_europe$Country)),
    selected = "Germany"
  )
})

output$ui_multi_norway <- renderUI({
  div(
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
    )
  )
})

output$ui_multi_germany <- renderUI({
  div(
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
    )
  )
})

output$ui_multi_europe <- renderUI({
  div(
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
  )
})

output$temporal_country_ui <- renderUI({
  pickerInput(
    inputId = "temporal_country",
    label = "Select country",
    choices = sort(unique(ts_europe$Country)),
    selected = "Norway",
    width = "100%"
  )
})