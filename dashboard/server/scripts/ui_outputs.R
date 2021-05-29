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