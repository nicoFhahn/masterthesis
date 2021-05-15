output$dropdown_ui_norway <- renderUI({
  cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
  pickerInput(
    inputId = "picker_norway",
    label = "Select variable",
    choices = list(
      Infrastructure = sort(cols[10:27]),
      Health = sort(cols[32:35]),
      Others = sort(cols[c(1:9, 29:31)])
    ),
    selected = "office"
  )
})

output$dropdown_ui_germany <- renderUI({
  cols <- colnames(newest_numbers_germany)[c(2:15, 17:34, 37:41, 44:46, 48)]
  pickerInput(
    inputId = "picker_germany",
    label = "Select variable",
    choices = list(
      Infrastructure = sort(cols[c(16:32, 37)]),
      Health = sort(cols[38:41]),
      Others = sort(cols[c(1:15, 33:36)])
    ),
    selected = "office"
  )
})