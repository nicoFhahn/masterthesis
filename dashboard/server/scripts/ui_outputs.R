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