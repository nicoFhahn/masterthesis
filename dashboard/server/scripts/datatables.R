output$datatable_1_norway <- renderDataTable(
  if (!is.na(model_norway$results$performance_frame)) {
    model_norway$results$performance_frame
  }
)

output$datatable_2_norway <- renderDataTable({
  if (!is.na(model_norway$results$sign_frame)) {
    model_norway$results$sign_frame
  }
})

output$datatable_1_germany <- renderDataTable(
  if (!is.na(model_germany$results$performance_frame)) {
    model_germany$results$performance_frame
  }
)

output$datatable_2_germany <- renderDataTable({
  if (!is.na(model_germany$results$sign_frame)) {
    model_germany$results$sign_frame
  }
})
