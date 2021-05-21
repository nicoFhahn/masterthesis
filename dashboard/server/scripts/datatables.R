output$datatable_1_norway <- renderDataTable(
  if (!is.na(model_norway$results$performance_frame)) {
    model_norway$results$performance_frame
  }
)

output$datatable_2_norway <- renderDataTable({
  if(!is.na(model_norway$results$sign_frame)) {
    model_norway$results$sign_frame
  }
})