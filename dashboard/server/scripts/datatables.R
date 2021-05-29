output$datatable_1_norway <- renderDT(
  if (!is.na(model_norway$results$performance_frame)) {
    datatable(model_norway$results$performance_frame)
  }
)

output$datatable_2_norway <- renderDT({
  if (!is.na(model_norway$results$sign_frame)) {
    model_norway$results$sign_frame
  }
})

output$datatable_1_germany <- renderDT(
  if (!is.na(model_germany$results$performance_frame)) {
    datatable(model_germany$results$performance_frame)
  }
)

output$datatable_2_germany <- renderDT({
  if (!is.na(model_germany$results$sign_frame)) {
    model_germany$results$sign_frame
  }
})

output$datatable_1_europe <- renderDT(
  if (!is.na(model_europe$results$performance_frame)) {
    datatable(model_europe$results$performance_frame)
  }
)

output$datatable_2_europe <- renderDT({
  if (!is.na(model_europe$results$sign_frame)) {
    datatable(model_europe$results$sign_frame, escape = FALSE)
  }
})
