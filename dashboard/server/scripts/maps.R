output$map_norway <- renderMapdeck({
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(13, 63),
    zoom = 4,
    pitch = 45,
    token = token
  )
})

output$model_map_norway <- renderMapdeck({
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(13, 63),
    zoom = 4,
    pitch = 45,
    token = token
  )
})

output$sir_map_norway <- renderMapdeck({
  pal <- colorRamp(
    lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
    alpha = TRUE
  )((1:256) / 256)
  pal[, 4] <- 150
  newest_numbers_norway$expected_count <- round(newest_numbers_norway$expected_count)
  newest_numbers_norway$tooltip <- paste(
    newest_numbers_norway$kommune_name,
    "<br>",
    "Expected count: ",
    newest_numbers_norway$expected_count)
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(13, 63),
    zoom = 5,
    pitch = 45,
    token = token
  ) %>%
    add_polygon(
      data = newest_numbers_norway,
      fill_colour = "expected_count",
      legend = list(stroke_colour = FALSE, fill_colour = TRUE),
      stroke_width = 500,
      stroke_colour = "#121212",
      auto_highlight = TRUE,
      palette = pal,
      tooltip = "tooltip",
      layer_id = "polygon_layer",
      legend_options = list(
        title = "Expected count"
      )
    )
})

output$sir_map_germany <- renderMapdeck({
  pal <- colorRamp(
    lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
    alpha = TRUE
  )((1:256) / 256)
  pal[, 4] <- 150
  newest_numbers_germany$expected_count <- round(newest_numbers_germany$expected_count)
  newest_numbers_germany$tooltip <- paste(
    newest_numbers_germany$kommune_name,
    "<br>",
    "Expected count: ",
    newest_numbers_germany$expected_count)
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(11, 53),
    zoom = 5,
    pitch = 45,
    token = token
  ) %>%
    add_polygon(
      data = newest_numbers_germany,
      fill_colour = "expected_count",
      legend = list(stroke_colour = FALSE, fill_colour = TRUE),
      stroke_width = 500,
      stroke_colour = "#121212",
      auto_highlight = TRUE,
      palette = pal,
      tooltip = "tooltip",
      layer_id = "polygon_layer",
      legend_options = list(
        title = "Expected count"
      )
    )
})

output$map_germany <- renderMapdeck({
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(11, 53),
    zoom = 4,
    pitch = 45,
    token = token
  )
})

output$model_map_germany<- renderMapdeck({
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(11, 53),
    zoom = 4,
    pitch = 45,
    token = token
  )
})


output$map_europe <- renderMapdeck({
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(11, 53),
    zoom = 3,
    pitch = 45,
    token = token
  )
})