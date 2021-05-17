output$map_norway <- renderMapdeck({
  mapdeck(
    style = mapdeck_style("dark"),
    location = c(13, 63),
    zoom = 4,
    pitch = 45,
    token = token
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
