observeEvent({input$picker_norway},{
  selected <- input$picker_norway
  cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
  diff_names <- cols[10:27][!cols[10:27] %in% unique(pois_norway$type)]
  pal <- colorRamp(
    lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
    alpha = TRUE)((1:256)/256)
  pal[, 4] <- 150
  if (selected %in% diff_names) {
    input_names <- c(
      "bakeries", "higher_education", "nursing_home", "place_of_worship",
      "schools", "shops"
    )
    real_names <- c(
      "bakery", "higher education", "nursing home", "place of worship",
      "school", "shop"
    )
    selected <- real_names[match(selected, input_names)]
  }
  if (!is.null(input$map_type_norway)) {
    if (input$map_type_norway == "Hexagon Map") {
      if (input$picker_norway %in% cols[10:27]) {
        mapdeck_update(map_id = "map_norway") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_norway") %>%
          add_hexagon(
            data = st_as_sf(
              pois_norway[pois_norway$type == selected, ],
              coords = c("longitude", "latitude"),
              crs = 4326
            ),
            # size of each cell in meters
            radius = 2500,
            # multiplier for the elevation of the cells
            elevation_scale = 250,
            colour_range = lacroix_palette("Pamplemousse", type = "continuous", n = 6)[6:1],
            auto_highlight = TRUE,
            layer_id = "pois_hexagon",
            update_view = FALSE
          )
      } else {
        mapdeck_update(map_id = "map_norway") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_norway")
      }
    } else if (input$map_type_norway == "Heatmap") {
      if (input$picker_norway %in% cols[10:27]) {
        mapdeck_update(map_id = "map_norway") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_norway") %>%
          add_heatmap(
            data = st_as_sf(
              pois_norway[pois_norway$type == selected, ],
              coords = c("longitude", "latitude"),
              crs = 4326
            ),
            colour_range = lacroix_palette("Pamplemousse", type = "continuous", n = 6)[6:1],
            layer_id = "pois_heatmap",
            update_view = FALSE
          )
      } else {
        mapdeck_update(map_id = "map_norway") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_norway")
      }
    } else if (input$map_type_norway == "Choropleth") {
      placeholder <- newest_numbers_norway[, input$picker_norway]
      placeholder$geometry <- NULL
      newest_numbers_norway$tooltip <- paste(
        newest_numbers_norway$kommune_name,
        "<br>",
        unlist(placeholder[, 1])
      )
      mapdeck_update(map_id = "map_norway") %>%
        clear_hexagon(layer_id = "pois_hexagon") %>%
        clear_heatmap(layer_id = "pois_heatmap") %>%
        clear_polygon(layer_id = "polygon_norway") %>%
        add_polygon(
          data = newest_numbers_norway,
          fill_colour = input$picker_norway,
          legend = list(stroke_colour = FALSE, fill_colour = TRUE),
          stroke_width = 500,
          stroke_colour = "#121212",
          auto_highlight = TRUE,
          palette = pal,
          tooltip = "tooltip",
          layer_id = "polygon_norway"
        )
    }
  }
}, priority = 100)

observeEvent({input$map_type_norway},{
  selected <- input$picker_norway
  cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
  diff_names <- cols[10:27][!cols[10:27] %in% unique(pois_norway$type)]
  pal <- colorRamp(
    lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
    alpha = TRUE)((1:256)/256)
  pal[, 4] <- 150
  if (!is.null(selected)) {
    if (selected %in% diff_names) {
      input_names <- c(
        "bakeries", "higher_education", "nursing_home", "place_of_worship",
        "schools", "shops"
      )
      real_names <- c(
        "bakery", "higher education", "nursing home", "place of worship",
        "school", "shop"
      )
      selected <- real_names[match(selected, input_names)]
    }
    if (!is.null(input$map_type_norway)) {
      if (input$map_type_norway == "Hexagon Map") {
        if (input$picker_norway %in% cols[10:27]) {
          mapdeck_update(map_id = "map_norway") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_norway") %>%
            add_hexagon(
              data = st_as_sf(
                pois_norway[pois_norway$type == selected, ],
                coords = c("longitude", "latitude"),
                crs = 4326
              ),
              # size of each cell in meters
              radius = 2500,
              # multiplier for the elevation of the cells
              elevation_scale = 250,
              colour_range = lacroix_palette("Pamplemousse", type = "continuous", n = 6)[6:1],
              auto_highlight = TRUE,
              layer_id = "pois_hexagon",
              update_view = FALSE
            )
        } else {
          mapdeck_update(map_id = "map_norway") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_norway")
        }
      } else if (input$map_type_norway == "Heatmap") {
        if (input$picker_norway %in% cols[10:27]) {
          mapdeck_update(map_id = "map_norway") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_norway") %>%
            add_heatmap(
              data = st_as_sf(
                pois_norway[pois_norway$type == selected, ],
                coords = c("longitude", "latitude"),
                crs = 4326
              ),
              colour_range = lacroix_palette("Pamplemousse", type = "continuous", n = 6)[6:1],
              layer_id = "pois_heatmap",
              update_view = FALSE
            )
        } else {
          mapdeck_update(map_id = "map_norway") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_norway")
        }
      } else if (input$map_type_norway == "Choropleth") {
        placeholder <- newest_numbers_norway[, input$picker_norway]
        placeholder$geometry <- NULL
        newest_numbers_norway$tooltip <- paste(
          newest_numbers_norway$kommune_name,
          "<br>",
          unlist(placeholder[, 1])
        )
        mapdeck_update(map_id = "map_norway") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_norway") %>%
          add_polygon(
            data = newest_numbers_norway,
            fill_colour = input$picker_norway,
            legend = list(stroke_colour = FALSE, fill_colour = TRUE),
            stroke_width = 500,
            stroke_colour = "#121212",
            auto_highlight = TRUE,
            palette = pal,
            tooltip = "tooltip",
            layer_id = "polygon_norway"
          )
      }
    }
  }
}, priority = 99)


observeEvent({input$map_style_norway},{
  mapdeck_update(map_id = "map_norway") %>%
    update_style(mapdeck_style(input$map_style_norway))
}, priority = 98)