observeEvent(
  {
    input$picker_norway
  },
  {
    selected_og <- input$picker_norway
    selected_og_2 <- colnames_norway_actual[match(selected_og, colnames_norway_nice)]
    selected <- colnames_norway_actual[match(selected_og, colnames_norway_nice)]
    cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
    diff_names <- cols[8:25][!cols[8:25] %in% unique(pois_norway$type)]
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
    pal <- colorRamp(
      lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
      alpha = TRUE
    )((1:256) / 256)
    pal[, 4] <- 150
    if (!is.null(input$map_type_norway)) {
      if (input$map_type_norway == "Hexagon Map") {
        if (selected_og %in% colnames_norway_nice[which(colnames_norway_actual %in% cols[8:25])]) {
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
        if (selected_og %in% colnames_norway_nice[which(colnames_norway_actual %in% cols[8:25])]) {
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
        placeholder <- newest_numbers_norway[, selected_og_2]
        placeholder$geometry <- NULL
        newest_numbers_norway$tooltip <- paste(
          newest_numbers_norway$kommune_name,
          "<br>",
          selected_og, ": ",
          unlist(placeholder[, 1])
        )
        mapdeck_update(map_id = "map_norway") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_norway") %>%
          add_polygon(
            data = newest_numbers_norway,
            fill_colour = selected_og_2,
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
  },
  priority = 100
)

observeEvent(
  {
    input$map_type_norway
  },
  {
    selected_og <- input$picker_norway
    selected_og_2 <- colnames_norway_actual[match(selected_og, colnames_norway_nice)]
    selected <- colnames_norway_actual[match(selected_og, colnames_norway_nice)]
    cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
    diff_names <- cols[8:25][!cols[8:25] %in% unique(pois_norway$type)]
    if (length(selected) > 0) {
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
      pal <- colorRamp(
        lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
        alpha = TRUE
      )((1:256) / 256)
      pal[, 4] <- 150
      if (!is.null(input$map_type_norway)) {
        if (input$map_type_norway == "Hexagon Map") {
          if (selected_og %in% colnames_norway_nice[which(colnames_norway_actual %in% cols[8:25])]) {
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
          if (selected_og %in% colnames_norway_nice[which(colnames_norway_actual %in% cols[8:25])]) {
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
          placeholder <- newest_numbers_norway[, selected_og_2]
          placeholder$geometry <- NULL
          newest_numbers_norway$tooltip <- paste(
            newest_numbers_norway$kommune_name,
            "<br>",
            selected_og, ": ",
            unlist(placeholder[, 1])
          )
          mapdeck_update(map_id = "map_norway") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_norway") %>%
            add_polygon(
              data = newest_numbers_norway,
              fill_colour = selected_og_2,
              legend = list(stroke_colour = FALSE, fill_colour = TRUE),
              stroke_width = 500,
              stroke_colour = "#121212",
              auto_highlight = TRUE,
              palette = pal,
              tooltip = "tooltip",
              layer_id = "polygon_norway",
              legend_options = list(
                selected_og_2 = list(title = selected_og)
              )
            )
        }
      }
    }
  },
  priority = 99
)


observeEvent(
  {
    input$map_style_norway
  },
  {
    mapdeck_update(map_id = "map_norway") %>%
      update_style(mapdeck_style(input$map_style_norway))
  },
  priority = 98
)

observeEvent(
  {
    input$picker_germany
  },
  {
    selected_og <- input$picker_germany
    selected_og_2 <- colnames_germany_actual[match(selected_og, colnames_germany_nice)]
    selected <- colnames_germany_actual[match(selected_og, colnames_germany_nice)]
    cols <- colnames(newest_numbers_germany)[c(2:15, 17:34, 37:41, 44:46, 48)]
    diff_names <- cols[c(16:32, 37)][!cols[c(16:32, 37)] %in% unique(pois_germany$type)]
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
    pal <- colorRamp(
      lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
      alpha = TRUE
    )((1:256) / 256)
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
    if (!is.null(input$map_type_germany)) {
      if (input$map_type_germany == "Hexagon Map") {
        if (selected_og %in% colnames_germany_nice[which(colnames_germany_actual %in% cols[c(16:32, 37)])]) {
          mapdeck_update(map_id = "map_germany") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_germany") %>%
            add_hexagon(
              data = st_as_sf(
                pois_germany[pois_germany$type == selected, ],
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
          mapdeck_update(map_id = "map_germany") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_germany")
        }
      } else if (input$map_type_germany == "Heatmap") {
        if (selected_og %in% colnames_germany_nice[which(colnames_germany_actual %in% cols[c(16:32, 37)])]) {
          mapdeck_update(map_id = "map_germany") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_germany") %>%
            add_heatmap(
              data = st_as_sf(
                pois_germany[pois_germany$type == selected, ],
                coords = c("longitude", "latitude"),
                crs = 4326
              ),
              colour_range = lacroix_palette("Pamplemousse", type = "continuous", n = 6)[6:1],
              layer_id = "pois_heatmap",
              update_view = FALSE
            )
        } else {
          mapdeck_update(map_id = "map_germany") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_germany")
        }
      } else if (input$map_type_germany == "Choropleth") {
        placeholder <- newest_numbers_germany[, selected_og_2]
        placeholder$geometry <- NULL
        newest_numbers_germany$tooltip <- paste(
          newest_numbers_germany$municipality,
          "<br>",
          selected_og, ": ",
          unlist(placeholder[, 1])
        )
        mapdeck_update(map_id = "map_germany") %>%
          clear_hexagon(layer_id = "pois_hexagon") %>%
          clear_heatmap(layer_id = "pois_heatmap") %>%
          clear_polygon(layer_id = "polygon_germany") %>%
          add_polygon(
            data = newest_numbers_germany,
            fill_colour = selected_og_2,
            legend = list(stroke_colour = FALSE, fill_colour = TRUE),
            stroke_width = 500,
            stroke_colour = "#121212",
            auto_highlight = TRUE,
            palette = pal,
            tooltip = "tooltip",
            layer_id = "polygon_germany"
          )
      }
    }
  },
  priority = 97
)

observeEvent(
  {
    input$map_type_germany
  },
  {
    selected_og <- input$picker_germany
    selected_og_2 <- colnames_germany_actual[match(selected_og, colnames_germany_nice)]
    selected <- colnames_germany_actual[match(selected_og, colnames_germany_nice)]
    cols <- colnames(newest_numbers_germany)[c(2:15, 17:34, 37:41, 44:46, 48)]
    diff_names <- cols[c(16:32, 37)][!cols[c(16:32, 37)] %in% unique(pois_germany$type)]
    if (length(selected) > 0) {
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
      pal <- colorRamp(
        lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
        alpha = TRUE
      )((1:256) / 256)
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
      if (!is.null(input$map_type_germany)) {
        if (input$map_type_germany == "Hexagon Map") {
          if (selected_og %in% colnames_germany_nice[which(colnames_germany_actual %in% cols[c(16:32, 37)])]) {
            mapdeck_update(map_id = "map_germany") %>%
              clear_hexagon(layer_id = "pois_hexagon") %>%
              clear_heatmap(layer_id = "pois_heatmap") %>%
              clear_polygon(layer_id = "polygon_germany") %>%
              add_hexagon(
                data = st_as_sf(
                  pois_germany[pois_germany$type == selected, ],
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
            mapdeck_update(map_id = "map_germany") %>%
              clear_hexagon(layer_id = "pois_hexagon") %>%
              clear_heatmap(layer_id = "pois_heatmap") %>%
              clear_polygon(layer_id = "polygon_germany")
          }
        } else if (input$map_type_germany == "Heatmap") {
          if (selected_og %in% colnames_germany_nice[which(colnames_germany_actual %in% cols[c(16:32, 37)])]) {
            mapdeck_update(map_id = "map_germany") %>%
              clear_hexagon(layer_id = "pois_hexagon") %>%
              clear_heatmap(layer_id = "pois_heatmap") %>%
              clear_polygon(layer_id = "polygon_germany") %>%
              add_heatmap(
                data = st_as_sf(
                  pois_germany[pois_germany$type == selected, ],
                  coords = c("longitude", "latitude"),
                  crs = 4326
                ),
                colour_range = lacroix_palette("Pamplemousse", type = "continuous", n = 6)[6:1],
                layer_id = "pois_heatmap",
                update_view = FALSE
              )
          } else {
            mapdeck_update(map_id = "map_germany") %>%
              clear_hexagon(layer_id = "pois_hexagon") %>%
              clear_heatmap(layer_id = "pois_heatmap") %>%
              clear_polygon(layer_id = "polygon_germany")
          }
        } else if (input$map_type_germany == "Choropleth") {
          placeholder <- newest_numbers_germany[, selected_og_2]
          placeholder$geometry <- NULL
          newest_numbers_germany$tooltip <- paste(
            newest_numbers_germany$municipality,
            "<br>",
            selected_og, ": ",
            unlist(placeholder[, 1])
          )
          mapdeck_update(map_id = "map_germany") %>%
            clear_hexagon(layer_id = "pois_hexagon") %>%
            clear_heatmap(layer_id = "pois_heatmap") %>%
            clear_polygon(layer_id = "polygon_germany") %>%
            add_polygon(
              data = newest_numbers_germany,
              fill_colour = selected_og_2,
              legend = list(stroke_colour = FALSE, fill_colour = TRUE),
              stroke_width = 500,
              stroke_colour = "#121212",
              auto_highlight = TRUE,
              palette = pal,
              tooltip = "tooltip",
              layer_id = "polygon_germany"
            )
        }
    }
    }
  },
  priority = 96
)


observeEvent(
  {
    input$map_style_germany
  },
  {
    mapdeck_update(map_id = "map_germany") %>%
      update_style(mapdeck_style(input$map_style_germany))
  },
  priority = 95
)

observeEvent(
  {
    input$start_norway
  },
  {
    vars <- c(input$multi_norway_demo, input$multi_norway_infra)
    sigma_0 <- as.numeric(input$sigma_0_norway)
    alpha <- as.numeric(input$alpha_norway)
    sigma_0 <- ifelse(is.na(sigma_0), 1, sigma_0)
    alpha <- ifelse(is.na(alpha), 0.01, alpha)
    set.seed(7918)
    test <- sample(
      seq_len(nrow(newest_numbers_norway)),
      size = floor(0.2 * nrow(newest_numbers_norway))
    )
    test_value <- newest_numbers_norway$infections[test]
    newest_numbers_norway$infections[test] <- NA
    link <- rep(NA, nrow(newest_numbers_norway))
    link[which(is.na(newest_numbers_norway$infections))] <- 1
    prior <- list(
      prec = list(
        prior = "pc.prec",
        param = c(sigma_0, alpha)
      )
    )
    if (length(vars) > 0) {
      formula <- as.formula(
        paste(
          "infections ~",
          paste(colnames_norway_actual[which(colnames_norway_nice %in% vars)], collapse = "+"),
          "+ f(idarea_1, model = 'bym2', graph = g_norway, scale.model = TRUE, hyper = prior)"
        )
      )
      
    } else {
      formula <- infections ~ 1 +
        f(idarea_1, model = "bym2", graph = g_norway, scale.model = TRUE, hyper = prior)
    }
    result <- inla(
      formula,
      family = "nbinomial",
      data = newest_numbers_norway,
      E = expected_count,
      control.predictor = list(
        compute = TRUE,
        link = link
      ),
      Ntrials = newest_numbers_norway$population,
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
    )
    model_norway$results$model <- result
    predicted <- c()
    for (i in seq_len(nrow(newest_numbers_norway))) {
      predicted[i] <- inla.emarginal(
        function(x) x * newest_numbers_norway$population[i],
        result$marginals.fitted.values[[i]]
      )
    }
    results_tibble <- tibble(
      DIC = round(result$dic$dic),
      WAIC = round(result$waic$waic),
      CPO = round(sum(log(result$cpo$cpo), na.rm = TRUE)),
      MAE = round(mean(abs(predicted[test] - test_value))),
      Precision = round(result$summary.hyperpar$mean[2], 2),
      Phi = round(result$summary.hyperpar$mean[3], 2),
      alpha = alpha,
      sigma_0 = sigma_0,
      Covariates = length(vars),
      ID = as.numeric(input$start_norway)
    )
    if (is.na(model_norway$results$performance_frame)) {
      model_norway$results$performance_frame <- results_tibble
    } else {
      model_norway$results$performance_frame <- rbind(
        model_norway$results$performance_frame,
        results_tibble
      )
    }
    model_norway$results$predictions <- predicted
    conf_intervals <- sapply(
      result$marginals.fixed[
        rownames(result$summary.fixed[
          order(result$summary.fixed$mean),
        ])
      ],
      function(x) {
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, x
          )
        )
      }
    )
    means <- sapply(
      result$marginals.fixed[
        rownames(result$summary.fixed[
          order(result$summary.fixed$mean),
        ])
      ],
      inla.emarginal,
      fun = exp
    )
    is_significant <- lapply(
      seq_len(ncol(conf_intervals)),
      function(x, ...) {
        sum(conf_intervals[, x] < 1) == 2 || sum(conf_intervals[, x] > 1) == 2
      }
    )
    vars_id <- lapply(colnames(conf_intervals[, unlist(is_significant)]), function(x, ...) {
      which(colnames_norway_actual %in% x)
    })
    colnames_norway_nice_2 <- c(colnames_norway_nice, "(Intercept)")
    vars_id[unlist(lapply(vars_id, length)) == 0][[1]] <- length(colnames_norway_nice_2)
    if (length(is_significant) & length(vars_id) == 0) vars_id <- list(length(colnames_norway_nice_2))
    # Var = colnames_norway_nice_2[unlist(vars_id)]
    # print(conf_intervals)
    # Q025 = round(as.numeric(conf_intervals[, unlist(is_significant)][1, ]), 3)
    # Mean = round(as.numeric(means[unlist(is_significant)]), 3)
    # Q975 = round(as.numeric(conf_intervals[, unlist(is_significant)][2, ]), 3)
    # ID = as.numeric(input$start_norway)
    sign_tibble <- tibble(
      Var = colnames_norway_nice_2[unlist(vars_id)],
      Q025 = round(as.numeric(as.matrix(conf_intervals[, unlist(is_significant)])[1, ]), 3),
      Mean = round(as.numeric(means[unlist(is_significant)]), 3),
      Q975 = round(as.numeric(as.matrix(conf_intervals[, unlist(is_significant)])[2, ]), 3),
      ID = as.numeric(input$start_norway)
    )
    print(sign_tibble)
    if (is.na(model_norway$results$sign_frame)) {
      model_norway$results$sign_frame <- sign_tibble
    } else {
      model_norway$results$sign_frame <- rbind(
        model_norway$results$sign_frame,
        sign_tibble
      )
    }
  },
  priority = 94
)

observeEvent(
  {
    list(input$picker_col_var, input$start_norway)
  },
  {
    result <- model_norway$results$model
    if (!is.na(result)) {
      newest_numbers_norway$`Relative risk` <- result$summary.fitted.values$mean
      csi <- result$marginals.random$idarea_1[
        seq_len(nrow(newest_numbers_norway))
      ]
      a <- 0
      prob_csi <- lapply(csi, function(x) {
        1 - inla.pmarginal(a, x)
      })
      zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
      newest_numbers_norway$`Posterior mean of the random effects` <- round(unlist(zeta), 2)
      newest_numbers_norway$`Exceedance probability` <- round(unlist(prob_csi), 2)
      newest_numbers_norway$`Spatial field unstructured component` <- result$summary.random$idarea_1$mean[1:356]
      newest_numbers_norway$`Spatial field structured component` <- result$summary.random$idarea_1$mean[357:712]
      pal <- colorRamp(
        lacroix_palette("Pamplemousse", type = "continuous", n = 60)[45:1],
        alpha = TRUE
      )((1:256) / 256)
      pal[, 4] <- 150
      placeholder <- newest_numbers_norway[, input$picker_col_var]
      placeholder$geometry <- NULL
      newest_numbers_norway$tooltip <- paste(
        newest_numbers_norway$tooltip,
        "<br>",
        input$picker_col_var, ": ",
        round(unlist(placeholder[, 1]), 2),
        sep = ""
      )
      mapdeck_update(map_id = "model_map_norway") %>%
        clear_polygon("polygon_layer") %>%
        add_polygon(
          data = newest_numbers_norway,
          fill_colour = input$picker_col_var,
          legend = list(stroke_colour = FALSE, fill_colour = TRUE),
          stroke_width = 500,
          stroke_colour = "#121212",
          auto_highlight = TRUE,
          palette = pal,
          tooltip = "tooltip",
          layer_id = "polygon_layer"
        )
    }
  },
  priority = 93
)

observeEvent(
  {
    input$map_style_norway_2
  },
  {
    mapdeck_update(map_id = "model_map_norway") %>%
      update_style(mapdeck_style(input$map_style_norway_2))
  },
  priority = 92
)