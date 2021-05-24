output$highchart_norway_1 <- renderHighchart({
  no_geometry_norway <- newest_numbers_norway
  no_geometry_norway$geometry <- NULL
  selected_og <- input$picker_norway
  selected_og_2 <- colnames_norway_actual[match(selected_og, colnames_norway_nice)]
  selected <- colnames_norway_actual[match(selected_og, colnames_norway_nice)]
  cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
  diff_names <- cols[8:25][!cols[8:25] %in% unique(pois_norway$type)]
  part_1 <- ifelse(
    selected %in% cols[8:25],
    "Number of buildings of type",
    "Histogram for variable"
  )
  part_2 <- ifelse(
    selected %in% cols[8:25],
    ifelse(
      str_sub(selected_og, -1) == "s",
      str_sub(selected_og, end = -2),
      selected_og
    ),
    input$picker_norway
  )
  if (str_detect(part_2, "Bakerie")) part_2 <- str_replace(part_2, "Bakerie", "Bakery")
  if (str_detect(part_2, "Places of")) part_2 <- str_replace(part_2, "Places of", "Place of")
  if (str_detect(part_2, "facilitie")) part_2 <- str_replace(part_2, "facilitie", "facility")
  axis_title <- ifelse(
    selected %in% cols[8:25],
    "Number of buildings",
    input$picker_norway
  )
  hchart(
    no_geometry_norway[, selected],
    color = "#E8D7FF"
  ) %>%
    hc_title(
      text = paste(part_1, part_2),
      style = list(
        color = "#fff",
        fontSize = "calc(0.5em + 0.5vw)"
      )
    ) %>%
    hc_xAxis(
      title = list(
        text = axis_title,
        style = list(
          color = "#fff",
          `font-size` = "calc(0.3em + 0.5vw)"
        )
      ),
      labels = list(
        style = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
    ) %>%
    hc_yAxis(
      title = list(
        text = "Frequency",
        style = list(
          color = "#fff",
          `font-size` = "calc(0.3em + 0.5vw)"
        )
      ),
      labels = list(
        style = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
    ) %>%
    hc_legend(enabled = FALSE)
})

output$highchart_norway_2 <- renderHighchart({
  dates <- format(unique(
    norway_munc_conf_long[
      norway_munc_conf_long$kommune_name == "Norway",
    ]$date
  ), "%d.%m")
  if (input$inf_numbers_norway == "Total number of infections") {
    highchart() %>%
      hc_xAxis(
        categories = dates,
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == "Norway",
        ]$value,
        type = "line",
        name = "Norway",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == input$municipality_norway,
        ]$value,
        type = "line",
        name = input$municipality_norway,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Total number of infections in", input$municipality_norway),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else if (input$inf_numbers_norway == "Total number of infections per 100k") {
    highchart() %>%
      hc_xAxis(
        categories = dates,
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == "Norway",
        ]$value_100k,
        type = "line",
        name = "Norway",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == input$municipality_norway,
        ]$value_100k,
        type = "line",
        name = input$municipality_norway,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Total number of infections per 100k in", input$municipality_norway),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else if (input$inf_numbers_norway == "Daily number of infections") {
    highchart() %>%
      hc_xAxis(
        categories = dates[2:length(unique(norway_munc_conf_long$date))],
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == "Norway",
        ]$value_daily[2:length(unique(norway_munc_conf_long$date))],
        type = "line",
        name = "Norway",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == input$municipality_norway,
        ]$value_daily[2:length(unique(norway_munc_conf_long$date))],
        type = "line",
        name = input$municipality_norway,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Daily number of infections in", input$municipality_norway),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else if (input$inf_numbers_norway == "Daily number of infections per 100k") {
    highchart() %>%
      hc_xAxis(
        categories = dates[2:length(unique(norway_munc_conf_long$date))],
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == "Norway",
        ]$value_daily_100k[2:length(unique(norway_munc_conf_long$date))],
        type = "line",
        name = "Norway",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == input$municipality_norway,
        ]$value_daily_100k[2:length(unique(norway_munc_conf_long$date))],
        type = "line",
        name = input$municipality_norway,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Daily number of infections per 100k in", input$municipality_norway),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else {
    highchart() %>%
      hc_xAxis(
        categories = dates[8:length(unique(norway_munc_conf_long$date))],
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == "Norway",
        ]$incidence_seven[8:length(unique(norway_munc_conf_long$date))],
        type = "line",
        name = "Norway",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = norway_munc_conf_long[
          norway_munc_conf_long$kommune_name == input$municipality_norway,
        ]$incidence_seven[8:length(unique(norway_munc_conf_long$date))],
        type = "line",
        name = input$municipality_norway,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Seven day incidence in", input$municipality_norway),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Incidence",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  }
})

output$highchart_germany_1 <- renderHighchart({
  no_geometry_germany <- newest_numbers_germany
  no_geometry_germany$geometry <- NULL
  selected_og <- input$picker_germany
  selected_og_2 <- colnames_germany_actual[match(selected_og, colnames_germany_nice)]
  selected <- colnames_germany_actual[match(selected_og, colnames_germany_nice)]
  cols <- colnames(newest_numbers_germany)[c(2:15, 17:34, 37:41, 44:46, 48)]
  diff_names <- cols[c(16:32, 37)][!cols[c(16:32, 37)] %in% unique(pois_germany$type)]
  part_1 <- ifelse(
    selected %in% cols[c(16:32, 37)],
    "Number of buildings of type",
    "Histogram for variable"
  )
  part_2 <- ifelse(
    selected %in% cols[c(16:32, 37)],
    ifelse(
      str_sub(selected_og, -1) == "s",
      str_sub(selected_og, end = -2),
      selected_og
    ),
    input$picker_germany
  )
  if (str_detect(part_2, "Bakerie")) part_2 <- str_replace(part_2, "Bakerie", "Bakery")
  if (str_detect(part_2, "Places of")) part_2 <- str_replace(part_2, "Places of", "Place of")
  if (str_detect(part_2, "facilitie")) part_2 <- str_replace(part_2, "facilitie", "facility")
  part_1 <- ifelse(
    selected %in% cols[c(16:32, 37)],
    "Number of buildings of type",
    "Histogram for variable"
  )
  axis_title <- ifelse(
    selected %in% cols[c(16:32, 37)],
    "Number of buildings",
    input$picker_germany
  )
  hchart(
    no_geometry_germany[, selected],
    color = "#E8D7FF"
  ) %>%
    hc_title(
      text = paste(part_1, part_2),
      style = list(
        color = "#fff",
        fontSize = "calc(0.5em + 0.5vw)"
      )
    ) %>%
    hc_xAxis(
      title = list(
        text = axis_title,
        style = list(
          color = "#fff",
          `font-size` = "calc(0.3em + 0.5vw)"
        )
      ),
      labels = list(
        style = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
    ) %>%
    hc_yAxis(
      title = list(
        text = "Frequency",
        style = list(
          color = "#fff",
          `font-size` = "calc(0.3em + 0.5vw)"
        )
      ),
      labels = list(
        style = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
    ) %>%
    hc_legend(enabled = FALSE)
})

output$highchart_germany_2 <- renderHighchart({
  dates <- format(unique(
    germany_munc_long[
      germany_munc_long$Landkreis == "Germany",
    ]$Date
  ), "%d.%m")
  if (input$inf_numbers_germany == "Total number of infections") {
    highchart() %>%
      hc_xAxis(
        categories = dates,
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == "Germany",
        ]$value,
        type = "line",
        name = "Germany",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == input$municipality_germany,
        ]$value,
        type = "line",
        name = input$municipality_germany,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Total number of infections in", input$municipality_germany),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else if (input$inf_numbers_germany == "Total number of infections per 100k") {
    highchart() %>%
      hc_xAxis(
        categories = dates,
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == "Germany",
        ]$value_100k,
        type = "line",
        name = "Germany",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == input$municipality_germany,
        ]$value_100k,
        type = "line",
        name = input$municipality_germany,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Total number of infections per 100k in", input$municipality_germany),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else if (input$inf_numbers_germany == "Daily number of infections") {
    highchart() %>%
      hc_xAxis(
        categories = dates[2:length(unique(germany_munc_long$Date))],
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == "Germany",
        ]$value_daily[2:length(unique(germany_munc_long$Date))],
        type = "line",
        name = "Germany",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == input$municipality_germany,
        ]$value_daily[2:length(unique(germany_munc_long$Date))],
        type = "line",
        name = input$municipality_germany,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Daily number of infections in", input$municipality_germany),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else if (input$inf_numbers_germany == "Daily number of infections per 100k") {
    highchart() %>%
      hc_xAxis(
        categories = dates[2:length(unique(germany_munc_long$Date))],
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == "Germany",
        ]$value_daily_100k[2:length(unique(germany_munc_long$Date))],
        type = "line",
        name = "Germany",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == input$municipality_germany,
        ]$value_daily_100k[2:length(unique(germany_munc_long$Date))],
        type = "line",
        name = input$municipality_germany,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Daily number of infections per 100k in", input$municipality_germany),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Number of infections",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  } else {
    highchart() %>%
      hc_xAxis(
        categories = dates[8:length(unique(germany_munc_long$Date))],
        title = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == "Germany",
        ]$incidence_seven[8:length(unique(germany_munc_long$Date))],
        type = "line",
        name = "Germany",
        color = "#F2C078"
      ) %>%
      hc_add_series(
        data = germany_munc_long[
          germany_munc_long$Landkreis == input$municipality_germany,
        ]$incidence_seven[8:length(unique(germany_munc_long$Date))],
        type = "line",
        name = input$municipality_germany,
        color = "#00F0B5"
      ) %>%
      hc_title(
        text = paste("Seven day incidence in", input$municipality_germany),
        style = list(
          color = "#fff",
          fontSize = "calc(0.5em + 0.5vw)"
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = "Incidence",
          style = list(
            color = "#fff",
            `font-size` = "calc(0.3em + 0.5vw)"
          )
        ),
        labels = list(
          style = list(
            color = "#fff",
            `font-size` = "calc(0.1em + 0.5vw)"
          )
        )
      ) %>%
      hc_legend(
        verticalAlign = "top",
        align = "right",
        layout = "vertical",
        itemStyle = list(
          color = "#fff",
          `font-size` = "calc(0.1em + 0.5vw)"
        )
      )
  }
})
