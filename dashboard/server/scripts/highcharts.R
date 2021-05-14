output$highchart_norway_1 <- renderHighchart({
  no_geometry_norway <- newest_numbers_norway
  no_geometry_norway$geometry <- NULL
  cols <- colnames(newest_numbers_norway)[c(3, 5:30, 33:39, 41)]
  part_1 <- ifelse(
    input$picker_norway %in% cols[10:27],
    "Number of buildings of type",
    "Histogram of variable"
  )
  axis_title <- ifelse(
    input$picker_norway %in% cols[10:27],
    "Number of buildings",
    input$picker_norway
  )
  hchart(
    no_geometry_norway[, input$picker_norway],
    color = "#E8D7FF"
  ) %>%
    hc_title(
      text = paste(part_1, input$picker_norway),
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
    ]$date), "%d.%m"
  )
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