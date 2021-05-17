library(highcharter)
no_geometry_norway <- newest_numbers_norway
no_geometry_norway$geometry <- NULL
hchart(
  no_geometry_norway[, "residential"],
  color = "red"
) %>%
  hc_title(
    text = paste("Number of buildings of type", "residential"),
    style = list(
      color = "#fff"
    )
  ) %>%
  hc_legend(enabled = FALSE)

highchart() %>%
  hc_xAxis(
    categories = unique(
      norway_munc_conf_long[
        norway_munc_conf_long$kommune_name == "Norway",
      ]$date
    )
  ) %>%
  hc_add_series(
    data = norway_munc_conf_long[
      norway_munc_conf_long$kommune_name == "Norway",
    ]$value,
    type = "line",
    name = "Total number of infections"
  )

highchart() %>%
  hc_xAxis(
    categories = unique(
      norway_munc_conf_long[
        norway_munc_conf_long$kommune_name == "Norway",
      ]$date
    )
  ) %>%
  hc_add_series(
    data = norway_munc_conf_long[
      norway_munc_conf_long$kommune_name == "Norway",
    ]$value_daily[2:length(unique(norway_munc_conf_long$date))],
    type = "line",
    name = "Daily number of infections"
  )

highchart() %>%
  hc_xAxis(
    categories = unique(
      norway_munc_conf_long[
        norway_munc_conf_long$kommune_name == "Norway",
      ]$date
    )
  ) %>%
  hc_add_series(
    data = norway_munc_conf_long[
      norway_munc_conf_long$kommune_name == "Norway",
    ]$value_daily_100k[2:length(unique(norway_munc_conf_long$date))],
    type = "line",
    name = "Daily number of infections per 1000k"
  ) %>%
  hc_legend(
    verticalAlign = "top",
    align = "right",
    layout = "proximate"
  )

highchart() %>%
  hc_xAxis(
    categories = unique(
      norway_munc_conf_long[
        norway_munc_conf_long$kommune_name == "Norway",
      ]$date
    )
  ) %>%
  hc_add_series(
    data = norway_munc_conf_long[
      norway_munc_conf_long$kommune_name == "Norway",
    ]$incidence_seven[8:length(unique(norway_munc_conf_long$date))],
    type = "line",
    name = "Seven day incidence"
  )
