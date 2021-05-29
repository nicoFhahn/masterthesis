model_norway <- reactiveValues(
  results = list(
    model = NA,
    predictions = numeric(),
    performance_frame = NA,
    sign_frame = NA
  )
)

model_germany <- reactiveValues(
  results = list(
    model = NA,
    predictions = numeric(),
    performance_frame = NA,
    sign_frame = NA
  )
)

model_europe <- reactiveValues(
  results = list(
    model = NA,
    predictions = numeric(),
    performance_frame = NA,
    sign_frame = NA,
    predictions_tibble = NA,
    car = numeric(),
    ts_country = NA,
    actual_numbers = numeric()
  )
)