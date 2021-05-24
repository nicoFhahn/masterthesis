library(readr)
library(INLA)
library(latex2exp)
library(tibble)
library(ggplot2)
ts_germany <- read_csv("wrangled_data/ts_germany.csv")
load("models/temporal_germany.Rda")
models <- models_final[[1]]
gof <- models_final[[2]]
mae <- models_final[[3]]
pred_tibble <- models_final[[4]]
formulas <- models_final[[5]]
gof[c(1, 2, 10, 12)]
mae[c(14, 15, 23, 25)]
mae[c(1, 2, 10, 12)]
new_cases_seven <- c()
new_cases_predicted_seven <- c()
ts_germany$new_cases_predicted <- round(pred_tibble$mean[pred_tibble$model == 12])
for (i in seq_len(nrow(ts_germany))) {
  start <- max(i - 6, 1)
  new_cases_seven[i] <- sum(ts_germany$new_cases[seq(start, i)])
  new_cases_predicted_seven[i] <- sum(ts_germany$new_cases_predicted[seq(start, i)])
}
ts_germany$new_cases_seven <- new_cases_seven
ts_germany$new_cases_predicted_seven <- new_cases_predicted_seven
ts_germany$incidence_seven <- 100000 * ts_germany$new_cases_seven / ts_germany$population
ts_germany$incidence_predicted_seven <- 100000 * ts_germany$new_cases_predicted_seven / ts_germany$population
incidence_tibble <- tibble(
  incidence = c(ts_germany$incidence_seven, ts_germany$incidence_predicted_seven),
  Date = rep(ts_germany$Date, 2),
  Type = c(rep("Actual 7-Day incidence", nrow(ts_germany)), rep("Predicted 7-Day incidence", nrow(ts_germany)))
)
ggplot(data = incidence_tibble) +
  geom_line(aes(x = Date, y = incidence, colour = Type), size = 1) +
  theme_minimal() +
  xlab(
    "Date"
  ) +
  ylab(
    "7-Day incidence"
  ) +
  ggtitle(
    "Predicted 7-Day incidence vs. actual 7-Day incidence"
  ) +
  geom_vline(xintercept = ts_germany$Date[483]) +
  scale_colour_manual(
    values = c("#357DED", "#DF2935")
  )
ggplot(data = pred_tibble[pred_tibble$model == 12, ]) +
  geom_ribbon(
    aes(ymin = q025, ymax = q975, x = Date),
    fill = "grey70"
  ) +
  geom_line(aes(x = Date, y = mean)) +
  geom_point(
    aes(x = Date, y = actual),
    alpha = 0.4,
    colour = "#3E92CC"
  ) +
  geom_vline(xintercept = ts_germany$Date[483]) +
  theme_minimal() +
  xlab(
    "Date"
  ) +
  ylab(
    "Infections"
  ) +
  ggtitle(
    "Predicted number of infections vs. actual number of infections"
  )

ggplot(data = pred_tibble[pred_tibble$model == 12, ][483:nrow(ts_germany), ]) +
  geom_ribbon(
    aes(ymin = q025, ymax = q975, x = Date),
    fill = "grey70"
  ) +
  geom_line(aes(x = Date, y = mean)) +
  geom_point(
    aes(x = Date, y = actual),
    size = 3,
    colour = "#3E92CC"
  ) +
  theme_minimal() +
  xlab(
    "Date"
  ) +
  ylab(
    "Infections"
  ) +
  ggtitle(
    "Predicted number of infections",
    subtitle = "Test data"
  )
temporal_cars <- lapply(
  c(2, 3, 4, 5, 6, 8, 9, 10, 12, 13),
  function(x, ...) {
    lapply(
      models[[x]]$marginals.random$id_date_1,
      function(y) {
        marg <- inla.tmarginal(
          function(z) exp(z), y
        )
        inla.emarginal(mean, marg)
      }
    )
  }
)
ts_germany$temporal_car_2 <- unlist(temporal_cars[[1]])
ts_germany$temporal_car_3 <- unlist(temporal_cars[[2]])
ts_germany$temporal_car_4 <- unlist(temporal_cars[[3]])
ts_germany$temporal_car_5 <- unlist(temporal_cars[[4]])
ts_germany$temporal_car_6 <- unlist(temporal_cars[[5]])
ts_germany$temporal_car_8 <- unlist(temporal_cars[[6]])
ts_germany$temporal_car_9 <- unlist(temporal_cars[[7]])
ts_germany$temporal_car_10 <- unlist(temporal_cars[[8]])
ts_germany$temporal_car_12 <- unlist(temporal_cars[[9]])
ts_germany$temporal_car_13 <- unlist(temporal_cars[[10]])
ggplot(data = ts_germany) +
  geom_line(aes(x = Date, y = temporal_car_12)) +
  theme_minimal() +
  xlab(
    "Date"
  ) +
  ylab(
    TeX("$\\exp\\left(\\phi_t\\right)$")
  ) +
  ggtitle(
    "Posterior temporal trend for the number of infections"
  )

models[[12]]$summary.fixed
# get the summary of the bym2 model
round(models[[12]]$summary.fixed[
  order(models[[12]]$summary.fixed$mean), 1:2
], 3)
# get the exponentiated coefficients
round(sapply(
  models[[12]]$marginals.fixed[
    rownames(models[[12]]$summary.fixed[
      order(models[[12]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
), 3)
round(sapply(
  models[[12]]$marginals.fixed[
    rownames(models[[12]]$summary.fixed[
      order(models[[12]]$summary.fixed$mean),
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
), 3)
