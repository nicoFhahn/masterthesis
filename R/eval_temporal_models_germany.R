library(readr)
library(INLA)
library(latex2exp)
ts_germany <- read_csv("wrangled_data/ts_germany.csv")
load("models/temporal_germany.Rda")
models <- models_final[[1]]
gof <- models_final[[2]]
mae <- models_final[[3]]
pred_tibble <- models_final[[4]]#
ggplot(data = pred_tibble[pred_tibble$model == 6, ]) +
  geom_ribbon(
    aes(ymin = q025, ymax = q975, x = Date), fill = "grey70"
  ) +
  geom_line(aes(x = Date, y = mean)) +
  geom_point(
    aes(x = Date, y = actual), alpha = 0.4,
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
    "Predicted number of infections vs. actual number of infections"
  )

ggplot(data = pred_tibble[pred_tibble$model == 6, ][487:500, ]) +
  geom_ribbon(
    aes(ymin = q025, ymax = q975, x = Date), fill = "grey70"
  ) +
  geom_line(aes(x = Date, y = mean)) +
  geom_point(
    aes(x = Date, y = actual), size = 3,
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

ggplot(data = pred_tibble[pred_tibble$model == 7, ]) +
  geom_ribbon(
    aes(ymin = q025, ymax = q975, x = Date), fill = "grey70"
  ) +
  geom_line(aes(x = Date, y = mean)) +
  geom_point(
    aes(x = Date, y = actual), alpha = 0.4,
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
    "Predicted number of infections vs. actual number of infections"
  )

ggplot(data = pred_tibble[pred_tibble$model == 7, ][487:500, ]) +
  geom_ribbon(
    aes(ymin = q025, ymax = q975, x = Date), fill = "grey70"
  ) +
  geom_line(aes(x = Date, y = mean)) +
  geom_point(
    aes(x = Date, y = actual), size = 3,
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

models[[7]]$summary.fixed
# get the summary of the bym2 model
models[[7]]$summary.fixed[
  order(models[[7]]$summary.fixed$mean), 1:2
]
# get the exponentiated coefficients
sapply(
  models[[7]]$marginals.fixed[
    rownames(models[[7]]$summary.fixed[
      order(models[[7]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
# and the credibility intervals
sapply(
  models[[7]]$marginals.fixed[
    rownames(models[[7]]$summary.fixed[
      order(models[[7]]$summary.fixed$mean),
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

temporal_car <- lapply(
  models[[7]]$marginals.random$id_date_1,
  function(x) {
    marg <- inla.tmarginal(
      function(y) exp(y), x
    )
    inla.emarginal(mean, marg)
  }
)

ts_germany$temporal_car <- unlist(temporal_car)
ggplot(data = ts_germany) +
  geom_line(aes(x = Date, y = temporal_car)) +
  theme_minimal() +
  xlab(
    "Date"
  ) +
  ylab(
    "Infections"
  ) +
  ggtitle(
    "Posterior temporal trend for the number of infections"
  )
