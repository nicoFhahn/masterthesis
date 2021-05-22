library(readr)
library(INLA)
library(latex2exp)
ts_norway <- read_csv("wrangled_data/ts_norway.csv")
load("models/temporal_norway.Rda")
models <- models_final[[1]]
gof <- models_final[[2]]
mae <- models_final[[3]]
pred_tibble <- models_final[[4]]
gof
mae
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

ggplot(data = pred_tibble[pred_tibble$model == 6, ][458:471, ]) +
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
ggplot(data = pred_tibble[pred_tibble$model == 13, ]) +
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

ggplot(data = pred_tibble[pred_tibble$model == 13, ][458:471, ]) +
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
temporal_car_4 <- lapply(
  models[[6]]$marginals.random$id_date_1,
  function(x) {
    marg <- inla.tmarginal(
      function(y) exp(y), x
    )
    inla.emarginal(mean, marg)
  }
)
temporal_car_7 <- lapply(
  models[[13]]$marginals.random$id_date_1,
  function(x) {
    marg <- inla.tmarginal(
      function(y) exp(y), x
    )
    inla.emarginal(mean, marg)
  }
)
car_frame <- tibble(
  date = rep(ts_norway$Date, 2),
  car = c(unlist(temporal_car_4), unlist(temporal_car_7)),
  model = c(rep("Model 6", 471), rep("Model 13", 471))
)
ggplot(data = car_frame) +
  geom_line(aes(x = date, y = car, colour = model)) +
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
# get the summary of the bym2 model
models[[6]]$summary.fixed[
  order(models[[6]]$summary.fixed$mean), 1:2
]
# get the exponentiated coefficients
sapply(
  models[[6]]$marginals.fixed[
    rownames(models[[6]]$summary.fixed[
      order(models[[6]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
# and the credibility intervals
sapply(
  models[[6]]$marginals.fixed[
    rownames(models[[6]]$summary.fixed[
      order(models[[6]]$summary.fixed$mean),
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

models[[13]]$summary.fixed[
  order(models[[13]]$summary.fixed$mean), 1:2
]
# get the exponentiated coefficients
sapply(
  models[[13]]$marginals.fixed[
    rownames(models[[13]]$summary.fixed[
      order(models[[13]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
# and the credibility intervals
sapply(
  models[[13]]$marginals.fixed[
    rownames(models[[13]]$summary.fixed[
      order(models[[13]]$summary.fixed$mean),
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
